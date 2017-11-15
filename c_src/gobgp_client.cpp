/* Copyright (c) 2010-2018, Vasu Dasari <vdasari@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <iostream>
#include <memory>
#include <sstream>
#include <string.h>
#include <string>

#include <grpc++/channel.h>
#include <grpc++/client_context.h>
#include <grpc++/create_channel.h>
#include <grpc++/security/credentials.h>
#include <grpc/grpc.h>

#include <dlfcn.h>

extern "C" {
// Gobgp library
#include "libgobgp.h"
}

#include "gobgp_client.h"

GoBgpClient::GoBgpClient(std::shared_ptr<Channel> channel)
    : stub_(GobgpApi::NewStub(channel))
{
}

GoBgpClient* GoBgpClient::self_    = NULL;
GoBgpClient *GoBgpClient::Self()
{
  if (self_) {
    return self_;
  }

  self_ = new GoBgpClient(
      grpc::CreateChannel("localhost:50051", grpc::InsecureChannelCredentials()));

  self_->Initialize();

  return self_;
}

void GoBgpClient::Initialize(
    std::string libfile,
    std::string server,
    int port
    )
{
  // if (handle_ = dlopen(libfile.c_str(), RTLD_NOW) == NULL) {
  handle_ = dlopen(libfile.c_str(), RTLD_NOW);
  if (handle_ == NULL) {
    printf("Could not load gobgp binary library\n");
    exit(1);
  } 

  dlerror();    /* Clear any existing error */

  serialize_path = (path*(*)(int, char*))dlsym(handle_, "serialize_path");
  decode_path = (char*(*)(path*))dlsym(handle_, "decode_path");
  get_route_family = (int(*)(const char*))dlsym(handle_, "get_route_family");
  if (
      (serialize_path == NULL) ||
      (decode_path == NULL) ||
      (get_route_family == NULL)
     ) {
    printf("Could not load required functions from the dynamic library\n");
    exit(1);
  }
}

std::string GoBgpClient::GetNeighbor()
{
  ClientContext context;
  gobgpapi::GetNeighborRequest request;
  gobgpapi::GetNeighborResponse response;
  grpc::Status status = stub_->GetNeighbor(&context, request, &response);

  if (status.ok()) {
    std::stringstream buffer;
    for (int i=0; i < response.peers_size(); i++) {

      gobgpapi::PeerConf peer_conf = response.peers(i).conf();
      gobgpapi::PeerState peer_info = response.peers(i).info();
      gobgpapi::Timers peer_timers = response.peers(i).timers();

      buffer
        << "BGP neighbor is: " << peer_conf.neighbor_address()
        << ", remote AS: " << peer_conf.peer_as() << "\n"
        << "\tBGP version: 4, remote route ID " << peer_conf.id() << "\n"
        << "\tBGP state = " << peer_info.bgp_state()
        << ", up for " << peer_timers.state().uptime() << "\n"
        << "\tBGP OutQ = " << peer_info.out_q()
        << ", Flops = " << peer_info.flops() << "\n"
        << "\tHold time is " << peer_timers.state().hold_time()
        << ", keepalive interval is " << peer_timers.state().keepalive_interval() << "seconds\n"
        << "\tConfigured hold time is " << peer_timers.config().hold_time() << "\n";

    }
    return buffer.str();
  } else {
    std::stringstream buffer;
    buffer
      << status.error_code() << "\n"
      << status.error_message() << "\n"
      << status.error_details() << "\n";
    return buffer.str();
  }
}

std::string GoBgpClient::RouteAnnounce(RouteOp Op,
    const std::string& route_family, const std::string& route_descr_const,
    std::string *serialize_buf)
{
  std::string route_descr(route_descr_const);
	path* path_c_struct = serialize_path(
			get_route_family(route_family.c_str()), (char*)route_descr.c_str());

  if (path_c_struct == NULL) {
    std::cerr << "Could not generate path\n";
    exit(-1);
  }

#if 0
  printf("Decoded NLRI output: %s, length %d raw string length: %ld\n",
			decode_path(path_c_struct), path_c_struct->nlri.len, strlen(path_c_struct->nlri.value));
#endif

  gobgpapi::Path* current_path = new gobgpapi::Path;

  for (int path_attribute_number = 0;
			path_attribute_number < path_c_struct->path_attributes_len;
			path_attribute_number++) {
		current_path->add_pattrs(path_c_struct->path_attributes[path_attribute_number]->value,
        path_c_struct->path_attributes[path_attribute_number]->len);
  }

  current_path->set_nlri(path_c_struct->nlri.value, path_c_struct->nlri.len);
  current_path->set_family(get_route_family("ipv4-unicast"));

  if (serialize_buf) {
    if (Op == RouteOp::Add) {
      gobgpapi::AddPathRequest request;
      gobgpapi::AddPathResponse response;
      request.set_allocated_path(current_path);

      request.SerializeToString(serialize_buf);
    } else {
      gobgpapi::DeletePathRequest request;
      gobgpapi::DeletePathResponse response;
      request.set_allocated_path(current_path);

      request.SerializeToString(serialize_buf);
    }
    return std::string("ok");

  } else {
    ClientContext context;
    grpc::Status status;

    if (Op == RouteOp::Add) {
      gobgpapi::AddPathRequest request;
      gobgpapi::AddPathResponse response;
      request.set_allocated_path(current_path);

      if(serialize_buf) {
        request.SerializeToString(serialize_buf);
      } else {
        status = stub_->AddPath(&context, request, &response);
      }
    } else {
      gobgpapi::DeletePathRequest request;
      gobgpapi::DeletePathResponse response;
      request.set_allocated_path(current_path);

      status = stub_->DeletePath(&context, request, &response);
    }

    if (status.ok()) {
      return std::string("ok");
    } else {
      std::stringstream buffer;
      buffer
        << status.error_code() << "\n"
        << status.error_message() << "\n"
        << status.error_details() << "\n";
      std::cout <<  buffer.str();
      return buffer.str();
    }
  }
}
  
