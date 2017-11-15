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
#ifndef __GoBgpClient_H__
#define __GoBgpClient_H__
#include <iostream>
#include <memory>
#include <sstream>
#include <string.h>
#include <string>
#include <functional>


#include "gobgp.grpc.pb.h"
#include <grpc/grpc.h>
#include <grpc++/channel.h>

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;

using gobgpapi::GobgpApi;

struct path_t;

class GoBgpClient {
  public:
    static GoBgpClient* Self();
    GoBgpClient(std::shared_ptr<Channel> channel);

    GoBgpClient(const std::string libfile, const std::string server, int port) {}

    void Initialize(
        std::string libfile = "/go/src/github.com/osrg/gobgp/gobgp/lib/libgobgp.so",
        std::string server = "localhost",
        int port = 50051
        );

    std::string GetNeighbor();

    std::string GetAllActiveAnnounces(unsigned int route_family);

    enum RouteOp {
      Add = 1,
      Delete = 2
    };
    std::string RouteAnnounce(RouteOp op,
        const std::string& route_family, const std::string& route_descr,
        std::string *serialize = NULL);

    // Functions imported from libgobgp.so file
    std::function<struct path_t*(int, char*)> serialize_path;
    std::function<char*(struct path_t*)> decode_path;
    std::function<int(const char*)> get_route_family;

  private:

    std::unique_ptr<GobgpApi::Stub> stub_;
    static GoBgpClient    *self_;
    void *handle_;
};
#endif
