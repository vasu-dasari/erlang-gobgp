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
#include <string>
#include "gobgp_client.h"

int main(int argc, char** argv)
{
  std::cout << GoBgpClient::Self()->GetNeighbor();

  std::cout << "Route family for l2vpn-evpn is " <<
    GoBgpClient::Self()->get_route_family("l2vpn-evpn")
    << std::endl;
  std::cout << "Adding route to 20.10.20.33/32: " << 
    GoBgpClient::Self()->RouteAnnounce(GoBgpClient::RouteOp::Add, "ipv4-unicast",
        "20.10.20.33/32 nexthop 10.10.1.99")
    << std::endl;

  std::cout << "Deleting route to 20.10.20.33/32: " << 
    GoBgpClient::Self()->RouteAnnounce(GoBgpClient::RouteOp::Delete, "ipv4-unicast",
        "20.10.20.33/32 nexthop 10.10.1.99")
    << std::endl;

  std::cout << "Adding route to 30.10.20.33/32: " << 
    GoBgpClient::Self()->RouteAnnounce(GoBgpClient::RouteOp::Add, "ipv4-unicast",
        "30.10.20.33/32 nexthop 10.10.1.99")
    << std::endl;

  std::cout << "Adding evpn route to 30.10.20.33/32: " << 
    GoBgpClient::Self()->RouteAnnounce(GoBgpClient::RouteOp::Add, "l2vpn-evpn",
        "macadv aa:bb:cc:dd:ee:04 2.2.2.4 1 1 rd 64512:10 rt 64512:10 encap vxlan")
    << std::endl;
}
