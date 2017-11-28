#!/usr/bin/python

from mininet.topo import Topo
from mininet.net import Mininet
from mininet.node import Node, RemoteController, OVSKernelSwitch
from mininet.log import setLogLevel, info, debug
from mininet.cli import CLI
import yaml
import requests
import time, sys

def configure_local_vtep(router_id):
    info("Router id %s\n" % (router_id))

    # Setting router id
    payload = dict(
        dpid="1",
        as_number=65000,
        router_id=router_id
    )
    response = requests.post(
        url="http://localhost:8080/vtep/speakers",
        json=payload
    )
    debug(response.text)

def configure_remote_vtep(neighbor_id, as_number):
    info("Neighbor Id %s as_number %s\n" % (neighbor_id, as_number))

    # Setting neighbor id
    payload = dict(
        address=neighbor_id,
        remote_as=as_number
    )
    response = requests.post(
        url="http://localhost:8080/vtep/neighbors",
        json=payload
    )
    debug(response.text)

def configure_endport(port, vni, ip, mac):
    info("Adding endpoint: Port %s VNI %s, IP %s, MAC %s\n" % (port, vni, ip, mac))

    payload = dict(
        vni=vni
    )
    response = requests.post(
        url="http://localhost:8080/vtep/networks",
        json=payload
    )
    debug(response.text)

    payload = dict(
        port=port,
        mac=mac,
        ip=ip
    )
    response = requests.post(
        url="http://localhost:8080/vtep/networks/" + `vni` + "/clients",
        json=payload
    )
    debug(response.text)

class LinuxRouter( Node ):
    "A Node with IP forwarding enabled."

    def add_vlan( self, intf, ip, vlan = None):
        if vlan is not None:
            self.cmd( 'vconfig add %s %d' % ( intf, vlan ) )
            info( "*** Adding vlan interface %s.%s\n" % (intf, vlan) )
            intf = self.intf(intf)
            newName = '%s.%d' % ( intf, vlan )
            # update the (Mininet) interface to refer to VLAN interface name
            intf.name = newName
            # add VLAN interface to host's name to intf map
            self.nameToIntf[ newName ] = intf
        else:
            newName = intf

        self.setIP(ip, 24, newName)

    def config( self, config, **params ):
        info( '*** Configuring LinuxRouter: \n' )
        super( LinuxRouter, self).config( **params )
        # Enable forwarding on the router
        self.cmd( 'sysctl net.ipv4.ip_forward=1' )

        for intf,attrs in sorted(config.items()):
          self.setIP( attrs['ip'], 24, intf )
          self.setMAC( attrs['mac'], intf )

    def terminate( self ):
        self.cmd( 'sysctl net.ipv4.ip_forward=0' )
        super( LinuxRouter, self ).terminate()

class NetworkTopo( Topo ):
    "A LinuxRouter connecting three IP subnets"

    def yaml_config( self, ConfigFile ):
        with open(ConfigFile, 'r') as stream:
          try:
            self.config = (yaml.load(stream))
          except yaml.YAMLError as exc:
            print(exc)

        s1 = self.addSwitch( 's1', cls=OVSKernelSwitch, dpid='0000000000000001')

        for key, value in self.config.items():
          if key == 'vtep':
            self.vtep = value
            continue

        # Remove vtep configuration from config data
        self.config.pop('vtep')

        for routerName, Intfs in self.config.items():
          router = self.addNode( routerName, cls=LinuxRouter, ip=None, config=Intfs )
          for intf,attrs in sorted(Intfs.items()):
            self.addLink( s1, router, intfName2=intf )

    def ryu( self, net ):
      configure_local_vtep(router_id = self.vtep['local'])

      neighbor_ip,as_number = self.vtep['remote'].split(":")
      configure_remote_vtep(neighbor_ip, as_number)

      s1 = net.get('s1')
      for routerName, Intfs in self.config.items():
        for intf,attrs in sorted(Intfs.items()):
          ovs_if = "s1-%s" % intf
          cmd = ("ovs-ofctl add-flow s1"
                 "\"table=1,priority=1,metadata=%s,dl_dst=ff:ff:ff:ff:ff:ff actions=output:%d\"" %
                 (attrs['vni'], (s1.ports[s1.intf(ovs_if)])))
          s1.cmd(cmd)
          configure_endport(
            port=ovs_if,
            vni=attrs['vni'],
            ip=attrs['ip'],
            mac=attrs['mac']
        )

    def build( self, yaml=None, **opts ):
        if yaml != None:
          self.yaml_config(yaml)
          return

        router = self.addNode( 'r0', cls=LinuxRouter, ip=None )
        s1 = self.addSwitch( 's1', cls=OVSKernelSwitch, dpid='0000000000000001')

        self.addLink( s1, router, intfName2='r0-eth1', addr2='02:00:0a:00:01:fe' )
        self.addLink( s1, router, intfName2='r0-eth2', addr2='02:00:0a:00:02:fe' )
        self.addLink( s1, router, intfName2='r0-eth3', addr2='00:00:14:00:01:fe' )
        self.addLink( s1, router, intfName2='r0-eth4', addr2='00:00:14:00:02:fe' )
        self.addLink( s1, router, intfName2='r0-eth5', addr2='00:00:1E:00:01:fe' )

def run():
    "Test linux router"
    topo = NetworkTopo()
    net = Mininet( topo=topo, controller=lambda name: RemoteController( name, ip='127.0.0.1' ))  # controller is used by s1-s3
    net.start()
    s1 = net.get('s1')
    s1.cmd("ovs-vsctl show")
    info("s1 = %s" % s1.nameToIntf)

    info( '*** Routing Table on Router:\n' )
    info( net[ 'r0' ].cmd( 'route' ) )
    CLI( net )
    net.stop()

def main(argv):
    info("Starting topology from %s\n" % argv[0])

    topo = NetworkTopo(yaml=argv[0])

    net = Mininet( topo=topo, controller=lambda name: RemoteController( name, ip='127.0.0.1' ))
    net.start()

    s1 = net.get('s1')
    s1.cmd("ovs-vsctl set-manager ptcp:6640")

    info("ryu-manager should have started using command\n%s\n" %
        "sudo ryu-manager ryu.app.rest_vtep ryu.app.ofctl_rest")

    time.sleep(2)  # Give some time to have DPID discovered

    topo.ryu(net)

    CLI( net )
    net.stop()

if __name__ == '__main__':
    setLogLevel( 'info' )
    main(sys.argv[1:])
