#!/bin/bash

ip route add 10.0.100.0/24 via 10.0.201.1
ip route add 10.0.202.0/24 via 10.0.201.1

./router-topo.py ryu1.yml
