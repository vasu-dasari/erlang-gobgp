#!/bin/bash

sysctl net.ipv4.ip_forward=1
ip route add 10.0.202.0/24 via 10.0.100.2
