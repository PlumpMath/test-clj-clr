#!/usr/bin/python

import socket


#TCP_IP = '192.168.1.8'
TCP_IP = '192.168.1.6'
TCP_PORT = 11000
BUFFER_SIZE = 1024
MESSAGE = "Hello, World!<EOF>"

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((TCP_IP, TCP_PORT))
s.send(MESSAGE)
#s.close()
data = s.recv(BUFFER_SIZE)


print "received data:", data
