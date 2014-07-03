#!/usr/bin/python

import asyncore
import logging
import collections
import re
import sys

# I know I know I know
def qwik_encode(s):
    return ' '.join([str(ord(c)) for c in s])

def qwik_decode(s):
    return ''.join([chr(int(w)) for w in re.match('(.*?)<EOF>',s).group(1).split()])

def has_eof(s):
    return "<EOF>" in s

def input_prompt(s):
    return raw_input(s)
    
class ReplClient(asyncore.dispatcher):

    def __init__(self, host_, port_, message, chunk_size=1024):
        self.logger = logging.getLogger('ReplClient')
        self.host = host_
        self.port = port_
        self.to_send = collections.deque()
        self.incoming = ""
        self.add_message(message)
        self.received_data = []
        self.chunk_size = chunk_size
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.logger.debug('connecting to %s', (host_, port_))
        self.connect((host_, port_))
        return

    def handle_connect(self): # this is stupid. delete, in the infinite future
        return
    
    def handle_close(self):
        self.logger.debug('handle_close()')
        self.close()
        return
    
    def writable(self):
        return bool(self.to_send)

    def handle_write(self):
        msg = self.to_send.pop()
        sendable_msg = qwik_encode(msg[:self.chunk_size])
        rest_msg = msg[self.chunk_size:]
        if not bool(rest_msg):
            sendable_msg += "<EOF>"
        sent = self.send(sendable_msg)
        if bool(rest_msg):
            self.to_send.appendleft(rest_msg)

    def handle_read(self):
        data = self.recv(self.chunk_size)
        self.incoming += data # don't think we have to ascii decode first level
        if(has_eof(self.incoming)):
            print qwik_decode(self.incoming)
            self.close()
            return
        else:
            self.logger.debug('awaiting more data')
            return
        
    def add_message(self, msg):
        self.message = msg
        self.to_send.append(msg)


if __name__ == '__main__':
    import socket 

    tcp_ip = '127.0.0.1' # or whatever
    tcp_port = 11000
    buffer_size = 1024

    logging.basicConfig(level=logging.DEBUG,
                        format='%(name)s: %(message)s',
                        )

    address = (tcp_ip, tcp_port)

    while(True):
        some_input = raw_input('--> ')
        client = ReplClient(tcp_ip, tcp_port, message=some_input, chunk_size=buffer_size)
        asyncore.loop()
