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
        self.host = host_
        self.port = port_
        self.to_send = collections.deque()
        self.incoming = ""
        self.add_message(message)
        self.received_data = []
        self.chunk_size = chunk_size
        self.logger = logging.getLogger('ReplClient')
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.logger.debug('connecting to %s', (host_, port_))
        self.connect((host_, port_))
        return

    def handle_connect(self):
        # self.logger.debug('handle_connect()')
        return
    
    def handle_close(self):
        self.logger.debug('handle_close()')
        self.close()
        # received_message = ''.join(self.received_data)
        # if received_message == self.message:
        #     self.logger.debug('RECEIVED COPY OF MESSAGE')
        # else:
        #     self.logger.debug('ERROR IN TRANSMISSION')
        #     self.logger.debug('EXPECTED "%s"', self.message)
        #     self.logger.debug('RECEIVED "%s"', received_message)
        return
    
    def writable(self):
        # self.logger.debug('writable() -> %s', bool(self.to_send))
        return bool(self.to_send)

    def handle_write(self):
        # sent = self.send(self.to_send[:self.chunk_size])
        msg = self.to_send.pop()
        sendable_msg = qwik_encode(msg[:self.chunk_size])
        rest_msg = msg[self.chunk_size:]
        if not bool(rest_msg):
            sendable_msg += "<EOF>"
        sent = self.send(sendable_msg)
        # self.logger.debug('handle_write() -> (%d) "%s"', sent, sendable_msg)
        if bool(rest_msg):
            self.to_send.appendleft(rest_msg)

    def handle_read(self):
        data = self.recv(self.chunk_size)
        # self.logger.debug('handle_read() -> (%d) "%s"', len(data), data)
        self.incoming += data # don't think we have to ascii decode first level
        if(has_eof(self.incoming)):
            # self.logger.debug('decodes to -> %s', qwik_decode(self.incoming))
            print qwik_decode(self.incoming)
            # self.received_data.append(data)
            # self.logger.debug('closing')
            self.close()
            # self.logger.debug('close complete')
            # self.logger.debug('reading input')
            # more_input = input_prompt('--> ')
            # self.logger.debug('input received -> (%d) %s',
            #                   len(more_input),
            #                   more_input)
            # ReplClient(self.host, self.port, message=more_input,
            #            chunk_size=self.chunk_size)
            return
        else:
            self.logger.debug('awaiting more data')
            return
        
    def add_message(self, msg):
        self.message = msg
        self.to_send.append(msg)



if __name__ == '__main__':
    import socket  # note that this is where we import socket

    #print(sys.argv)
    #print(sys.argv[1])
    #tcp_ip = '192.168.1.6'
    #if 1 < len(sys.argv[1]):
    #    tcp_ip = sys.argv[1][2]
    tcp_ip = '127.0.0.1'
    tcp_port = 11000
    buffer_size = 1024

    logging.basicConfig(level=logging.DEBUG,
                        format='%(name)s: %(message)s',
                        )

    #address = ('localhost', 0) # let the kernel give us a port
    address = (tcp_ip, tcp_port)

    # server = EchoServer(address)

    # ip, port = server.address # find out what port we were given

    #client = EchoClient(ip, port, message=open('lorem.txt', 'r').read())
    while(True):
        some_input = raw_input('--> ')
        # print('received input:{}'.format(some_input))
        client = ReplClient(tcp_ip, tcp_port, message=some_input, chunk_size=buffer_size)
        asyncore.loop()
    #client = EchoClient(ip, port, message=some_input)
    
