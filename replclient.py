#!/usr/bin/python

import asyncore
import logging
import collections
import re

# I know I know I know
def qwik_encode(s):
    return ' '.join([str(ord(c)) for c in s])

def qwik_decode(s):
    return ''.join([chr(int(w)) for w in re.match('(.*?)<EOF>',s).group(1).split()])

class ReplClient(asyncore.dispatcher):

    def __init__(self, host, port, message, chunk_size=1024):
        self.to_send = collections.deque()
        self.add_message(message)
        self.received_data = []
        self.chunk_size = chunk_size
        self.logger = logging.getLogger('EchoClient')
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.logger.debug('connecting to %s', (host, port))
        self.connect((host, port))
        return

    def handle_connect(self):
        self.logger.debug('handle_connect()')
    
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
        self.logger.debug('writable() -> %s', bool(self.to_send))
        return bool(self.to_send)

    def handle_write(self):
        # sent = self.send(self.to_send[:self.chunk_size])
        msg = self.to_send.pop()
        sendable_msg = qwik_encode(msg[:self.chunk_size])
        rest_msg = msg[self.chunk_size:]
        if not bool(rest_msg):
            sendable_msg += "<EOF>"
        sent = self.send(sendable_msg)
        self.logger.debug('handle_write() -> (%d) "%s"', sent, sendable_msg)
        if bool(rest_msg):
            self.to_send.appendleft(rest_msg)

    def handle_read(self):
        data = self.recv(self.chunk_size)
        self.logger.debug('handle_read() -> (%d) "%s"', len(data), data)
        self.logger.debug('decodes to -> %s', qwik_decode(data))
        self.received_data.append(data)
        # here's the input part:
        # self.to_send.append(raw_input('--> ') + "<EOF>")
        incoming = raw_input('--> ')
        ReplClient(tcp_ip, tcp_port, message=incoming, chunk_size=self.chunk_size)
        
    def add_message(self, msg):
        self.message = msg
        self.to_send.append(msg)


if __name__ == '__main__':
    import socket  # note that this is where we import socket

    tcp_ip = '192.168.1.6'
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

    some_input = raw_input('--> ')
    client = ReplClient(tcp_ip, tcp_port, message=some_input, chunk_size=buffer_size)
    #client = EchoClient(ip, port, message=some_input)

    asyncore.loop()
