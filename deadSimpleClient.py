#!/usr/bin/python

import socket


#TCP_IP = '192.168.1.8'
TCP_IP = '192.168.1.6'
TCP_PORT = 11000
BUFFER_SIZE = 1024
#MESSAGE = "Hello, World!<EOF>"
MESSAGE = "Gunpowder is classified as a low explosive because of its relatively slow decomposition rate and consequently low brisance. Low explosives deflagrate (i.e., burn) at subsonic speeds, whereas high explosives detonate, producing a supersonic wave. Gunpowder's burning rate increases with pressure, so it will burst containers if contained but otherwise just burns in the open. Ignition of the powder packed behind a bullet must generate enough pressure to force it from the muzzle at high speed, but not enough to rupture the gun barrel. Gunpowder thus makes a good propellant, but is less suitable for shattering rock or fortifications. Gunpowder was widely used to fill artillery shells and in mining and civil engineering to blast rock roughly until the second half of the 19th century, when the first high explosives (nitro-explosives) were discovered. Gunpowder is no longer used in modern explosive military warheads, nor is it used as main explosive in mining operations due to its cost relative to that of newer alternatives such as ammonium nitrate/fuel oil (ANFO).[8] Black powder is still used as a delay element in various munitions where its slow-burning properties are valuable.<EOF>"

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((TCP_IP, TCP_PORT))
s.send(MESSAGE)
#s.close()
data = s.recv(BUFFER_SIZE)


print "received data:", data
