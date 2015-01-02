Desert-Pocket-Gopher
====================

Desert Pocket Gopher


to test:

mirage configure --unix
make
sudo ./mir-example

sudo ifconfig tap0 10.0.0.1 netmask 255.255.255.0
lynx gopher://10.0.0.2
