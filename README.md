Desert-Pocket-Gopher
====================

Server for the Gopher protocol written for mirage.
- server.ml: server
- dsl.ml: DSL compiler for making Gopher sites
- site.ml: example site
- resources/: files for use on the site 


to build and run:

    mirage configure --unix
    make
    sudo ./mir-example
    sudo ifconfig tap0 10.0.0.1 netmask 255.255.255.0
    
and open in a gopher client of your choice:

    lynx gopher://10.0.0.2

![example site in lynx](http://i.imgur.com/s3MuzGl.png)
