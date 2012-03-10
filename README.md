# Quickstart

Setting up Conserv is painless using [Quicklisp](http://quicklisp.org). If the library isn't
available on Quicklisp yet, you can get it by doing
`$ git clone git://github.com/zkat/conserv.git` before trying to quickload it.

```lisp
CL-USER> (ql:quickload 'conserv)
         ...
         ...
         ...
CL-USER> (defclass echo () ())
#<STANDARD-CLASS ECHO>
CL-USER> (defmethod conserv.tcp:on-socket-data ((driver echo) data)
           (write-sequence data conserv.tcp:*socket*))
#<STANDARD-METHOD CONSERV:ON-SOCKET-DATA (ECHO T)>
CL-USER> (conserv:with-event-loop ()
           (conserv.tcp:server-listen (make-instance 'echo)
                                      :host "127.0.0.1"
                                      :port 1337))
```

Now in a terminal...

    $ netcat localhost 1337
    hello world!
    hello world!

# Introduction

Conserv is an event-based networking library inspired by the networking modules of
[Node.js](http://nodejs.org). It aims to provide a simple, easy-to-use API for implementing and
using network protocols. Conserv currently includes a TCP client and server API and an HTTP/1.1
server with CONNECT/Upgrade support (allowing implementation of proxies and websocket servers that
share a port with a regular web application, for example).

# Events

Events are the bread and butter of writing applications using Conserv. These take the form of
generic functions with default behavior. All event functions accept one or more DRIVER arguments,
which are used for dispatching these events. Once a class is defined and the desired events are
implemented for it, you simply provide the Conserv server/client with an instance of the driver, and
it'll take care of the rest.

# Packages

Conserv splits its various supported protocols between several packages. Documentation for
individual packages can be found under docs/<package-name>.md

  * conserv - Includes the basic event library. with-event-loop, add-timer, exit-event-loop, and so on.
  * conserv.tcp - TCP client/server
  * conserv.http - HTTP server

# License

Conserv is free software, licensed under the MIT license. See the included COPYING file for
licensing details.
