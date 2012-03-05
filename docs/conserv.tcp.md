# TCP

The conserv.tcp module contains a socket implementation for writing both TCP clients and TCP
servers.

## Sockets

TCP clients are called 'sockets'. Sockets are both binary and character output streams. This means
that, depending on the `socket-external-format-out` setting on the socket, you can write to them
using `write-sequence/string/byte/char`, and higher-level stream functions such as `format`. Conserv
queues output when these are used, so no events need to be handled to be able to asynchronously
output.

Receiving data from sockets is a different story. Data is received asynchronously, and users must
implement `on-socket-data` in order to receive data when it comes in. The data will be encoded
according to the `socket-external-format-out` settings on the socket before being passed to the
event.

Sockets can connect to internet hosts as well as local unix sockets (aka IPC sockets). Connections
are established using the `socket-connect` function. Sockets are also created by TCP servers
whenever a connection is accepted.

### Events

*[variable]* `*socket*`

  During execution of socket events, this is bound to the `socket` object associated with the current
  event. This variable is unbound outside the scope of `socket-event-driver` events.

*[generic function]* `on-socket-error driver error`

  Event called when `*socket*` has experienced some error. `error` is the actual
  condition. This event is executed immediately before the client is shut down.
  By default, this event simply drops the client connection.

  The fact that `on-socket-error` receives the actual condition allows a sort of
  condition handling by specializing both the driver and the condition. For
  example:

```lisp
(defmethod on-socket-error ((driver my-driver) (error something-harmless))
  (format t "~&Nothing to see here.~%"))

(defmethod on-socket-error ((driver my-driver) (error blood-and-guts))
  (format t "~&Oh, the humanity!~%")
  (drop-connection error))
```
*[restart function]* `drop-connection &optional condition`

  Drops the current connection and allows execution to continue. Can only be called within the
  scope of `on-socket-error`.

*[generic function]* `on-socket-end-of-file driver`

  Event called when an EOF has been received while reading from `*socket*`

*[generic function]* `on-socket-connect driver`

  Event called immediately after a successful `socket-connect`.

*[generic function]* `on-socket-data driver data`

  Event called when `*socket*` has received some new `data`. If
  `(socket-external-format-in *socket*)` is `nil`, `data` will by an array of
  `(unsigned-byte 8)`. Otherwise, it will be used to encode the incoming data before passing it to
  `on-socket-data`.

*[generic function]* `on-socket-close driver`

  Event called when `*socket*` has been disconnected.

*[generic function]* `on-socket-output-empty driver`

  Event called when `*socket*`'s output queue is empty.

### Socket objects

*[function]* `socket-connect driver host &key port external-format-in external-format-out`

  Establishes a TCP connection with `host`. If successful, returns a `socket` object.

  * `driver` -- an instance of a driver class that will be used to dispatch `socket-event-driver`
    events.
  * `host` -- Either a string representing a remote IP address or hostname, or a pathname to a local
    IPC socket.
  * `port` -- Port number to connect to. Should not be provided if `host` is an IPC socket.
  * `external-format-in` -- Either an external format or `nil`. Used for determining the encoding of
    data passed to `on-socket-data`
  * `external-format-out` -- Either an external format or `nil`. Used for converting strings written
    to the socket. If `nil`, an error will be signaled if an attempt is made to write a string to
    the socket.

*[accessor]* `socket-driver socket`

  Driver object used to dispatch `socket`'s events.

*[accessor]* `socket-server socket`

  Holds the associated server object if this socket was accepted by a server.

*[accessor]* `socket-external-format-in socket`

  External format to use when converting incoming octets into characters. If `nil`,
  no encoding will be done on incoming data, and `on-socket-data` will receive the
  raw `(unsigned-byte 8)` data.

*[accessor]* `socket-external-format-out socket`

  External format to use for outgoing octets and strings. If `nil`, an error is
  signaled if an attempt is made to write a string to `socket`.

*[function]* `socket-bytes-read socket`

  Total bytes successfully read by `socket` so far.

*[function]* `socket-bytes-writter socket`

  Total bytes successfully written to `socket`. Note that this is incremented only
  after the data has been written to the file descriptor, not after each call to a
  stream writing function.

*[function]* `socket-local-p socket`

  Returns true if `socket` is connected to a local IPC socket, `nil` otherwise.

*[function]* `socket-remote-name socket`

  Name of the remote host `socket` is connected to.

*[function]* `socket-remote-port socket`

  Remote port that `socket` is connected to.

*[function]* `socket-local-name socket`

  Local host name for `socket`'s connection.

*[function]* `socket-local-port socket`

  Local port for `socket`'s connection.

*[function]* `socket-paused-p socket`

  True when `socket` is paused, i.e. it is not currently waiting for read events.

*[function]* `socket-pause socket &key timeout`

  Pauses read events for `socket`. While a socket is paused, it will continue to send data as it is
  queued, but it will not listen for and handle read events (meaning, on-socket-data will not be
  called). This can be useful for throttling clients. If `timeout` is provided, it is interpreted as
  the number of seconds to wait before resuming reads on the socket. Has no effect if `socket`
  is already paused, although the resume timeout will still be activated.

*[function]* `socket-resume socket`

  Resumes reads on a paused `socket`. If `socket` was not already paused, this function has no
  effect.

## Servers

TCP servers, or just 'servers', can listen for incoming connections. They support both internet
connections and local/unix socket/IPC connections. Servers are started by calling
`server-listen`. Once activated, servers will create `socket` objects whenever a client connects to
the host and port the server is listening on.

By default, new `socket`s are assigned the `server`'s `server-driver` as their `socket-driver`,
although this can safely be changed by `setf`ing `socket-driver` inside the `server`'s
`on-server-connection` event.

### Events

*[variable]* `*server*`

  During execution of `server-event-driver` events, this variable is bound to the associated
  `server` object. This variable is unbound outside of the scope of `server-event-driver` events.

*[generic function]* `on-server-listen driver`

  Event called when `*server*` has started listening.

*[generic function]* `on-server-connection driver client`

  Event called when a new `client` (a `socket`) has successfully connected to `*server*`.

*[generic function]* `on-server-error driver error`

  Event called when `*server*` has experienced some error. `error` is the actual condition that was
  signaled. This event is called immediately before the server and all its connected client sockets
  are closed.

*[generic function]* `on-server-close driver`

  Event called after the server has been closed.

### Server objects

*[function]* `server-listen driver &key host port external-format-in external-format-out`

  Starts a TCP listener. If successful, returns a `server` object.

  * `driver` -- An instance of a driver class that will be used to dispatch `server-event-driver`
    events. This driver will also be set as the `socket-driver` for incoming connections.
  * `host` -- Either a string representing a local IP address to bind to, or a pathname to use as a
    unix socket.
  * `port` -- Port to listen on. Should not be provided if `host` is a unix socket.
  * `external-format-in` -- Default external-format-in for client connections.
  * `external-format-out` -- Default external-format-out for client connections.

*[function]* `server-driver server`

  Driver object used to dispatch `server-event-driver` events on `server`.

*[accessor]* `server-external-format-in server`

  Default external-format-in for incoming connections.

*[accessor]* `server-external-format-out server`

  Default external-format-out for incoming connections.

*[function]* `server-list-clients server`

  Returns a list of all `socket` objects connected to `server`.

*[function]* `server-count-clients server`

  Returns the number of `socket` objects connected to `server`.

*[function]* `server-pause server &key timeout`

  Pauses read events for `server`, preventing it from accepting new connections. While paused, all
  `socket`s already connected to `server` continue to work normally, but new connections will not be
  accepted. Incoming clients are not immediately turned away, although they may time out while
  connecting if the server is paused for too long. If `timeout` is provided, it is interpreted as
  the number of seconds to wait before starting to accept clients again. This function has no effect
  if `server` is already paused, although the resume timeout will still be activated.

*[function]* `server-resume server`

  Resumes read events for `server`, enabling new client connections. If `server` was already
  accepting clients, this function has no effect.
