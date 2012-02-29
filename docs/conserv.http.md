# HTTP

The `conserv.http` module contains an implementation of a basic webserver that supports `HTTP/1.1`,
`HTTP/1.0`, and upgrading to new protocols (such as `WebSockets`). The HTTP module is fairly minimal
in what it provides and acts more as a low-level utility for implementing more useful web servers
and web frameworks.

`conserv.http` uses the same general architecture as other `conserv` modules. That is, it uses
generic functions dispatched by instances of `driver` classes in order to implement asynchronous
events.

An HTTP server primarily involves two objects:

  * `*http-server*`, a server built on top of conserv.tcp:server which listens for requests on a
    given port
  * `*request*`, which represents an individual HTTP request. Requests are used to access incoming
    data, such as request method, url, version, and incoming headers, as well as outgoing data such
    as outgoing headers. Requests are also writable character and byte streams, with any data
    written to them getting sent as part of the HTTP response for that request.
    
## Features

While the HTTP module is fairly minimal, it aims to provide a reasonably convenient interface for
dealing with the HTTP protocol.

  * *keep-alive support* - Requests are independent of the underlying socket
    connection. `conserv.http` supports `keep-alive` (which is the default for `HTTP/1.1`
    connections), and will automatically wait for the next http request once the current one is
    closed. Keep-alive behavior is controlled both by the reported user-agent HTTP version, and by
    setting the outgoing header `:connection` to an appropriate value (`"close"` to close the
    connection, `"keep-alive"` to keep it open).
  * *request abstraction* - `conserv.http` creates a request object for each incoming HTTP
    request. This objects can be used to access various information about the connecting user agent,
    as well as communicate with them. Requests are writable character and byte streams, and
    automatically buffer data written to them for later output. Requests are also used to control
    the external format of incoming and outgoing data.
  * *Upgrade/CONNECT support* - `conserv.http` understands the `CONNECT` request method and the
    `Upgrade` header as requests to switch protocols. An event, `on-request-ugrade`, can be used to
    convert the socket connection to an alternate protocol, such as `WebSockets`, while still
    listening on a single port.
  * *event-based, data streaming architecture* - Instead of single functions generating the entire
     output for an http response, `conserv.http` gives users the ability to send and receive data in
     a non-blocking, event-based way. This makes it easy to handle large binary data, and use
     persistent connections to push data to clients on demand (such as with Comet). Unless a
     `:content-length` header is specified in the HTTP response, `conserv.http` will automatically
     chunk-encode outgoing data.
  * *High performance* - `conserv.http` is built on `conserv`'s event-based tcp socket interface,
    which allows it to potentially support thousands of incoming clients on a single thread.

## HTTP Server

HTTP servers are created by calling `http-listen`. Any incoming tcp connections on the listening
host and port will be interpreted as HTTP requests, and `on-http-listen`, among other events, will
be invoked. `conserv.http` mostly handles the details of header parsing, request handling and
responses, and errors. It does not handle tasks such as parsing `GET` and `POST` parameters or even
parsing the request url into its components.

### Events

*[variable]* `*http-server*`

  During execution of `http-server-event-driver` events, this variable is bound to the
  associated `http-server` object. This variable is unbound outside of the scope of
  `http-server-event-driver` events.

*[generic function]* `on-http-listen driver`

  Event called when `*http-server*` has just started listening for connections.

*[generic function]* `on-http-request driver`

  Event called when an incoming HTTP request has been made. `*request*` is available at this point.
  
*[generic function]* `on-http-connection driver`

  Event called when a user-agent first connects to the server. `conserv.tcp:*socket*` contains the
  incoming socket connection. `*request*` is NOT bound at this point.

*[generic function]* `on-http-close driver`

  Event called after the `*http-server*` and all its active connections have been closed.

*[generic function]* `on-http-error driver error`

  Event called when an error has happened during processing. `error` is the actual error condition signaled.

### HTTP server objects

*[function]* `http-listen driver &key host port external-format-in external-format-out`

  Starts an HTTP server listening on `host` and `port`. If successful, returns an `http-server`
  object.

  * `driver` -- An instance of a driver class. Used to dispatch `http-server-event-driver` events.
  * `host` -- Either a string representing a local IP address to bind to, or a pathname to use as a unix socket.
  * `port` -- Port to listen on. Should not be provided if `host` is a unix socket.
  * `external-format-in` -- Default `external-format-in` for requests.
  * `external-format-out` -- Default `external-format-out` for requests.
  
*[accessor]* `http-server-driver http-server`

  User driver for http server event dispatch.

*[accessor]* `http-server-external-format-in http-server`

  Default `external-format-in` for requests.
  
*[accessor]* `http-server-external-format-out http-server`

  Default `external-format-out` for requests.

*[method]* `cl:close http-server &key abort`

  Implementation of `cl:close` for `http-server`. Shuts down all active connections and then shuts
  down the http server itself, making the port available for listening again. If `abort` is true,
  all sockets are immediately shut down without waiting for any pre-shutdown cleanup to complete.

## Requests

Requests in `conserv.http` represent the incoming-data end of HTTP requests. They contain
information about the incoming request, and are associated with incoming request body data. They
also control the default encoding of that incoming data.

### Events

The variable `*http-server*` is also available when these events are called.

*[variable]* `*request*`

  During execution of `request-event-driver` events, this variable is bound to the associated
  `request` object. This variable is unbound outside of the scope of `request-event-driver` events,
  unless otherwise noted.

*[generic function]* `on-request-data driver data`

  Event called when `*request*` has received data from the user-agent. `data` will be automatically
  encoded according to `(request-external-format *request*)`.

*[generic function]* `on-request-upgrade driver data`

  Event called when `*request*` has received either an `Upgrade:` header, or a `CONNECT` method in
  an HTTP request. When this happens, `*request*`'s socket is deregistered with the HTTP server. The
  socket is available through the `conserv.tcp:*socket*` variable. `data` is the first chunk of
  non-header data received by the socket, and is either `nil`, or an array of `(unsigned-byte 8)`,
  regardless of the `external-format` assigned to `*request*`. By default, this event immediately
  shuts down the socket.

*[generic function]* `on-request-close driver`

  Event called after `*request*` has been closed.

### Request objects

*[function]* `request-method request`

  Request method as a string (e.g. `"GET"`, `"POST"`, `"CONNECT"`)

*[function]* `request-url request`

  Request URL as a string (e.g. `"/"`, `"/blog/post?number=1"`, `"http://example.com/something"`)

*[function]* `request-http-version request`

  HTTP version as a string. (e.g. `"1.1"`, `"1.0"`)
  
*[function]* `request-headers-in request`

  An alist of `(name . value)` pairs representing the incoming headers sent by the client. Header
  names are `:keywords`. All header values are strings.
  
*[accessor]* `request-external-format request`

  External format used to encode incoming data. If `nil`, no encoding will be done, and
  `on-request-data` will receive arrays of `(unsigned-byte 8)` as its `data`.

*[accessor]* `request-response-status request`

  HTTP status code for this reply. Can be either a simple integer, such as `404`, or a cons of
  `(code . message-string)`, such as `(404 . "Not Found")`. If no message is present, only the
  code is sent to clients. Returns `(200 . "OK")` by default.

*[function]* `request-headers-out request`

  An alist containing the outgoing headers. Header names can either be strings or
  `:keywords`. Values can be any printable lisp value. These headers should be set using the
  `set-headers` function.

  Certain headers trigger special behavior:

  * `(:connection . "close")` -- Disables keep-alive (if active), and requests
    that the underlying socket connection be closed after the current request
    completes.
  * `(:connection . "keep-alive")` -- Enables keep-alive (if not already active).
  * `(:content-length . integer)` -- Disables chunked encoding of outgoing data.
  * `(:transfer-encoding . "chunked")` -- The default. Enables chunked encoding of outgoing data.

*[function]* `set-headers request &rest headers-and-values`

  Sets `request`'s outgoing headers. `headers` is a plist of `header-name` `header-value` pairs. On
  output, header values will be converted to strings with `*print-escape*` and `*print-readably*`
  set to `nil` (like `princ` or `format`'s `~A` directive). Has no effect if `write-headers` has
  already been called.
  
*[function]* `write-headers request`

  Begins http response output, including writing the response line with the response status and all
  the headers that have been set for `request`. This function should only be called once per
  `request`. It is implicitly called as soon as any attempt is made to write to `request` through other
  methods.
  
*[accessor]* `request-external-format-out request`

  The external format used to encode outgoing strings. If `nil`, attempting to write a string or
  character to the reply will signal an error -- only arrays of `(unsigned-byte 8)` will be
  allowed.

*[method]* `cl:close request &key abort`

  Ends the HTTP request associated with this `request`. If the request headers have not been written
  and `abort` is `nil`, the response headers are written to the user agent before closing the
  request. If `abort` is true, the associated request and its socket are immediately closed and
  nothing else is written to the user agent, and queued output will not be flushed before closing
