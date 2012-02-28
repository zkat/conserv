(in-package #:conserv.http)

(defvar *request*)
(setf (documentation '*request* 'variable)
      "During execution of `request-event-driver` events, this variable is bound to the associated
`request` object. This variable is unbound outside of the scope of `request-event-driver` events,
unless otherwise noted.")

(defprotocol request-event-driver (a)
  ((data ((driver a) data)
    :default-form nil
    :documentation "Event called when `*request*` has received data from the user-agent. `data` will
                    be automatically encoded according to `(request-external-format *request*)`.
                    `*reply*` is also available when this event is invoked.")
   (continue ((driver a))
    :documentation "Event called when the user-agent sent the `Expect: 100-continue` header. By
                    default, this event calls `write-continue` on `*reply*`.")
   (upgrade ((driver a) data)
    :default-form (close *socket* :abort t)
    :documentation "Event called when `*request*` has received either an `Upgrade:` header, or a
                    `CONNECT` method in an HTTP request. When this happens, `*request*`'s socket is
                    deregistered with the HTTP server. The socket is available through the
                    `conserv.tcp:*socket*` variable. `data` is the first chunk of non-header data
                    received by the socket, and is either `nil`, or an array of `(unsigned-byte 8)`,
                    regardless of the `external-format` assigned to `*request*`. By default, this
                    event immediately shuts down the socket.")
   (close ((driver a))
    :default-form nil
    :documentation "Event called after `*request*` has been closed."))
  (:prefix on-request-))

(defprotocol request (a)
  ((state ((request a))
    :accessorp t
    :documentation "Internal -- keeps track of the internal state of the request, :headers, :body,
                    or :closed")
   (method ((request a))
    :accessorp t
    :documentation "Request method as a string (e.g. `\"GET\"`, `\"POST\"`, `\"CONNECT\"`)")
   (url ((request a))
    :accessorp t
    :documentation "Request URL as a string (e.g. `\"/\"`, `\"/blog/post?number=1\"`, `\"http://example.com/something\"`)")
   (headers ((request a))
    :accessorp t
    :documentation "An alist of (name . value) pairs. Header names are :keywords. All values are
                    strings.")
   #+nil(trailers ((request a)))
   (http-version ((request a))
    :accessorp t
    :documentation "HTTP version as a string. (e.g. `\"1.1\"`, `\"1.0\"`)")
   (external-format ((request a))
    :accessorp t
    :documentation "External format used to encode incoming data. If `nil`, no encoding will be
                    done, and `on-request-data` will receive arrays of `(unsigned-byte 8)` as its
                    `data`.")
   (request-parser ((request a))
    :documentation "Internal -- Request parser object.")
   (http-server ((request a))
    :documentation "HTTP server associated with this request.")
   (driver ((request a))
    :documentation "Driver instance for dispatching REQUEST's events.")
   (keep-alive-p ((request a))
    :accessorp t
    :documentation "Controls whether the underlying connection should be closed after this request
                    is completed. For HTTP/1.1 clients, this defaults to true. For other clients,
                    the default is nil. This value should not be set directly by users, instead, set
                    the outgoing Connection: close header in this request's reply.")
   (socket ((request a))
    :documentation "TCP socket associated with this request."))
  (:prefix request-))

(defclass request ()
  ((method :initform nil :accessor request-method)
   (url :accessor request-url)
   (headers :accessor request-headers)
   (http-version :accessor request-http-version)
   (external-format :initarg :external-format :accessor request-external-format)
   (request-parser :initform (make-request-parser) :reader request-request-parser)
   (http-server :reader request-http-server :initarg :http-server)
   (driver :reader request-driver :initarg :driver)
   (keep-alive-p :accessor request-keep-alive-p :initform t)
   (state :accessor request-state :initform :headers)
   (socket :initarg :socket :reader request-socket)))
