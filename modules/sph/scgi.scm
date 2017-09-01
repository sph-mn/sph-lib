; Copyright (C) 2010-2017 sph <sph@posteo.eu>
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sph scgi)
  (export
    scgi-default-address
    scgi-handle-requests
    scgi-read-header
    sph-scgi-description)
  (import
    (rnrs exceptions)
    (rnrs io ports)
    (sph)
    (sph server)
    (only (guile)
      reverse
      getuid
      set-port-encoding!)
    (only (rnrs io simple) eof-object? read-char))

  (define sph-scgi-description
    "scgi interface. a server that accepts scgi requests and calls a custom procedure to handle them.
     http://python.ca/scgi/protocol.txt")

  (define binary-char-null (char->integer #\nul))
  (define binary-char-colon (char->integer #\:))

  (define (get-netstring-length port)
    "socket -> integer
     get the length of the header"
    ;reason for an error here can be a connection closed too soon
    (let loop ((octet (get-u8 port)) (octet-buffer (list)))
      (if (eof-object? octet) (raise (quote scgi-invalid-header))
        (if (eq? binary-char-colon octet) (string->number (list->string (reverse octet-buffer)))
          (loop (get-u8 port) (pair (integer->char octet) octet-buffer))))))

  (define (get-content-length port count cont)
    "socket integer procedure:{integer integer} -> any
     count is the count of chars of the offset between the begin of the scgi message and the begin of the body.
     get the length of the body"
    (let loop ((count (- count 1)) (octet (get-u8 port)) (octet-buffer (list)) (delimiter-count 0))
      (if (= binary-char-null octet)
        (if (< delimiter-count 1)
          (loop (- count 1) (get-u8 port) octet-buffer (+ delimiter-count 1))
          (cont (string->number (list->string (reverse octet-buffer))) count))
        (loop (- count 1) (get-u8 port)
          (if (> delimiter-count 0) (pair (integer->char octet) octet-buffer) octet-buffer)
          delimiter-count))))

  (define (scgi-read-header port cont) "socket procedure:{list:header -> any} -> any"
    (let (netstring-length (get-netstring-length port))
      (get-content-length port netstring-length
        (l (content-length rest-netstring-length) "integer integer -> any"
          (cont
            (pair (pair "CONTENT_LENGTH" content-length)
              (let loop
                ( (count rest-netstring-length) (octet (get-u8 port)) (key (list))
                  (value #f) (r (list)))
                (if (> count 0)
                  (if value
                    (if (= binary-char-null octet)
                      (loop (- count 1) (get-u8 port)
                        (list) #f
                        (pair (pair (list->string (reverse key)) (list->string (reverse value))) r))
                      (loop (- count 1) (get-u8 port) key (pair (integer->char octet) value) r))
                    (if (= binary-char-null octet) (loop (- count 1) (get-u8 port) key (list) r)
                      (loop (- count 1) (get-u8 port) (pair (integer->char octet) key) value r)))
                  r))))))))

  (define scgi-default-address (string-append "/tmp/" (number->string (getuid)) "/scgi"))

  (define*
    (scgi-handle-requests proc #:optional socket thread-count address port-number .
      server-listen-args)
    "procedure:{list:header:((string . string) ...) port:client-socket ->} socket/false false/integer string ->
     start listening on a socket and call proc for each incoming request.
     the socket protocol-family depends on the address: if it starts with a slash a local unix socket is used, if it contains colons ip6, otherwise ip4.
     if socket is false, a socket is created with (socket AF_UNIX SOCK_STREAM 0). default port for tcp sockets is 6500.
     server-listen-args is passed to (sph server) server-listen"
    (apply server-listen
      (l (port)
        ; set to an 8-bit encoding because we are dealing with octets
        (set-port-encoding! port "ISO-8859-1")
        (scgi-read-header port (l (header) (proc header port))))
      (or socket
        (server-create-bound-socket (or address scgi-default-address) (or port-number 6500)))
      thread-count server-listen-args)))
