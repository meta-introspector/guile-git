;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guile-Git.
;;;
;;; Guile-Git is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-Git is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-Git.  If not, see <http://www.gnu.org/licenses/>.

(define-module (tests proxy)
  #:use-module (git)
  #:use-module (tests helpers)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri))

(define %proxy-port 12345)

(define-record-type <box>
  (make-box value)
  box?
  (value box-ref box-set!))

(define (spawn-proxy box)
  "Spawn an HTTP server that serves one request and stores it into BOX."
  (define (handle-request request body)
    (box-set! box request)
    (values (build-response #:code 404)
            #vu8()))

  ;; Create the socket here to ensure that the server's up and running when
  ;; this procedure returns.
  (let* ((socket (socket AF_INET SOCK_STREAM 0))
         (mutex  (make-mutex))
         (ready  (make-condition-variable)))
    (setsockopt socket SOL_SOCKET SO_REUSEPORT 1)
    (bind socket AF_INET INADDR_LOOPBACK %proxy-port)
    (with-mutex mutex
      (call-with-new-thread
       (lambda ()
         (let* ((impl   (lookup-server-impl 'http))
                (server (open-server impl `(#:socket ,socket))))
           (signal-condition-variable ready)
           (serve-one-client handle-request impl server '())
           (close-server impl server))))

      (wait-condition-variable ready mutex
                               (+ (current-time) 10)))))

(define (clone-through-proxy url)
  "Spawn a proxy, attempt to clone URL through that proxy, and return the
request received by the proxy."
  (let ((box (make-box #f)))
    (spawn-proxy box)
    (let* ((fetch-options (make-fetch-options
                           #:proxy-url
                           ;; Note: libgit2 wants a proper URL with a path.
                           (string-append "http://localhost:"
                                          (number->string %proxy-port)
                                          "/")))
           (clone-options (make-clone-options
                           #:fetch-options fetch-options)))
      (catch 'git-error
        (lambda ()
          (clone url "/tmp/guile-git-clone-test" clone-options))
        (lambda _
          (match (box-ref box)
            ((? request? request)
             (list (request-method request)
                   (uri->string (request-uri request))))))))))

(test-begin "proxy")


;; Guile < 3.0.3 doesn't recognize the CONNECT method in (web http).
(when (string<? (version) "3.0.3")
  (test-skip 1))

(test-equal "clone with HTTPS proxy"
  '(CONNECT "example.org:443")
  (clone-through-proxy "https://example.org/example.git"))

;; XXX: libgit2 1.0.1 doesn't support HTTP proxy, so we only test HTTPS.  See
;; <https://github.com/libgit2/libgit2/issues/5650>.

(test-end "proxy")
