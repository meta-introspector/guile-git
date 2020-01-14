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

(define-module (git proxy)
  #:use-module (system foreign)
  #:use-module (git structs)
  #:export (make-proxy-options)
  #:re-export (proxy-options?
               proxy-options-url
               proxy-options-type))

(define* (make-proxy-options #:key
                             url
                             (type (if url 'specified 'none)))
  "Return a <proxy-options> record.  TYPE must be a symbol: one of 'none (no
proxy), 'auto (auto-detect proxy), or 'specified (use the specified proxy
URL).  URL must be #f or a string."
  (let ((options (make-proxy-options-bytestructure)))
    (set-proxy-options-url! options url)
    (set-proxy-options-type! options type)
    options))
