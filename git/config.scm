;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
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

(define-module (git config)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git structs)
  #:use-module (git types)
  #:export (config-foreach
            config-fold
            config-get-entry))

;;; config https://libgit2.github.com/libgit2/#HEAD/group/config

(define %config-entry-free (libgit2->pointer "git_config_entry_free"))

(define (pointer->config-entry! pointer)
    (set-pointer-finalizer! pointer %config-entry-free)
      (pointer->config-entry pointer))

(define config-foreach
  (let ((proc (libgit2->procedure* "git_config_foreach" '(* * *))))
    (lambda (config callback)
      (let ((callback* (procedure->pointer int
                                           (lambda (entry _)
                                             (callback (pointer->config-entry! entry)))
                                           (list '* '*))))
        (proc (config->pointer config) callback* %null-pointer)))))

(define (config-fold proc knil config)
  (let ((out knil))
    (config-foreach
      config
      (lambda (entry)
        (set! out (proc entry out))
        0))
    out))

(define config-get-entry
  (let ((proc (libgit2->procedure* "git_config_get_entry" '(* * *))))
    (lambda (config name)
      (let ((out (make-double-pointer)))
        (proc out (config->pointer config) (string->pointer name))
        (pointer->config-entry! (dereference-pointer out))))))
