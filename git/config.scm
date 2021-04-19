;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define %config-entry-free!
  (libgit2->procedure void "git_config_entry_free" '(*)))

(define config-foreach
  (let ((proc (libgit2->procedure* "git_config_foreach" '(* * *)))
        (wrap (lambda (callback)
                (lambda (ptr _)
                  ;; Note: do *not* call %CONFIG-ENTRY-FREE! on PTR since PTR
                  ;; is documented as being valid only for the duration of
                  ;; the iteration.
                  (callback (pointer->config-entry ptr))))))
    (lambda (config callback)
      (let ((callback* (procedure->pointer int (wrap callback)
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
        (let* ((ptr   (dereference-pointer out))
               (entry (pointer->config-entry ptr)))
          ;; It's our responsibility to free PTR.
          (%config-entry-free! ptr)
          entry)))))
