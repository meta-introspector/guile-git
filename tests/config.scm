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

(define-module (tests config)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git))

(test-begin "config")

(libgit2-init!)

(with-repository "simple-bare" directory

  (test-equal "config entry get"
    "true"
    (let* ((repository (repository-open directory))
           (config (repository-config repository))
           (entry (config-get-entry config "core.bare")))
      (config-entry-value entry)))

  (test-equal "config entry"
    "true"
    (let* ((repository (repository-open directory))
           (config (repository-config repository))
           (bare #f))
      (config-foreach config (lambda (entry)
                               (when (equal? (config-entry-name entry) "core.bare")
                                 (set! bare (config-entry-value entry)))
                               0))
      bare))

  (test-equal "config entry fold"
    "true"
    (let* ((repository (repository-open directory))
           (config (repository-config repository)))
      (config-fold (lambda (entry out)
                     (format #t "~a~%" entry)
                     (if (equal? (config-entry-name entry) "core.bare")
                         (config-entry-value entry)
                         out))
                   #f config))))

(libgit2-shutdown!)

(test-end)