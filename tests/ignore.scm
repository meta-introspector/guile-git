;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2021 Fredrik Salomonsson <plattfot@posteo.net>
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

(define-module (tests ignore)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format))

(use-modules (tests helpers))
(use-modules (git))

(test-begin "ignore")

(libgit2-init!)

(with-repository "simple" directory
  (call-with-output-file (string-append directory file-name-separator-string ".gitignore")
    (lambda (port)
      (format port "directory")))
  (let ((repository (repository-open directory)))
    (test-equal "ignore path is ignored, directory"
      #t
      (ignore-path-is-ignored repository "directory"))
    (test-equal "ignore path is ignored, directory/message"
      #t
      (ignore-path-is-ignored repository
                              (string-append "directory" file-name-separator-string "message")))
    (test-equal "ignore path is not ignored, README"
      #f
      (ignore-path-is-ignored repository "README"))))

(libgit2-shutdown!)

(test-end)
