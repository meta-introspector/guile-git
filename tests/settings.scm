;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2022 André Batista <nandre@riseup.net>
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

(define-module (tests settings)
  #:use-module (git)
  #:use-module (tests helpers)
  #:use-module (srfi srfi-64))

(test-begin "settings")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "disable owner validation"
    #f
    ((lambda ()
       (set-owner-validation! #f)
       (owner-validation?))))

  (test-equal "enable owner validation"
    #t
    ((lambda ()
      (set-owner-validation! #t)
      (owner-validation?)))))

(libgit2-shutdown!)

(test-end)
