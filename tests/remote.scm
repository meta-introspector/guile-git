;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2017, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (tests remote)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git)
             (git object))

(test-begin "remote")

(libgit2-init!)

(with-repository "simple-bare" directory

  (test-equal "remote lookup & name"
    "origin"
    (let* ((repository (repository-open directory))
           (remote (remote-lookup repository "origin")))
      (remote-name remote)))

  (test-equal "remote lookup, not found"
    (list GIT_ENOTFOUND GITERR_CONFIG)
    (catch 'git-error
      (lambda ()
        (let ((repository (repository-open directory)))
          (clear-git-error!)
          (remote-lookup repository "does-not-exist")))
      (lambda (key err)
        (list (git-error-code err) (git-error-class err)))))

  (test-equal "remote-url"
    ;; This is the "origin" remote in 'data/simple-bare.tgz'.
    "/home/erik/Workspace/guile-git/tests/data/simple"
    (let* ((repository (repository-open directory))
           (remote (remote-lookup repository "origin")))
      (remote-url remote)))

  (test-equal "remote-set-url!"
    "https://example.org"
    (let ((repository (repository-open directory)))
      (remote-set-url! repository "origin" "https://example.org")
      (remote-url (remote-lookup repository "origin"))))

  (test-equal "remote-ls (detached)"
    '((0 "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
	 "0000000000000000000000000000000000000000"
	 "HEAD")
      (0 "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
	 "0000000000000000000000000000000000000000"
	 "refs/heads/master"))
    (let ((remote (remote-create-detached directory)))
      (remote-connect/detached remote)
      ;; Order is unimportant(?), so sort the results.
      (sort (map (lambda (remote-head)
		   (list (remote-head-local remote-head)
			 (oid->string (remote-head-oid remote-head))
			 (oid->string (remote-head-loid remote-head))
			 (remote-head-name remote-head)))
		 (remote-ls remote))
	    (lambda (x y)
	      (string<? (list-ref x 3) (list-ref y 3)))))))

(libgit2-shutdown!)

(test-end)
