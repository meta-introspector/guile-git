;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (tests clone)
  #:use-module (git)
  #:use-module (tests helpers)
  #:use-module (tests ssh)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "clone")

(libgit2-init!)

(define (make-ssh-url dir port)
  (format #f "ssh://localhost:~a/~a" port dir))

(define ssh-server-port 8899)

(define (clone-test directory auth-method)
  (let* ((repo-dir (in-vicinity (getcwd) directory))
         (clone-dir (in-vicinity repo-dir "out")))
    (clone (make-ssh-url repo-dir ssh-server-port)
           clone-dir
           (make-clone-options #:fetch-options
                               (make-fetch-options auth-method)))
    (let* ((repository (repository-open clone-dir))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-id (commit-lookup repository oid))))))

;; Skip the following tests when sshd is unavailable.
(unless (sshd-available?)
  (test-skip 4))

(with-sshd-server ssh-server-port

  (with-repository "simple-bare" directory
    (test-equal "clone-auth-ssh-credentials"
      "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
      (clone-test directory (make-client-ssh-auth))))

  (with-repository "simple-bare" directory
    (test-equal "clone-auth-ssh-agent"
      "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
      (with-ssh-agent
       (clone-test directory (%make-auth-ssh-agent)))))

  (with-repository "simple-bare" directory
    (test-assert "clone-and-fetch-auth-ssh-credentials"
      (let* ((auth (make-client-ssh-auth))
             (do-clone (clone-test directory auth))
             (clone-dir (in-vicinity directory "out"))
             (repository (repository-open clone-dir))
             (remote (remote-lookup repository "origin")))
        (remote-fetch remote #:fetch-options (make-fetch-options auth))
        #t)))

  (test-assert "clone + transfer-progress"
    (with-repository "simple-bare" repository-directory
      (let ((stats '()))                          ;list of <indexer-progress>
        (let* ((checkout-directory (in-vicinity repository-directory
                                                "checkout"))
               (transfer-progress (lambda (progress)
                                    (set! stats (cons progress stats))
                                    #t))
               (fetch-options (make-fetch-options (make-client-ssh-auth)
                                                  #:transfer-progress
                                                  transfer-progress)))

          (clone (make-ssh-url (canonicalize-path repository-directory)
                               ssh-server-port)
                 checkout-directory
                 (make-clone-options #:fetch-options fetch-options)))

        ;; Make sure the <indexer-progress> records we got exhibit
        ;; monotonic growth.
        (match (reverse stats)
          ((first rest ...)
           (let ((max (indexer-progress-total-objects first)))
             (equal? (map indexer-progress-received-objects
                          (take (cons first rest) (+ max 1)))
                     (iota (+ max 1))))))))))

(libgit2-shutdown!)

(test-end)

;; Local Variables:
;; eval: (put 'with-sshd-server 'scheme-indent-function 1)
;; End:
