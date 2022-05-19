;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (git settings)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:export (set-owner-validation!
            set-tls-certificate-locations!
            set-user-agent!))

;; 'git_libgit2_opt_t' enum defined in <git2/common.h>.
(define GIT_OPT_GET_MWINDOW_SIZE 0)
(define GIT_OPT_SET_MWINDOW_SIZE 1)
(define GIT_OPT_GET_MWINDOW_MAPPED_LIMIT 2)
(define GIT_OPT_SET_MWINDOW_MAPPED_LIMIT 3)
(define GIT_OPT_GET_SEARCH_PATH 4)
(define GIT_OPT_SET_SEARCH_PATH 5)
(define GIT_OPT_SET_CACHE_OBJECT_LIMIT 6)
(define GIT_OPT_SET_CACHE_MAX_SIZE 7)
(define GIT_OPT_ENABLE_CACHING 8)
(define GIT_OPT_GET_CACHED_MEMORY 9)
(define GIT_OPT_GET_TEMPLATE_PATH 10)
(define GIT_OPT_SET_TEMPLATE_PATH 11)
(define GIT_OPT_SET_SSL_CERT_LOCATIONS 12)
(define GIT_OPT_SET_USER_AGENT 13)
(define GIT_OPT_ENABLE_STRICT_OBJECT_CREATION 14)
(define GIT_OPT_ENABLE_STRICT_SYMBOLIC_REF_CREATION 15)
(define GIT_OPT_SET_SSL_CIPHERS 16)
(define GIT_OPT_GET_USER_AGENT 17)
(define GIT_OPT_ENABLE_OFS_DELTA 18)
(define GIT_OPT_ENABLE_FSYNC_GITDIR 19)
(define GIT_OPT_GET_WINDOWS_SHAREMODE 20)
(define GIT_OPT_SET_WINDOWS_SHAREMODE 21)
(define GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION 22)
(define GIT_OPT_SET_ALLOCATOR 23)
(define GIT_OPT_ENABLE_UNSAVED_INDEX_SAFETY 24)
(define GIT_OPT_GET_PACK_MAX_OBJECTS 25)
(define GIT_OPT_SET_PACK_MAX_OBJECTS 26)
(define GIT_OPT_DISABLE_PACK_KEEP_FILE_CHECKS 27)
(define GIT_OPT_ENABLE_HTTP_EXPECT_CONTINUE 28)
(define GIT_OPT_GET_MWINDOW_FILE_LIMIT 29)
(define GIT_OPT_SET_MWINDOW_FILE_LIMIT 30)
(define GIT_OPT_SET_ODB_PACKED_PRIORITY 31)
(define GIT_OPT_SET_ODB_LOOSE_PRIORITY 32)
(define GIT_OPT_GET_EXTENSIONS 33)
(define GIT_OPT_SET_EXTENSIONS 34)
(define GIT_OPT_GET_OWNER_VALIDATION 35)
(define GIT_OPT_SET_OWNER_VALIDATION 36)

(define set-owner-validation!
  (let ((proc (libgit2->procedure* "git_libgit2_opts" (list int int))))
    (lambda (owner-validation?)
      "Enable/disable owner validation checks.  When enabled, raise an error
when a repository directory is not owned by the current user.  See
CVE-2022-24765."
      (proc GIT_OPT_SET_OWNER_VALIDATION (if owner-validation? 1 0)))))

(define set-tls-certificate-locations!
  (let ((proc (libgit2->procedure* "git_libgit2_opts" (list int '* '*))))
    (lambda* (directory #:optional file)
      "Search for TLS certificates under FILE (a certificate bundle) or under
DIRECTORY (a directory containing one file per certificate, with \"hash
symlinks\" as created by OpenSSL's 'c_rehash').  Either can be #f but not both.
This is used when transferring from a repository over HTTPS."
      (proc GIT_OPT_SET_SSL_CERT_LOCATIONS
            (if file (string->pointer file) %null-pointer)
            (if directory (string->pointer directory) %null-pointer)))))

(define set-user-agent!
  (let ((proc (libgit2->procedure* "git_libgit2_opts" (list int '*))))
    (lambda (user-agent)
      "Append USER-AGENT to the 'User-Agent' HTTP header."
      (proc GIT_OPT_SET_USER_AGENT (string->pointer user-agent)))))
