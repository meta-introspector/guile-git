;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2023 Sören Tempel <soeren@soeren-tempel.net>
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

;; This Scheme script checks the size of the specified bytestructure
;; descriptors, for each struct defined in `git/structs.scm`, against
;; the size of the C type as determined using `sizeof()`.
;;
;; If the size doesn't match, then the bytestructure descriptor needs
;; to be adjusted for the utilized libgit2 version, e.g. new members
;; may need to be added to the bytestructure or types of existing
;; members must be updated accordingly.

(use-modules (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim)

             (git structs)
             (bytestructures guile))

;; Generated using (with some minor manual fixups):
;;
;;     awk '
;;         /^\(define %/ {
;;             libgit=$2
;;             gsub("-", "_", libgit)
;;             gsub("%", "", libgit)
;;             printf("(cons \"git_%s\" (@@ (git structs) %%%s))\n", libgit, $2)
;;         }
;;     ' < git/structs.scm
;;
(define structs
  (list
    (cons "git_time" (@@ (git structs) %time))
    (cons "git_signature" (@@ (git structs) %signature))
    (cons "git_error" (@@ (git structs) %error))
    (cons "git_strarray" (@@ (git structs) %strarray))
    (cons "git_status_options" (@@ (git structs) %status-options))
    (cons "git_diff_file" (@@ (git structs) %diff-file))
    (cons "git_diff_binary_file" (@@ (git structs) %diff-binary-file))
    (cons "git_diff_delta" (@@ (git structs) %diff-delta))
    (cons "git_diff_binary" (@@ (git structs) %diff-binary))
    (cons "git_status_entry" (@@ (git structs) %status-entry))
    (cons "git_diff_line" (@@ (git structs) %diff-line))
    (cons "git_diff_hunk" (@@ (git structs) %diff-hunk))
    (cons "git_config_entry" (@@ (git structs) %config-entry))
    (cons "git_proxy_options" (@@ (git structs) %proxy-options))
    (cons "git_indexer_progress" (@@ (git structs) %indexer-progress))
    (cons "git_remote_callbacks" (@@ (git structs) %remote-callbacks))
    (cons "git_fetch_options" (@@ (git structs) %fetch-options))
    (cons "git_checkout_options" (@@ (git structs) %checkout-options))
    (cons "git_clone_options" (@@ (git structs) %clone-options))
    (cons "git_submodule_update_options" (@@ (git structs) %submodule-update-options))
    (cons "git_remote_head" (@@ (git structs) %remote-head))
    (cons "git_describe_options" (@@ (git structs) %describe-options))
    (cons "git_describe_format_options" (@@ (git structs) %describe-format-options))
    (cons "git_diff_options" (@@ (git structs) %diff-options))))

(define (run-c99 source-file)
  (define c99-compiler
    (or (getenv "CC") "gcc"))

  (let* ((dest "/tmp/guile-git-sanity")
         (cmd  (format #f "~a -Werror -Wall -std=c99 -o ~s ~s && ~a"
                          c99-compiler dest source-file dest))
         (port (open-input-pipe cmd))
         (out  (read-string port)))
    (close-input-port port)
    (delete-file dest)
    out))

(define (sizeof type-name)
  (let ((file-name "/tmp/guile-git-sanity.c"))
    (call-with-output-file file-name
      (lambda (port)
        (format port
          "
            #include <stdio.h>
            #include <git2/remote.h>
            #include <git2/proxy.h>
            #include <git2/types.h>
            #include <git2/errors.h>
            #include <git2/status.h>
            #include <git2/config.h>
            #include <git2/checkout.h>
            #include <git2/clone.h>
            #include <git2/submodule.h>
            #include <git2/describe.h>

            int main(void) {
              printf(\"%zu\", sizeof(~a));
              return 0;
            }
          " type-name)))

    (let ((type-size (string->number (run-c99 file-name))))
      (delete-file file-name)
      type-size)))

(define (main . args)
  (for-each
    (lambda (pair)
      (let* ((c-size (sizeof (car pair)))
             (s-size (bytestructure-descriptor-size (cdr pair)))
             (match? (eq? c-size s-size)))
        (format #t "~a: ~a\n" (if match? "PASS" "FAIL") (car pair))
        (unless match?
          (format #t "  expected: ~d bytes\n  actual:   ~d bytes\n" c-size s-size)
          (exit #f))))
    structs))
