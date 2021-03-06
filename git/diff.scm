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

(define-module (git diff)
  #:use-module (system foreign)
  #:use-module (git errors)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (DIFF-OPTIONS-VERSION
            GIT-SUBMODULE-IGNORE-UNSPECIFIED
            GIT-SUBMODULE-IGNORE-NONE
            GIT-SUBMODULE-IGNORE-UNTRACKED
            GIT-SUBMODULE-IGNORE-DIRTY
            GIT-SUBMODULE-IGNORE-ALL
            GIT-DIFF-FORMAT-PATCH
            GIT-DIFF-FORMAT-PATCH-HEADER
            GIT-DIFF-FORMAT-RAW
            GIT-DIFF-FORMAT-NAME-ONLY
            GIT-DIFF-FORMAT-NAME-STATUS
            GIT-DIFF-FORMAT-PATCH-ID
            
            make-diff-options
            diff-index-to-index
            diff-index-to-workdir
            diff-tree-to-index
            diff-tree-to-tree
            diff-tree-to-workdir
            diff->string))

;;; https://libgit2.org/libgit2/#HEAD/group/diff
(define DIFF-OPTIONS-VERSION 1)

(define GIT-SUBMODULE-IGNORE-UNSPECIFIED -1)
(define GIT-SUBMODULE-IGNORE-NONE 1)
(define GIT-SUBMODULE-IGNORE-UNTRACKED 2)
(define GIT-SUBMODULE-IGNORE-DIRTY 3)
(define GIT-SUBMODULE-IGNORE-ALL 4)

(define GIT-DIFF-FORMAT-PATCH 1)        ;; full git diff
(define GIT-DIFF-FORMAT-PATCH-HEADER 2) ;; just the file headers of patch
(define GIT-DIFF-FORMAT-RAW 3)          ;; like git diff --raw
(define GIT-DIFF-FORMAT-NAME-ONLY 4)    ;; like git diff --name-only
(define GIT-DIFF-FORMAT-NAME-STATUS 5)  ;; like git diff --name-status
(define GIT-DIFF-FORMAT-PATCH-ID 6)     ;; git diff as used by git patch-id

(define make-diff-options
  ;; Return a <diff-options> structure for use with DIFF procedures.
  (let ((proc (libgit2->procedure* "git_diff_options_init"
                                   `(* ,unsigned-int))))
    (lambda* (#:key (version DIFF-OPTIONS-VERSION))
      (let ((diff-options (make-diff-options-bytestructure)))
        (proc (diff-options->pointer diff-options) version)
        diff-options))))

(define* diff-index-to-index
  (let ((proc (libgit2->procedure* "git_diff_index_to_index" '(* * * * *))))
    (lambda* (repository old-index new-index #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (index->pointer old-index)
              (index->pointer new-index)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-index-to-workdir
  (let ((proc (libgit2->procedure* "git_diff_index_to_workdir" '(* * * *))))
    (lambda* (repository index #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (index->pointer index)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-tree-to-index
  (let ((proc (libgit2->procedure* "git_diff_tree_to_index" '(* * * * *))))
    (lambda* (repository old-tree index #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (tree->pointer old-tree)
              (index->pointer index)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-tree-to-tree
  (let ((proc (libgit2->procedure* "git_diff_tree_to_tree" '(* * * * *))))
    (lambda* (repository old-tree new-tree #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (tree->pointer old-tree)
              (tree->pointer new-tree)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-tree-to-workdir
  (let ((proc (libgit2->procedure* "git_diff_tree_to_workdir" '(* * * * ))))
    (lambda* (repository old-tree #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (tree->pointer old-tree)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff->string
  (let ((proc (libgit2->procedure* "git_diff_to_buf" `(* * ,int))))
    (lambda* (diff #:optional (format GIT-DIFF-FORMAT-PATCH))
      (let ((out (make-buffer)))
        (proc out (diff->pointer diff) format)
        (buffer-content/string out)))))
