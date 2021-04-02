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

(define-module (tests diff)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers)
             (ice-9 match))
(use-modules (git)
             (git object))

(test-begin "diff")

(libgit2-init!)

(with-repository "simple-bare" directory

  (test-equal "diff tree to tree"
    "diff --git a/directory/message b/directory/message
deleted file mode 100644
index b075b00..0000000
--- a/directory/message
+++ /dev/null
@@ -1 +0,0 @@
-a file in a directory
"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit1 (commit-lookup repository oid))
           (commit2 (commit-parent commit1)))
      (diff->string (diff-tree-to-tree repository (commit-tree commit1) (commit-tree commit2)))))

  (test-equal "diff foreach"
    '(1 0 1 1)
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit1 (commit-lookup repository oid))
           (commit2 (commit-parent commit1))
           (diff (diff-tree-to-tree repository (commit-tree commit1) (commit-tree commit2)))
           (count '(0 0 0 0)))
      (diff-foreach
        diff
        ;file-cb
        (lambda (delta progress)
          (match count
            ((f b h l)
             (set! count `(,(+ f 1) ,b ,h ,l))
             0)))
        ;binary-cb
        (lambda (delta binary)
          (match count
            ((f b h l)
             (set! count `(,f ,(+ b 1) ,h ,l))
             0)))
        ;hunk-cb
        (lambda (delta hunk)
          (match count
            ((f b h l)
             (set! count `(,f ,b ,(+ h 1) ,l))
             0)))
        ;line-cb
        (lambda (delta hunk line)
          (match count
            ((f b h l)
             (set! count `(,f ,b ,h ,(+ l 1)))
             0))))
      count))

  (test-equal "diff fold"
    '(1 0 1 1)
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit1 (commit-lookup repository oid))
           (commit2 (commit-parent commit1))
           (diff (diff-tree-to-tree repository (commit-tree commit1) (commit-tree commit2))))
      (diff-fold
        (lambda (delta progress count)
          (match count
            ((f b h l)
             `(,(+ f 1) ,b ,h ,l))))
        (lambda (delta binary count)
          (match count
            ((f b h l)
             `(,f ,(+ b 1) ,h ,l))))
        (lambda (delta hunk count)
          (match count
            ((f b h l)
             `(,f ,b ,(+ h 1) ,l))))
        (lambda (delta hunk line count)
          (match count
            ((f b h l)
             `(,f ,b ,h ,(+ l 1)))))
        '(0 0 0 0)
        diff)))

  (test-equal "diff filename"
    "directory/message"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit1 (commit-lookup repository oid))
           (commit2 (commit-parent commit1))
           (diff (diff-tree-to-tree repository (commit-tree commit1) (commit-tree commit2)))
           (name #f))
      (diff-foreach
        diff
        (lambda (delta progress)
          (set! name (diff-file-path (diff-delta-old-file delta)))
          0)
        (const 0)
        (const 0)
        (const 0))
      name)))

(libgit2-shutdown!)

(test-end)
