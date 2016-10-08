;;; Copyright © Ludovic Courtès <ludo@gnu.org>
;;; Released under the GNU GPL version 3 or later.

(define-module (git)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (git config)
  #:export (repository?
	    open-repository
	    reference?
	    repository-head
	    reference-target
	    oid?
	    commit-signature))

;; DRAFT!

(define libgit2
  (dynamic-link %libgit2))

(define (libgit2->procedure return name params)
  (pointer->procedure return (dynamic-func name libgit2) params))

(define-inlinable (libgit2->procedure* name params)
  (let ((proc (libgit2->procedure int name params)))
    (lambda args
      (let ((ret (apply proc args)))
	(unless (zero? ret)
	  (throw 'git-error ret))))))

(define initialize!
  (libgit2->procedure int "git_libgit2_init" '()))

(define-syntax define-libgit2-type
  (lambda (s)
    "Define a wrapped pointer type for an opaque type of libgit2."
    (syntax-case s ()
      ((_ name)
       (let ((symbol     (syntax->datum #'name))
	     (identifier (lambda (symbol)
			   (datum->syntax #'name symbol))))
	 (with-syntax ((rtd    (identifier (symbol-append '< symbol '>)))
		       (pred   (identifier (symbol-append symbol '?)))
		       (wrap   (identifier (symbol-append 'pointer-> symbol)))
		       (unwrap (identifier (symbol-append symbol '->pointer))))
	   #`(define-wrapped-pointer-type rtd
	       pred
	       wrap unwrap
	       (lambda (obj port)
		 (format port "#<git-~a ~a>"
			 #,(symbol->string symbol)
			 (number->string (pointer-address (unwrap obj))
					 16))))))))))

(define-libgit2-type annotated-commit)
(define-libgit2-type blame)
(define-libgit2-type blame-options)
(define-libgit2-type blob)
(define-libgit2-type branch-iterator)
(define-libgit2-type checkout-options)
(define-libgit2-type commit)
(define-libgit2-type config)
(define-libgit2-type cred)
(define-libgit2-type diff)
(define-libgit2-type diff-delta)
(define-libgit2-type diff-options)
(define-libgit2-type index)
(define-libgit2-type object)
(define-libgit2-type oid)
(define-libgit2-type refdb)
(define-libgit2-type reference)
(define-libgit2-type repository)
(define-libgit2-type signature)
(define-libgit2-type tree)

(define %buffer-struct                            ;git_buf
  (list '* size_t size_t))

(define (make-buffer)
  (make-c-struct %buffer-struct `(,%null-pointer 0 0)))

(define free-buffer
  (libgit2->procedure void "git_buf_free" '(*)))

(define (buffer-content buf)
  (match (parse-c-struct buf %buffer-struct)
    ((pointer asize size)
     (pointer->bytevector pointer size))))

(define (buffer-content/string buf)
  (match (parse-c-struct buf %buffer-struct)
    ((pointer asize size)
     (pointer->string pointer size "UTF-8"))))

(define (make-double-pointer)
  (bytevector->pointer (make-bytevector (sizeof '*))))

;;; annotated

(define annotated-commit-free
  (let ((proc (libgit2->procedure void "git_annotated_commit_free" '(*))))
    (lambda (commit)
      (proc (annotated-commit->pointer commit)))))

(define annotated-commit-from-fetchhead
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_fetchhead" '(* * * * *))))
    (lambda (repository branch-name remote-url id)
      (let ((out (make-double-pointer)))
	(proc out
	      (repository->pointer repository)
	      (string->pointer branch-name)
	      (string->pointer remote-url)
	      (oid->pointer id))
	(pointer->annotated-commit (dereference-pointer out))))))

(define annotated-commit-from-ref
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_ref" '(* * *))))
    (lambda (repository reference)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) (reference->pointer reference))
	(pointer->annotated-commit (dereference-pointer out))))))

(define annotated-commit-from-revspec
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_revspec" '(* * *))))
    (lambda (repository revspec)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) (string->pointer revspec))))))

(define annotated-commit-id
  (let ((proc (libgit2->procedure '* "git_annotated_commit_id" '(*))))
    (lambda (commit)
      (pointer->oid (proc (annotated-commit->pointer commit))))))

(define annotated-commit-lookup
  (let ((proc (libgit2->procedure* "git_annotated_commit_lookup")))
    (lambda (repository id)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) (oid->pointer id))
	(pointer->annotated-commit (dereference-pointer out))))))

;;; attr

(define attr-add-macro
  (let ((proc (libgit2->procedure* "git_attr_add_macro" '(* * *))))
    (lambda (repository name values)
      (proc (repository->pointer repository)
	    (string->pointer name)
	    (string->pointer values)))))

(define attr-cache-flush
  (let ((proc (libgit2->procedure void "git_attr_cache_flush" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_foreach

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_get

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_get_many

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_value

;;; blame

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_buffer

(define blame-file
  (let ((proc (libgit2->procedure* "git_blame_file" '(* * * *))))
    (lambda (repository path options)
      (let ((out (make-double-pointer)))
	(proc out
	      (repository->pointer repository)
	      (string->pointer path)
	      (blame-options->pointer options))
	(pointer->blame (dereference-pointer out))))))

(define blame-free
  (let ((proc (libgit2->procedure void "git_blame_free" '(*))))
    (lambda (blame)
      (proc (blame->pointer blame)))))

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_byindex

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_byline

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_count

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_init_options

;;; blob https://libgit2.github.com/libgit2/#HEAD/group/blob

;;; branch https://libgit2.github.com/libgit2/#HEAD/group/branch

(define branch-create
  (let ((proc (libgit2->procedure* "git_branch_create" `(* * * * ,int))))
    (lambda (repository branch-name target force)
      (let ((out (make-double-pointer)))
	(proc out
	      (repository->pointer repository)
	      (string->pointer branch-name)
	      (commit->pointer target)
	      (if force 1 0))
	(pointer->reference (dereference-pointer out))))))

(define branch-create-from-annotated
  (let ((proc (libgit2->procedure* "git_branch_create_from_annotated" `(* * * * ,int))))
    (lambda (repository branch-name commit force)
      (let ((out (make-double-pointer)))
	(proc out
	      (repository->pointer repository)
	      (string->pointer branch-name)
	      (annotated-commit->pointer commit)
	      (if force 1 0))
	(pointer->reference (dereference-pointer out))))))

(define branch-delete
  (let ((proc (libgit2->procedure* "git_branch_delete" '(*))))
    (lambda (branch)
      (proc (reference->pointer branch)))))

(define branch-is-head?
  (let ((proc (libgit2->procedure int "git_branch_is_head" '(*))))
    (lambda (branch)
      (case (proc (reference->pointer branch))
	((0) #f)
	((1) #t)
	(else => (lambda (code) (throw 'git-error code)))))))

(define branch-iterator-free
  (let ((proc (libgit2->procedure void "git_branch_iterator_free" '(*))))
    (lambda (iterator)
      (proc (branch-iterator->pointer iterator)))))

(define GIT-BRANCH-LOCAL 1)
(define GIT-BRANCH-REMOTE 2)
(define GIT-BRANCH-ALL (logior GIT-BRANCH-LOCAL GIT-BRANCH-ALL))

(define branch-iterator-new
  (let ((proc (libgit2->procedure* "git_branch_iterator_new" `(* * ,int))))
    (lambda (repository flags)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) flags)
	(pointer->branch-iterator (dereference-pointer out))))))

(define branch-lookup
  (let ((proc (libgit2->procedure* "git_branch_lookup" `(* * * ,int))))
    (lambda (repository branch-name type)
      (let ((out (make-double-pointer)))
	(proc out
	      (repository->pointer repository)
	      (string->pointer branch-name)
	      type)
	(pointer->reference (dereference-pointer out))))))

(define branch-move
  (let ((proc (libgit2->procedure* "git_branch_move" `(* * * ,int))))
    (lambda (reference new-branch-name force)
      (let ((out (make-double-pointer)))
	(proc out
	      (reference->pointer reference)
	      (string->pointer new-branch-name)
	      (if force 1 0))
	(pointer->reference (dereference-pointer out))))))

(define branch-name
  (let ((proc (libgit2->procedure* "git_branch_name" '(* *))))
    (lambda (reference)
      (let ((out (make-double-pointer)))
	(proc out (reference->pointer reference))
	(pointer->string (dereference-pointer out))))))

(define branch-next
  (let ((proc (libgit2->procedure* "git_branch_next" '(* * *))))
    (lambda (iterator)
      (let ((out (make-double-pointer))
	    (out-type (make-double-pointer)))
	(proc out out-type (branch-iterator->pointer iterator))
	(values (pointer->reference (dereference-pointer out))
		(pointer-address (dereference-pointer out-type)))))))

(define branch-set-upstream
  (let ((proc (libgit2->procedure* "git_branch_set_upstream" '(* *))))
    (lambda (branch upstream-name)
      (proc (reference->pointer branch) (string->pointer upstream-name)))))

(define branch-upstream
  (let ((proc (libgit2->procedure* "git_branch_upstream" '(* *))))
    (lambda (branch)
      (let ((out (make-double-pointer)))
	(proc out (reference->pointer branch))
	(pointer->reference (dereference-pointer out))))))

;;; checkout https://libgit2.github.com/libgit2/#HEAD/group/checkout

(define checkout-head
  (let ((proc (libgit2->procedure* "git_checkout_head" '(* *))))
    (lambda (repository options)
      (proc (repository->pointer repository) %null-pointer))))

(define checkout-index
  (let ((proc (libgit2->procedure* "git_checkout_index" '(* * *))))
    (lambda (repository index options)
      (proc (repository->pointer repository)
	    (index->pointer index)
	    %null-pointer))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/checkout/git_checkout_init_options

(define checkout-tree
  (let ((proc (libgit2->procedure* "git_checkout_tree" `(* * *))))
    (lambda (repository treeish)
      (proc (repository->pointer repository)
	    (object->pointer treeish)
	    %null-pointer))))

;;; cherrypick https://libgit2.github.com/libgit2/#HEAD/group/cherrypick

(define cherrypick
  (let ((proc (libgit2->procedure* "git_cherrypick" '(* * *))))
    (lambda (repository commit)
      (proc (repository->pointer repository)
	    (commit->pointer commit)
	    %null-pointer))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/cherrypick/git_cherrypick_commit

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/cherrypick/git_cherrypick_init_options

;;; clone https://libgit2.github.com/libgit2/#HEAD/group/clone

(define clone
  (let ((proc (libgit2->procedure* "git_clone" '(* * * *))))
    (lambda (url local-path)
      (let ((out (make-double-pointer)))
	(proc out (string->pointer url) (string->pointer local-path) %null-pointer)))))

;; commit https://libgit2.github.com/libgit2/#HEAD/group/commit

(define commit-amend
  (let ((proc (libgit2->procedure* "git_commit_amend" '(* * * * * * * *))))
    (lambda (id commit update-ref author commiter message-encoding message tree)
      (proc (oid->pointer id)
	    (commit->pointer commit)
	    (string->pointer update-ref)
	    (signature->pointer author)
	    (signature->pointer commiter)
	    (string->pointer message-encoding)
	    (string->pointer message)
	    (tree->pointer tree)))))

(define commit-author
  (let ((proc (libgit2->procedure '* "git_commit_author" '(*))))
    (lambda (commit)
      (pointer->signature (proc (commit->pointer commit))))))

(define commit-body
  (let ((proc (libgit2->procedure '* "git_commit_body" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define commit-committer
  (lambda ((proc (libgit2->procedure '* "git_commit_committer" '(*))))
    (lambda (commit)
      (pointer->signature (proc (commit->pointer commit))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_buffer

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_from_callback

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_v

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_with_signature

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_dup

(define commit-extract-signature
  (let ((proc (libgit2->procedure* "git_commit_extract_signature" '(* * * * *))))
    (lambda* (repository oid #:optional (field "gpgsig"))
      (let ((signature (make-buffer))
	    (data      (make-buffer)))
	(proc signature data (repository->pointer repository)
	      (oid->pointer oid)
	      (string->pointer field))
	(let ((signature* (buffer-content/string signature))
	      (data*      (buffer-content/string data)))
	  (free-buffer signature)
	  (free-buffer data)
	  (values signature* data*))))))

(define commit-free
  (let ((proc (libgit2->procedure void "git_commit_free" '(*))))
    (lambda (commit)
      (proc (commit->pointer commit)))))

(define commit-header-field
  (let ((proc (libgit2->procedure* "git_commit_header_field" '(* * *))))
    (lambda (commit field)
      (let ((out (make-buffer)))
	(proc out (commit->pointer commit) (string->pointeger field))
	(let ((out* (buffer-content/string out)))
	  (free-buffer out)
	  out*)))))

(define commit-id
  (let ((proc (libgit2->procedure '* "git_commit_id" '(*))))
    (lambda (commit)
      (pointer->oid (proc (commit->pointer commit))))))

(define commit-lookup
  (let ((proc (libgit2->procedure* "git_commit_lookup" `(* * *))))
    (lambda (repository oid)
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (repository->pointer repository) (oid->pointer oid))
	(pointer->commit (dereference-pointer out))))))

(define commit-lookup-prefix
  (let ((proc (libgit2->procedure* "git_commit_lookup_prefix" `(* * * ,size_t))))
    (lambda (repository id len)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) (oid->pointer id) len)))))

(define commit-message
  (let ((proc (libgit2->procedure '* "git_commit_message" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define commit-message-encoding
  (let ((proc (libgit2->procedure '* "git_commit_message_encoding" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define commit-message-raw
  (let ((proc (libgit2->procedure '* "git_commit_message_raw" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_nth_gen_ancestor

(define commit-owner
  (let ((proc (libgit2->procedure '* "git_commit_owner" '(*))))
    (lambda (commit)
      (pointer->repository (proc (commit->pointer commit))))))

(define commit-parent
  (let ((proc (libgit2->procedure* "git_commit_parent" `(* ,unsigned-int))))
    (lambda (commit n)
      (let ((out (make-double-pointer)))
	(proc out (commit->pointer commit) n)
	(pointer->commit (dereference-pointer out))))))

(define commit-parent-id
  (let ((proc (libgit2->procedure '* "git_commit_parent_id" `(* ,unsigned-int))))
    (lambda (commit n)
      (pointer->oid (proc (commit->pointer commit) n)))))

(define commit-parentcount
  (let ((proc (libgit2->procedure unsigned-int "git_commit_parentcount" '(*))))
    (lambda (commit)
      (proc (commit->pointer commit)))))

(define commit-raw-header
  (let ((proc (libgit2->procedure '* "git_commit_raw_header" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define commit-summary
  (let ((proc (libgit2->procedure '* "git_commit_summary" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_time

(define commit-time-offset
  (let ((proc (libgit2->procedure int "git_commit_time_offset" '(*))))
    (lambda (commit)
      (proc (commit->pointer commit)))))

(define commit-tree
  (let ((proc (libgit2->procedure* "git_commit_tree" '(*))))
    (lambda (commit)
      (let ((out (make-double-pointer)))
	(proc out (commit->pointer commit))
	(pointer->tree (dereference-pointer out))))))

(define commit-tree-id
  (let ((proc (libgit2->procedure '* "git_commit_tree_id" '(*))))
    (lambda (commit)
      (pointer->oid (proc (commit->pointer commit))))))

;;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/config

;;; https://libgit2.github.com/libgit2/#HEAD/group/cred

(define cred-defaul-new
  (let ((proc (libgit2->procedure* "git_cred_default_new" '(*))))
    (lambda ()
      (let ((out (make-double-pointer)))
	(proc out)
	(pointer->cred (dereference-pointer out))))))

(define cred-free
  (let ((proc (libgit2->procedure void "git_cred_free" '(*))))
    (lambda (cred)
      (proc (cred->pointer cred)))))

(define cred-has-username?
  (let ((proc (libgit2->procedure int "git_cred_has_username" '(*))))
    (lambda (cred)
      (eq? (proc (cred->pointer cred)) 1))))

(define cred-ssh-custom-new
  (let ((proc (libgit2->procedure* "git_cred_ssh_custom_new" ,(* * * ,size_t * *))))
    (lambda (username publickey sign-callback)
      (let ((out (make-double-pointer)))
	(proc out
	      (string->pointer username)
	      (string->pointer publickey)
	      (string-length publickey)
	      (procedure-pointer sign-callback))
	(pointer->cred (dereference-pointer cred))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/cred/git_cred_ssh_interactive_new

(define cred-ssh-key-from-agent
  (let ((proc (libgit2->procedure* "git_cred_ssh_key_from_agent" '(* *))))
    (lambda (username)
      (let ((out (make-double-pointer)))
	(proc out (string->pointer username))
	(pointer->cred (dereference-pointer out))))))

(define cred-ssh-key-from-memory-new
  (let ((proc (libgit2->procedure* "git_cred_ssh_key_memory_new" '(* * * * *))))
    (lambda (username publickey privatekey passphrase)
      (let ((out (make-double-pointer)))
	(proc out
	      (string->pointer username)
	      (string->pointer publickey)
	      (string->pointer privatekey)
	      (string->pointer passphrase))
	(pointer->cred (dereference-pointer cred))))))

;; XXX: duplicate of the above?
(define cred-ssh-key-new
  (let ((proc (libgit2->procedure* "git_cred_ssh_key_new" '(* * * * *))))
    (lambda (username publickey privatekey passphrase)
      (let ((out (make-double-pointer)))
	(proc out
	      (string->pointer username)
	      (string->pointer publickey)
	      (string->pointer privatekey)
	      (string->pointer passphrase))
	(pointer->cred (dereference-pointer cred))))))

(define cred-username-new
  (let ((proc (libgit2->procedure* "git_cred_username_new" '(* *))))
    (lambda (username)
      (let ((out (make-double-pointer)))
	(proc out (string->pointer username))
	(pointer->cred (dereference-pointer out))))))

(define cred-userpass
  (let ((proc (libgit2->procedure* "git_cred_userpass" `(* * * ,unsigned-int *))))
    (lambda (url user-from-url allowed-types)
      (let ((out (make-double-pointer)))
	(proc out
	      (string->pointer url)
	      (string->pointer user-from-url)
	      allowed-types
	      %null-pointer)
	(pointer->cred (dereference-pointer out))))))

(define cred-userpass-paintext-new
  (let ((proc (libgit2->procedure* "git_cred_userpass_paintext_new" '(* * *))))
    (lambda (username password)
      (let ((out (make-double-pointer)))
	(proc out (string->pointer username) (string->pointer password))
	(pointer->cred (dereference-pointer out))))))

;;; FIXME: descript_commit https://libgit2.github.com/libgit2/#HEAD/group/describe

;;; FIXME: diff https://libgit2.github.com/libgit2/#HEAD/group/diff
 
(define diff-free
  (let ((proc (libgit2->procedure void "git_diff_free" '(*))))
    (lambda (diff)
      (proc (diff->pointer diff)))))

(define diff-get-delta
  (let ((proc (libgit2->procedure '* "git_diff_get_delta" '(*))))
    (lambda (diff)
      (pointer->diff-delta (proc (diff->pointer diff))))))

(define diff-num-deltas
  (let ((proc (libgit2->procedure size_t "git_diff_num_deltas" '(*))))
    (lambda (diff)
      (proc (diff->pointer diff)))))

;;; FIXME: fetch https://libgit2.github.com/libgit2/#HEAD/group/fetch

;;; FIXME: filter https://libgit2.github.com/libgit2/#HEAD/group/filter


;;; repository

(define repository-config
  (let ((proc (libgit2->procedure* "git_repository_config" '(* *))))
    (lambda (repository)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc out (repository->pointer repository))
	(pointer->config (dereference-pointer out))))))

(define repository-config-snapshot
  (let ((proc (libgit2->procedure* "git_repository_config_snapshot" '(* *))))
    (lambda (repository)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc out (repository->pointer repository))
	(pointer->config (dereference-pointer out))))))

(define repository-detach-head
  (let ((proc (libgit2->procedure* "git_repository_detach_head" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

(define repository-head
  (let ((proc (libgit2->procedure* "git_repository_head" '(* *))))
    (lambda (repository)
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (repository->pointer repository))
	(pointer->reference (dereference-pointer out))))))

(define repository-discover
  (let ((proc (libgit2->procedure* "git_repository_discover" `(* * ,int *))))
    (lambda (start-path across-fs ceiling-dirs)
      (let ((out (make-buffer)))
	(proc out
	      (string->pointer start-path)
	      (if across-fs 1 0)
	      (string->pointer ceiling-dirs))
	(let ((out* (buffer-content/string out)))
	  (free-buffer out)
	  out)))))

(define repository-free
  (let ((proc (libgit2->procedure* "git_repository_free" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

(define repository-get-namespace
  (let ((proc (libgit2->procedure '* "git_repository_get_namespace" '(*))))
    (lambda (repository)
      (pointer->string (proc (repository->pointer repository))))))

(define repository-head-detached?
  (let ((proc (libgit2->procedure int "git_repository_head_detached" '(*))))
    (lambda (repository)
      (case (proc (repository->pointer repository))
	((0) #f)
	((1) #t)
	(else => (lambda (code) (throw 'git-error code)))))))

(define repository-head-unborn?
  (let ((proc (libgit2->procedure int "git_repository_head_unborn" '(*))))
    (lambda (repository)
      (case (proc (repository->pointer repository))
	((0) #f)
	((1) #t)
	(else => (lambda (code) (throw 'git-error code)))))))

(define repository-ident
  (let ((proc (libgit2->procedure* "git_repository_ident" '(* * *))))
    (lambda (repository)
      (let ((name (make-bytevector (sizeof '*)))
	    (name* ((bytevector->pointer name)))
	    (email (make-bytevector (sizeof '*)))
	    (email* ((bytevector->pointer email))))
	(proc name mail (repository->pointer repository))
	(values (pointer->string (make-pointer (u64vector-ref name 0)))
		(pointer->string (make-pointer (u64vector-ref email 0))))))))

(define repository-index
  (let ((proc (libgit2->procedure* "git_repository_index" '(* *))))
    (lambda (repository)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc (repository->pointer repository))
	(pointer->index (dereference-pointer out))))))

(define repository-init
  (let ((proc (libgit2->procedure* "git_repository_init" `(* * ,int))))
    (lambda (path is-bare)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc out (string->pointer path) (if is-bare 1 0))
	(pointer->repository (dereference-pointer out))))))

(define repository-is-bare?
  (let ((proc (libgit2->procedure int "git_repository_is_bare" '(*))))
    (lambda (repository)
      (eq? (proc (repository->pointer repository)) 1))))

(define repository-is-empty?
  (let ((proc (libgit2->procedure int "git_repository_is_empty" '(*))))
    (lambda (repository)
      (eq? (proc (repository->pointer repository)) 1))))

(define repository-is-shallow?
  (let ((proc (libgit2->procedure int "git_repository_is_shallow" '(*))))
    (lambda (repository)
      (eq? (proc (repository->procedure repository)) 1))))

(define open-repository
  (let ((proc (libgit2->procedure* "git_repository_open" '(* *))))
    (lambda (file)
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (string->pointer file))
	(pointer->repository (dereference-pointer out))))))

(define repository-path
  (let ((proc (libgit2->procedure '* "git_repository_path" '(*))))
    (lambda (repository)
      (pointer->string (proc (repository->procedure repository))))))

(define repository-refdb
  (let ((proc (libgit2->procedure* "git_repository_refdb" `(* *))))
    (lambda (repository)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc out (repository->pointer repository))
	(pointer->refdb (dereference-pointer out))))))

(define repository-set-ident
  (let ((proc (libgit2->procedure* "git_repository_set_ident" '(* * *))))
    (lambda (repository name email) ;;; FIXE: make name and email optional
      (proc (repository->pointer repository)
	    (string->pointer name "UTF-8")
	    (string->pointer email "UTF-8")))))

(define repository-state
  (let ((proc (libgit2->procedure int "git_repository_state" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

(define repository-workdir
  (let ((proc (libgit2->procedure '* "git_repository_workdir" '(*))))
    (lambda (repository)
      (pointer->string (proc (repository->pointer repository))))))

;;; reference

(define reference-target
  (let ((proc (libgit2->procedure '* "git_reference_target" '(*))))
    (lambda (reference)
      (pointer->oid (proc (reference->pointer reference))))))

;;; object

(define GIT_OBJ_ANY -2)

(define lookup-object
  (let ((proc (libgit2->procedure* "git_object_lookup" `(* * * ,int))))
    (lambda* (repository oid #:optional (type GIT_OBJ_ANY))
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (repository->pointer repository) (oid->pointer oid)
	      type)
	(pointer->object (dereference-pointer out))))))

(initialize!)
