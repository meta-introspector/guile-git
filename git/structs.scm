;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Marius Bakke <marius@devup.no>
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

(define-module (git structs)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module ((system foreign) #:select (null-pointer?
                                           bytevector->pointer
                                           make-pointer
                                           pointer->bytevector
                                           pointer->string
                                           string->pointer
                                           sizeof
                                           dereference-pointer
                                           pointer-address
                                           void))
  #:use-module (bytestructures guile)
  #:use-module (ice-9 match)
  #:export (git-error? git-error-code git-error-message git-error-class pointer->git-error
            time->pointer pointer->time time-time time-offset
            signature->pointer pointer->signature signature-name signature-email signature-when
            oid? oid->pointer pointer->oid make-oid-pointer oid=?

            diff-file? diff-file-oid diff-file-path diff-file-size diff-file-flags diff-file-mode diff-file-id-abbrev

            diff-delta? diff-delta-status diff-delta-flags diff-delta-status diff-delta-nfiles diff-delta-old-file diff-delta-new-file

            status-entry? status-entry-status status-entry-head-to-index status-entry-index-to-workdir pointer->status-entry

            make-status-options-bytestructure status-options->pointer set-status-options-show! set-status-options-flags!

            make-remote-callbacks remote-callbacks->pointer set-remote-callbacks-version!
            make-fetch-options-bytestructure fetch-options-bytestructure fetch-options->pointer fetch-options-callbacks
            fetch-options-download-tags set-fetch-options-download-tags!
            set-remote-callbacks-credentials!
            fetch-options-proxy-options set-fetch-options-proxy-options!

            proxy-options?
            make-proxy-options-bytestructure proxy-options-bytestructure proxy-options->pointer proxy-options-callbacks
            proxy-options-url proxy-options-type
            set-proxy-options-url! set-proxy-options-type!


            make-clone-options-bytestructure clone-options-bytestructure clone-options->pointer set-clone-options-fetch-opts!

            make-describe-options-bytestructure describe-options->pointer describe-options->bytestructure
            set-describe-options-max-candidates-tag! set-describe-options-strategy!
            set-describe-options-pattern! set-describe-options-only-follow-first-parent!
            set-describe-options-show-commit-oid-as-fallback!

            make-describe-format-options-bytestructure describe-format-options->pointer describe-format-options->bytestructure
            set-describe-format-options-abbreviated-size!
            set-describe-format-options-always-use-long-format!
            set-describe-format-options-dirty-suffix!

            remote-head? remote-head-local remote-head-oid remote-head-loid remote-head-name pointer->remote-head pointer->remote-head-list))


;;; bytestructures helper

(define bytestructure->pointer
  (compose bytevector->pointer bytestructure-bytevector))

(define (pointer->bytestructure pointer struct)
  (make-bytestructure (pointer->bytevector pointer (bytestructure-descriptor-size struct))
                      0
                      struct))

;;; git-time

(define %time (bs:struct `((time ,int64) ;; time in seconds since epoch
                           (offset ,int)))) ;; timezone offset, in minutes

(define-record-type <time>
  (%make-time bytestructure)
  time?
  (bytestructure time-bytestructure))

(define (pointer->time pointer)
  (%make-time (pointer->bytestructure pointer %time)))

(define (time->pointer time)
  (bytestructure->pointer (time-bytestructure time)))

(define (time-time time)
  (bytestructure-ref (time-bytestructure time) 'time))

(define (time-offset time)
  (bytestructure-ref (time-bytestructure time) 'offset))

;;; git-signature

(define %signature (bs:struct `((name ,(bs:pointer uint8)) ;; char *
                                (email ,(bs:pointer uint8)) ;; char *
                                (when ,%time))))

(define-record-type <signature>
  (%make-signature bytestructure)
  signature?
  (bytestructure signature-bytestructure))

(define (pointer->signature pointer)
  (%make-signature (pointer->bytestructure pointer %signature)))

(define (signature->pointer signature)
  (bytestructure->pointer (signature-bytestructure signature)))

(define (signature-name signature)
  (pointer->string (make-pointer (bytestructure-ref (signature-bytestructure signature) 'name))))

(define (signature-email signature)
  (pointer->string (make-pointer (bytestructure-ref (signature-bytestructure signature) 'email))))

(define (signature-when signature)
  (let ((when* (bytestructure-ref (signature-bytestructure signature) 'when)))
    (%make-time when*)))

;;; git oid

(define-syntax GIT-OID-RAWSZ
  (identifier-syntax 20))

(define-record-type <oid>
  (%make-oid bytevector)
  oid?
  (bytevector oid-bytevector))

(define (pointer->oid pointer)
  ;; C functions typically return 'const git_oid *' and the OID's memory
  ;; belongs to the object it is associated with.  Thus, always copy the OID
  ;; contents to make sure it's not modified or freed behind our back.
  (%make-oid (bytevector-copy
              (pointer->bytevector pointer GIT-OID-RAWSZ))))

(define (oid->pointer oid)
  (bytevector->pointer (oid-bytevector oid)))

(define (make-oid-pointer)
  (bytevector->pointer (make-bytevector GIT-OID-RAWSZ)))

(define (oid=? oid1 oid2)
  "Return true if OID1 and OID2 are equal."
  ;; This is more efficient than calling 'git_oid_equal' through the FFI.
  (bytevector=? (oid-bytevector oid1)
                (oid-bytevector oid2)))

;;; git status options

(define %error
  (bs:struct `((message ,(bs:pointer uint8))
               (class   ,int))))

(define %strarray
  (bs:struct `((strings ,(bs:pointer
                          (bs:pointer uint8)))
               (count ,size_t))))

(define %status-options
  (bs:struct `((version ,unsigned-int)
               (status-show ,int)
               (flags ,unsigned-int)
               (pathspec ,%strarray))))

(define %diff-file
  (bs:struct `((oid ,(bs:vector 20 uint8))
               (path ,(bs:pointer uint8))
               (size ,int64)
               (flags ,uint32)
               (mode ,uint16)
               (id-abbrev ,uint16))))

(define %diff-delta
  (bs:struct `((status ,int)
               (flags ,uint32)
               (similarity ,uint16)
               (nfiles ,uint16)
               (old-file ,%diff-file)
               (new-file ,%diff-file))))

(define %status-entry
  (bs:struct `((status ,int)
               (head-to-index ,(bs:pointer %diff-delta))
               (index-to-workdir ,(bs:pointer %diff-delta)))))

(define (flags->symbols flags map-list)
  (fold (lambda (flag-map symbols)
          (match flag-map
            ((flag symbol)
             (if (> (logand flag flags) 0)
                 (cons symbol symbols)
                 symbols))))
        '()
        map-list))

(define (status-names status)
  (flags->symbols status
                  '((1     index-new)
                    (2     index-modified)
                    (4     index-deleted)
                    (8     index-renamed)
                    (16    index-typechange)
                    (128   wt-new)
                    (256   wt-modified)
                    (512   wt-deleted)
                    (1024  wt-typechange)
                    (2048  wt-renamed)
                    (4096  wt-unreadable)
                    (16384 ignored)
                    (32768 conflicted))))

(define-record-type <git-error>
  (%make-git-error code message class)
  git-error?
  (code    git-error-code)
  (message git-error-message)
  (class   git-error-class))

(define-record-type <diff-file>
  (%make-diff-file oid path size flags mode id-abbrev)
  diff-file?
  (oid diff-file-oid)
  (path diff-file-path)
  (size diff-file-size)
  (flags diff-file-flags)
  (mode diff-file-mode)
  (id-abbrev diff-file-id-abbrev))

(define-record-type <diff-delta>
  (%make-diff-delta status flags similarity nfiles old-file new-file)
  diff-delta?
  (status diff-delta-status)
  (flags diff-delta-flags)
  (similarity diff-delta-similarity)
  (nfiles diff-delta-nfiles)
  (old-file diff-delta-old-file)
  (new-file diff-delta-new-file))

(define-record-type <status-entry>
  (%make-status-entry status head-to-index index-to-workdir)
  status-entry?
  (status status-entry-status)
  (head-to-index status-entry-head-to-index)
  (index-to-workdir status-entry-index-to-workdir))

(define-record-type <status-options>
  (%make-status-options bytestructure)
  status-options?
  (bytestructure status-options-bytestructure))

(define* (pointer->git-error pointer code)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %error)))
        (%make-git-error code
                         (pointer->string
                          (make-pointer (bytestructure-ref bs 'message)))
                         (bytestructure-ref bs 'class)))))

(define (make-status-options-bytestructure)
  (%make-status-options (bytestructure %status-options)))

(define (status-options->pointer status-options)
  (bytestructure->pointer (status-options-bytestructure status-options)))

(define (set-status-options-show! status-options show)
  (bytestructure-set! (status-options-bytestructure status-options)
                      'status-show show))

(define (set-status-options-flags! status-options flags)
  (bytestructure-set! (status-options-bytestructure status-options)
                      'flags flags))

(define (bs-diff-file->diff-file bs)
  (%make-diff-file
   (%make-oid (bytestructure-bytevector
               (bytestructure-ref bs 'oid)))
   (pointer->string
    (make-pointer (bytestructure-ref bs 'path)))
   (bytestructure-ref bs 'size)
   (bytestructure-ref bs 'flags)
   (bytestructure-ref bs 'mode)
   (bytestructure-ref bs 'id-abbrev)))

(define (pointer->diff-delta pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %diff-delta)))
        (%make-diff-delta
         (bytestructure-ref bs 'status)
         (bytestructure-ref bs 'flags)
         (bytestructure-ref bs 'similarity)
         (bytestructure-ref bs 'nfiles)
         (bs-diff-file->diff-file (bytestructure-ref bs 'old-file))
         (bs-diff-file->diff-file (bytestructure-ref bs 'new-file))))))

(define (pointer->status-entry pointer)
  (let ((bs (pointer->bytestructure pointer %status-entry)))
    (%make-status-entry
     (status-names (bytestructure-ref bs 'status))
     (pointer->diff-delta
      (make-pointer (bytestructure-ref bs 'head-to-index)))
     (pointer->diff-delta
      (make-pointer (bytestructure-ref bs 'index-to-workdir))))))

;; proxy options: https://libgit2.org/libgit2/#HEAD/type/git_proxy_options

(define %proxy-options
  (bs:struct `((version ,unsigned-int)            ;GIT-PROXY-OPTIONS-VERSION
               (type ,int)                        ;git_proxy_t enum
               (url ,(bs:pointer void))           ;string | NULL
               (credendials ,(bs:pointer void))   ;git_cred_acquire_cb *
                                                  ;git_transport_certificate_check_cb
               (transport-certificate-check-cb ,(bs:pointer void))
               (payload ,(bs:pointer void)))))

(define GIT-PROXY-OPTIONS-VERSION 1)   ;supported version--see <git2/proxy.h>

(define-record-type <proxy-options>
  (%make-proxy-options bytestructure)
  proxy-options?
  (bytestructure proxy-options-bytestructure))

(define (make-proxy-options-bytestructure)
  (let ((bs (bytestructure %proxy-options)))
    (bytestructure-set! bs 'version GIT-PROXY-OPTIONS-VERSION)
    (%make-proxy-options bs)))

(define (proxy-options->pointer proxy-options)
  (bytestructure->pointer (proxy-options-bytestructure proxy-options)))

(define %proxy-options-strings
  ;; This weak-key 'eq?' hash table maps <proxy-options> records to pointer
  ;; objects that must outlive them.
  (make-weak-key-hash-table))

(define (symbol->proxy-type symbol)
  "Convert SYMBOL to an integer of the 'git_proxy_t' enum."
  (match symbol
    ('none      0)
    ('auto      1)
    ('specified 2)))

(define (proxy-type->symbol type)
  "Convert INTEGER, a value of the 'git_proxy_t' enum, to a symbol."
  (match type
    (0 'none)
    (1 'auto)
    (2 'specified)))

(define (proxy-options-type proxy-options)
  "Return the proxy type, a symbol, specified in PROXY-OPTIONS."
  (let ((proxy-options-bs (proxy-options-bytestructure proxy-options)))
    (proxy-type->symbol
     (bytestructure-ref proxy-options-bs 'type))))

(define (proxy-options-url proxy-options)
  "Return the proxy URL specified in PROXY-OPTIONS, or #f if there is none."
  (let* ((proxy-options-bs (proxy-options-bytestructure proxy-options))
         (ptr              (make-pointer
                            (bytestructure-ref proxy-options-bs 'url))))
    (and (not (null-pointer? ptr))
         (pointer->string ptr -1 "UTF-8"))))

(define (set-proxy-options-type! proxy-options type)
  "Change the type of proxy in PROXY-OPTIONS to TYPE, one of 'none (no
proxy), 'auto (auto-detect proxy), or 'specified (use the specified proxy
URL)."
  (let ((proxy-options-bs (proxy-options-bytestructure proxy-options)))
    (bytestructure-set! proxy-options-bs 'type
                        (symbol->proxy-type type))))

(define (set-proxy-options-url! proxy-options url)
  "Set the proxy URL in PROXY-OPTIONS to URL.  Make sure to change the proxy
type to 'specified for this to take effect."
  (let ((proxy-options-bs (proxy-options-bytestructure proxy-options))
        (str              (and url (string->pointer url "UTF-8"))))
    (if str
        (begin
          ;; Make sure STR is not reclaimed before PROXY-OPTIONS-BS.
          (hashq-set! %proxy-options-strings proxy-options-bs str)
          (bytestructure-set! proxy-options-bs 'url (pointer-address str)))
        (bytestructure-set! proxy-options-bs 'url 0))))

;; git fetch options

(define %remote-callbacks
  (bs:struct `((version ,unsigned-int)
               (sideband-progress ,(bs:pointer uint8))
               (completion ,(bs:pointer uint8))
               (credentials ,(bs:pointer uint8))
               (certificate-check ,(bs:pointer uint8))
               (transfer-progress ,(bs:pointer uint8))
               (update-tips ,(bs:pointer uint8))
               (pack-progress ,(bs:pointer uint8))
               (push-transfer-progress ,(bs:pointer uint8))
               (push-update-reference ,(bs:pointer uint8))
               (push-negotiation ,(bs:pointer uint8))
               (transport ,(bs:pointer uint8))
               (payload ,(bs:pointer uint8)))))

(define-record-type <remote-callbacks>
  (%make-remote-callbacks bytestructure)
  remote-callbacks?
  (bytestructure remote-callbacks-bytestructure))

(define REMOTE-CALLBACKS-VERSION 1)               ;<git2/remote.h>

(define (make-remote-callbacks)
  (let ((bs (bytestructure %remote-callbacks)))
    (bytestructure-set! bs 'version REMOTE-CALLBACKS-VERSION)
    (%make-remote-callbacks bs)))

(define (remote-callbacks->pointer remote-callbacks)
  (bytestructure->pointer (remote-callbacks-bytestructure remote-callbacks)))

(define (set-remote-callbacks-version! remote-callbacks version)
  (bytestructure-set! (remote-callbacks-bytestructure remote-callbacks) 'version version))

(define %fetch-options
  (bs:struct `((version ,int)
               (callbacks ,%remote-callbacks)
               (prune ,int)
               (update-fetchhead ,int)
               (download-tags ,int)
               (proxy-opts ,%proxy-options)
               (custom-headers ,%strarray))))

(define-record-type <fetch-options>
  (%make-fetch-options bytestructure)
  fetch-options?
  (bytestructure fetch-options-bytestructure))

(define (make-fetch-options-bytestructure)
  (%make-fetch-options (bytestructure %fetch-options)))

(define (fetch-options->pointer fetch-options)
  (bytestructure->pointer (fetch-options-bytestructure fetch-options)))

(define (remote-autotag-option->symbol integer)
  "Convert INTEGER, a value of the 'git_remote_autotag_option_t' enum, to a
symbol."
  (case integer
    ((0) 'unspecified)          ;follow user configuration
    ((1) 'auto)                 ;tags for objects we're downloading
    ((2) 'none)                 ;don't ask for any tags
    ((3) 'all)                  ;ask for all the tags
    (else 'unknown)))

(define (symbol->remote-autotag-option symbol)
  "Convert SYMBOL to an integer of the 'git_remote_autotag_option_t' enum."
  (case symbol
    ((unspecified) 0)
    ((auto)        1)
    ((none)        2)
    ((all)         3)
    (else          1)))

(define (fetch-options-download-tags fetch-options)
  "Return the tag download policy specified by FETCH-OPTIONS.  The policy is
denoted by a symbol: 'unspecified means to follow the user configuration,
'auto to download tags for objects we're already downloading, 'none to not
ask for any tags, and 'all to ask for all the tags."
  (remote-autotag-option->symbol
   (bytestructure-ref (fetch-options-bytestructure fetch-options)
                      'download-tags)))

(define* (set-fetch-options-download-tags! fetch-options policy)
  "Use POLICY, a symbol (see 'fetch-options-download-tags'), as the download
tag policy in FETCH-OPTIONS."
  (bytestructure-set! (fetch-options-bytestructure fetch-options)
                      'download-tags
                      (symbol->remote-autotag-option policy)))

(define (fetch-options-callbacks fetch-options)
  (bytestructure-ref (fetch-options-bytestructure fetch-options) 'callbacks))


(define (set-remote-callbacks-credentials! callbacks credentials)
  (bytestructure-set! callbacks 'credentials credentials))

(define (fetch-options-proxy-options fetch-options)
  "Return the <proxy-options> record associated with FETCH-OPTIONS."
  (let ((bs (fetch-options-bytestructure fetch-options)))
    (%make-proxy-options (bytestructure-ref bs 'proxy-opts))))

(define %fetch-options-proxy-options
  ;; This weak-key 'eq?' hash table maps <fetch-options> records to
  ;; <proxy-options> records that they aggregate and that must outlive them.
  (make-weak-key-hash-table))

(define (set-fetch-options-proxy-options! fetch-options proxy-options)
  "Use PROXY-OPTIONS as the proxy options in FETCH-OPTIONS."
  (let ((fetch-bs (fetch-options-bytestructure fetch-options))
        (proxy-bs (proxy-options-bytestructure proxy-options)))
    (bytestructure-set! fetch-bs 'proxy-opts
                        (bytestructure-bytevector proxy-bs))

    ;; Make sure PROXY-OPTIONS outlives FETCH-OPTIONS.
    (hashq-set! %fetch-options-proxy-options fetch-bs proxy-bs)))

;; git clone options

(define %checkout-options
  (bs:struct `((version ,unsigned-int)
               (checkout-strategy ,unsigned-int)
               (disable-filters ,int)
               (dir-mode ,unsigned-int)
               (file-mode ,unsigned-int)
               (file-open-flags ,int)
               (notify-flags ,unsigned-int)
               (notify-cb ,(bs:pointer uint8))
               (notify-payload ,(bs:pointer uint8))
               (progress-cb ,(bs:pointer uint8))
               (progress-payload ,(bs:pointer uint8))
               (paths ,%strarray)
               (baseline ,(bs:pointer uint8))
               (baseline-index ,(bs:pointer uint8))
               (target-directory ,(bs:pointer uint8))
               (ancestor-label ,(bs:pointer uint8))
               (our-label ,(bs:pointer uint8))
               (their-label ,(bs:pointer uint8))
               (perfdata-cb ,(bs:pointer uint8))
               (perfdata-payload ,(bs:pointer uint8)))))

(define %clone-options
  (bs:struct `((version ,int)
               (checkout-opts ,%checkout-options)
               (fetch-opts ,%fetch-options)
               (bare ,int)
               (local ,int)
               (checkout-branch ,(bs:pointer uint8))
               (repository-cb ,(bs:pointer uint8))
               (repository-cb-payload ,(bs:pointer uint8))
               (remote-cb ,(bs:pointer uint8))
               (remote-cb-payload ,(bs:pointer uint8)))))

(define-record-type <clone-options>
  (%make-clone-options bytestructure)
  clone-options?
  (bytestructure clone-options-bytestructure))

(define (make-clone-options-bytestructure)
  (%make-clone-options (bytestructure %clone-options)))

(define (clone-options->pointer clone-options)
  (bytestructure->pointer (clone-options-bytestructure clone-options)))

(define (set-clone-options-fetch-opts! clone-options fetch-options)
  (let ((clone-options-bs (clone-options-bytestructure clone-options))
        (fetch-options-bs (fetch-options-bytestructure fetch-options)))
    (bytestructure-set! clone-options-bs 'fetch-opts
                        (bytestructure-bytevector fetch-options-bs))))

;; git remote head

(define %remote-head
  (bs:struct `((local ,int)
               (oid ,(bs:vector GIT-OID-RAWSZ uint8))
               (loid ,(bs:vector GIT-OID-RAWSZ uint8))
               (name ,(bs:pointer uint8))
               (symref-target ,(bs:pointer uint8)))))

(define-record-type <remote-head>
  (%make-remote-head local oid loid name symref-target)
  remote-head?
  (local remote-head-local)
  (oid remote-head-oid)
  (loid remote-head-loid)
  (name remote-head-name)
  (symref-target remote-head-symref-target))

(define (pointer->remote-head pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %remote-head)))
        (%make-remote-head
         (bytestructure-ref bs 'local)
         (%make-oid (bytestructure-bytevector (bytestructure-ref bs 'oid)))
         (%make-oid (bytestructure-bytevector (bytestructure-ref bs 'loid)))
         (pointer->string (make-pointer (bytestructure-ref bs 'name)))
         (bytestructure-ref bs 'symref-target)))))

(define (pointer->pointer-list ptr length)
  (let ((size (sizeof '*)))
    (let ((main (pointer->bytevector ptr (* size length))))
        (let loop ((count 0)
                   (acc '()))
          (if (= count length)
              (reverse acc)
              (let* ((offset (* count size))
                     (next-ptr (bytevector->pointer main offset)))
                (loop (+ count 1)
                      (cons (dereference-pointer next-ptr) acc))))))))

(define (pointer->remote-head-list ptr length)
   (map pointer->remote-head
        (pointer->pointer-list ptr length)))

;;; git describe options

(define %describe-options
  (bs:struct `((version ,unsigned-int)
               (max-candidates-tag ,unsigned-int)
               (describe-strategy ,unsigned-int)
               (pattern ,(bs:pointer uint8)) ;char *
               (only-follow-first-parent ,int)
               (show-commit-oid-as-fallback ,int))))

(define-record-type <describe-options>
  (%make-describe-options bytestructure)
  describe-options?
  (bytestructure describe-options-bytestructure))

(define (make-describe-options-bytestructure)
  (%make-describe-options (bytestructure %describe-options)))

(define (describe-options->pointer options)
  (bytestructure->pointer (describe-options-bytestructure options)))

(define (set-describe-options-max-candidates-tag! options max-candidates)
  (bytestructure-set! (describe-options-bytestructure options)
                      'max-candidates-tag max-candidates))

(define (set-describe-options-strategy! options strategy)
  (bytestructure-set! (describe-options-bytestructure options)
                      'describe-strategy strategy))

(define (set-describe-options-pattern! options pattern)
  (bytestructure-set! (describe-options-bytestructure options)
                      'pattern (pointer-address pattern)))

(define (set-describe-options-only-follow-first-parent! options only-follow-first-parent?)
  (bytestructure-set! (describe-options-bytestructure options)
                      'only-follow-first-parent only-follow-first-parent?))

(define (set-describe-options-show-commit-oid-as-fallback! options fallback-to-oid?)
  (bytestructure-set! (describe-options-bytestructure options)
                      'show-commit-oid-as-fallback fallback-to-oid?))

;;; git describe format options

(define %describe-format-options
  (bs:struct `((version ,unsigned-int)
               (abbreviated-size ,unsigned-int)
               (always-use-long-format ,int)
               (dirty-suffix ,(bs:pointer uint8))))) ;char *

(define-record-type <describe-format-options>
  (%make-describe-format-options bytestructure)
  describe-format-options?
  (bytestructure describe-format-options-bytestructure))

(define (describe-format-options->pointer format-options)
  (bytestructure->pointer (describe-format-options-bytestructure
                           format-options)))

(define (make-describe-format-options-bytestructure)
  (%make-describe-format-options (bytestructure %describe-format-options)))

(define (set-describe-format-options-abbreviated-size! format-options abbreviated-size)
  (bytestructure-set! (describe-format-options-bytestructure format-options)
                      'abbreviated-size abbreviated-size))

(define (set-describe-format-options-always-use-long-format! format-options
                                                             always-use-long-format?)
  (bytestructure-set! (describe-format-options-bytestructure format-options)
                      'always-use-long-format always-use-long-format?))

(define (set-describe-format-options-dirty-suffix! format-options dirty-suffix)
  (bytestructure-set! (describe-format-options-bytestructure format-options)
                      'dirty-suffix (pointer-address dirty-suffix)))
