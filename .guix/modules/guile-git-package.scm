;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guile-git-package)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix gexp)
  #:use-module ((guix git-download) #:select (git-predicate))
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define %srcdir
  (string-append (dirname (current-filename)) "/../.."))

(define-public guile-git
  (package
    (name "guile-git")
    (version "0.1")
    (source (local-file %srcdir "guile-git-checkout"
                        #:recursive? #t
                        #:select? (git-predicate %srcdir)))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           pkg-config
           texinfo
           openssh
           git))
    (inputs
     (list guile-3.0 libgit2 openssl zlib guile-bytestructures))
    (synopsis "Guile bindings for libgit2")
    (description
     "This package provides Guile bindings to libgit2, a library to
manipulate repositories of the Git version control system.")
    (home-page "https://gitlab.com/guile-git/guile-git.git")
    (license gpl3+)))

guile-git
