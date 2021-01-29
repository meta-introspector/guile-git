

(use-modules ((guix licenses) #:select (gpl3+))
             (guix gexp)
             ((guix git-download) #:select (git-predicate))
             (guix packages)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages compression)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages ssh)
             (gnu packages texinfo)
             (gnu packages tls)
             (gnu packages version-control))

(define %srcdir
  (dirname (current-filename)))

(package
  (name "guile-git")
  (version "0.1")
  (source (local-file "." "guile-git-checkout"
                      #:recursive? #t
                      #:select? (git-predicate %srcdir)))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)
     ("openssh" ,openssh)
     ("git" ,git)))
  (inputs
   `(("guile" ,guile-3.0)
     ("libgit2" ,libgit2)
     ("openssl" ,openssl)
     ("zlib" ,zlib)
     ("guile-bytestructures" ,guile-bytestructures)))
  (synopsis "Guile bindings for libgit2")
  (description "")
  (home-page "")
  (license gpl3+))
