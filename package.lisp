(cl:in-package :cl-user)

(defpackage #:quickdist
  (:use #:cl #:alexandria)
  (:import-from #:quicklisp
                #:file-size)
    (:import-from #:uiop
                #:native-namestring)
  (:export #:quickdist
           #:*distinfo-template*
           #:*distinfo-file-template*
           #:*dist-dir-template*
           #:*archive-dir-template*
           #:*archive-url-template*
           #:*gnutar*))
