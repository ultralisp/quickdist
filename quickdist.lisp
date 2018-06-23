(in-package #:quickdist)

(defparameter *distinfo-template*
  "name: {name}
version: {version}
distinfo-subscription-url: {base-url}/{name}.txt
release-index-url: {base-url}/{name}/{version}/releases.txt
system-index-url: {base-url}/{name}/{version}/systems.txt
")
(defparameter *distinfo-file-template* "{dists-dir}/{name}.txt")
(defparameter *dist-dir-template*      "{dists-dir}/{name}/{version}")
(defparameter *archive-dir-template*   "{dists-dir}/{name}/archive")
(defparameter *archive-url-template*   "{base-url}/{name}/archive")

(defparameter *gnutar*
  #+os-macosx "/usr/local/bin/gtar"
  #-os-macosx "/bin/tar"
  "Location of the GNU TAR program")

(defvar *template-readtable*
  (let ((readtable (copy-readtable)))
    (set-syntax-from-char #\} #\) readtable)
    readtable))

(defun read-template-form (stream)
  (let ((*readtable* *template-readtable*)
        (*package* (symbol-package :keyword)))
    (read-delimited-list #\} stream)))

(defmacro do-character-stream ((var stream &optional result) &body body)
  `(loop for ,var = (read-char ,stream nil)
         while ,var do ,@body
         finally (return ,result)))

(defun render-template (template data)
  (with-output-to-string (out)
    (with-input-from-string (in template)
      (do-character-stream (c in)
        (if (not (char= c #\{))
            (write-char c out)
            (let ((form (read-template-form in)))
              (princ (or (getf data (car form))
                         (error "The value of {~a} is undefined." (car form)))
                     out)))))))

(defun effective-mtime (path)
  (if (not (fad:directory-pathname-p path))
      (file-write-date path)
      (apply #'max 0 (mapcar #'effective-mtime (fad:list-directory path)))))

(defun format-date (universal-time)
  (let* ((time (multiple-value-list (decode-universal-time universal-time)))
         (timestamp (reverse (subseq time 0 6))))
    (format nil "~{~2,'0d~}" timestamp)))

(defun md5sum (path)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :md5 path)))

(defun tar-content-sha1 (path)
  (let ((octets (babel-streams:with-output-to-sequence (buffer)
                  (external-program:run *gnutar* (list "-xOf" path) :output buffer))))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha1 (copy-seq octets)))))

(defun last-directory (path)
  (first (last (pathname-directory path))))

(defun native-namestring (path)
  #+ccl(ccl:native-translated-namestring path)
  #+sbcl(sb-ext:native-namestring path)
  #-(or ccl sbcl)(namestring path))

(defun archive (destdir-path source-path)
  (let* ((mtime (format-date (effective-mtime source-path)))
         (name (format nil "~a-~a" (last-directory source-path) mtime))
         (out-path (make-pathname :name name :type "tgz" :defaults (truename destdir-path))))
    (external-program:run *gnutar* (list "-C" (native-namestring source-path) "."
                                           "-czf" (native-namestring out-path)
                                           "--transform" (format nil "s#^.#~a#" name))
                          :output *standard-output* :error *error-output*)
    out-path))


(defun make-distignore-predicate (path)
  (if-let ((distignore-file (probe-file (fad:merge-pathnames-as-file path ".distignore"))))
    (flet ((trim-string (string)
             (string-trim '(#\Tab #\Space #\Newline) string)))
      (let* ((regexes (split-sequence:split-sequence #\Newline
                                                     (read-file-into-string distignore-file)))
             (scanners (mapcar #'ppcre:create-scanner (mapcar #'trim-string regexes))))
        (lambda (string)
          (let ((path (native-namestring path))
                (string (native-namestring string)))
            (when (starts-with-subseq path string)
              (let ((subpath (enough-namestring string path)))
                (loop for scanner in scanners
                        thereis (ppcre:scan scanner subpath))))))))
    (constantly nil)))


(defun find-system-files (path black-list)
  (flet ((system-name->filename (name) (concatenate 'string name ".asd")))
    (let ((system-files nil)
          (blacklisted-filenames (mapcar #'system-name->filename black-list))
          (distignoredp (make-distignore-predicate path)))
      (flet ((add-system-file (path) (push path system-files))

             (asd-file-p (path) (and (string-equal "asd" (pathname-type path))
                                     (not (find (file-namestring path) blacklisted-filenames
                                                :test #'equalp))
                                     (not (funcall distignoredp path)))))
        (fad:walk-directory path #'add-system-file :test #'asd-file-p))
      (sort system-files #'string< :key #'pathname-name))))


(defun asdf-dependency-name (form)
  (if (and (listp form) (eq :feature (car form)))
      (asdf-dependency-name (third form))
      (cond
        ((and (listp form) (eq :version (first form)))
         (second form))
        (t form))))


(defun stringify (value)
  (format nil "~(~A~)" value))


(defun stringify-list (list)
  (mapcar #'stringify list))


(defun normalize-dependency-name (dep)
  "Sometimes dependency may be specified as a list according to this piece of ASDF documentation:

dependency-def := simple-component-name
               | ( :feature feature-expression dependency-def )
                 # (see Feature dependencies)
               | ( :version simple-component-name version-specifier )
               | ( :require module-name )
"
  (string-downcase (if (listp dep)
                       (case (first dep)
                         (:feature (third dep))
                         (:version (second dep))
                         (otherwise (error "Dependencies like ~A are not supported yet."
                                           dep)))
                       dep)))


(defun get-external-dependencies (system-name)
  "Returns direct external dependencies for the system.

   If system is of package inferred class, then
   this function will go through all it's internal components recursively
   and collects their external dependencies.

   For external dependendencies, which are subcomponents of other package
   inferred system, a name of a primary system is returned.

   Resulting value is a list of strings of systems names sorted alphabetically."
  (check-type system-name string)
  (let* ((system (asdf:find-system system-name))
         (primary-name (asdf:primary-system-name system))
         (defsystem-dependencies (asdf:system-defsystem-depends-on system))
         (usual-dependencies (asdf:system-depends-on system))
         (all-direct-dependencies (nconc defsystem-dependencies
                                         usual-dependencies))
         (normalized-deps (mapcar #'normalize-dependency-name
                                  all-direct-dependencies))
         (expanded-deps (loop for dep in normalized-deps
                              for dep-primary-name = (asdf:primary-system-name dep)
                              ;; We only expand inner component of the current
                              ;; system, because for quicklisp distribution we
                              ;; need to specify only direct dependencies
                              if (string-equal primary-name
                                               dep-primary-name)
                                appending (get-external-dependencies dep)
                              else
                                ;; For external dependency we need to return
                                ;; its primary system's name because
                                ;; only primary systems are listed in the quicklisp's
                                ;; metadata.
                                collect dep-primary-name)))
    (delete-duplicates (sort expanded-deps
                             #'string<)
                       :test 'string-equal)))


(defun copy-hash-table-partially (table &key keys)
  "Returns a copy of hash table TABLE, with the keys specified in :keys argument."
  (let* ((test-func (hash-table-test table))
         (copy (make-hash-table :test test-func
                                :size (hash-table-size table)
                                :rehash-size (hash-table-rehash-size table)
                                :rehash-threshold (hash-table-rehash-threshold table))))
    (maphash (lambda (k v)
               (when (member k keys :test test-func)
                 (setf (gethash k copy)
                       v)))
             table)
    copy))


(defun get-systems (asd-path)
  (check-type asd-path (or string pathname))
  (setf asd-path (fad:pathname-as-file (probe-file asd-path)))
  
  (handler-bind ((asdf:system-out-of-date
                   (lambda (c)
                     (declare (ignorable c))
                     (invoke-restart 'continue))))
    ;; Here we'll freeze systems registered by asdf
    ;; and will check which were added after the asd file was loaded
    (let* ((systems-before '("asdf"))
           (asdf/system-registry:*registered-systems*
             (copy-hash-table-partially
              asdf/system-registry:*registered-systems*
              :keys systems-before)))
      (asdf:load-asd asd-path)
     
      (flet ((was-loaded-before (system-name)
               (member system-name
                       systems-before
                       :test #'string-equal)))
        (sort (loop for system-name in (remove-if #'was-loaded-before (asdf:registered-systems))
                    for primary-name = (asdf:primary-system-name system-name)
                    for dependencies = (get-external-dependencies primary-name)
                    collect (list* (string-downcase primary-name)
                                   dependencies))
              #'string-lessp
              :key #'first)))))


(defun unix-filename (path)
  (format nil "~a.~a" (pathname-name path) (pathname-type path)))


(defun unix-filename-relative-to (base path)
  (let ((base-name (native-namestring (truename base)))
        (path-name (native-namestring (truename path))))
    (subseq path-name (mismatch base-name path-name))))


(defun blacklisted (project-name black-alist)
  (let ((project-string (stringify project-name)))
    (when-let ((blacklisted (assoc project-string black-alist :test #'equal)))
      (rest blacklisted))))


(defun blacklistedp (project-name system-name black-alist)
  (find (stringify system-name) (blacklisted project-name black-alist)))


(defun create-dist (projects-path dist-path archive-path archive-url black-alist)
  (with-open-file (release-index (make-pathname :name "releases" :type "txt" :defaults dist-path)
                                 :direction :output :if-exists :supersede)
    (write-line "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]" release-index)
    (with-open-file (system-index (make-pathname :name "systems" :type "txt" :defaults dist-path)
                                  :direction :output :if-exists :supersede)
      (write-line "# project system-file system-name [dependency1..dependencyN]" system-index)
      (dolist (project-path (fad:list-directory projects-path))
        (when (fad:directory-pathname-p project-path)
          (let* ((project-name (last-directory project-path))
                 (system-files (find-system-files project-path
                                                  (blacklisted project-name black-alist))))
            (if (not system-files)
                (warn "No .asd files found in ~a, skipping." project-path)
                (with-simple-restart (skip-project "Skip this project, continue with the next.")
                  (let* ((tgz-path (archive archive-path project-path))
                         (project-prefix (pathname-name tgz-path))
                         (project-url (format nil "~a/~a" archive-url (unix-filename tgz-path))))
                    (log:info "Processing" project-name)
                    (format release-index "~a ~a ~a ~a ~a ~a~{ ~a~}~%"
                            project-name project-url (file-size tgz-path)
                            (md5sum tgz-path) (tar-content-sha1 tgz-path) project-prefix
                            (mapcar (curry #'unix-filename-relative-to project-path)
                                    system-files))
                    (dolist (system-file system-files)
                      (dolist (name-and-dependencies (get-systems system-file))
                        (let ((*print-case* :downcase)
                              (system-name (pathname-name system-file)))
                          (unless (blacklistedp project-name system-name black-alist)
                            (format system-index "~a ~a ~a~{ ~a~}~%"
                                    project-name
                                    system-name
                                    (first name-and-dependencies)
                                    (rest name-and-dependencies)))))))))))))))


(defun quickdist (&key name (version :today) base-url projects-dir dists-dir black-alist)
  (let* ((version (if (not (eq version :today)) version (format-date (get-universal-time))))
         (projects-path (fad:pathname-as-directory projects-dir))
         (template-data (list :name name :version version
                              :base-url (string-right-trim "/" base-url)
                              :dists-dir (string-right-trim "/" (native-namestring dists-dir))))
         (distinfo-path (fad:pathname-as-file (render-template *distinfo-file-template*
                                                               template-data)))
         (dist-path (fad:pathname-as-directory (render-template *dist-dir-template*
                                                                template-data)))
         (archive-path (fad:pathname-as-directory (render-template *archive-dir-template*
                                                                   template-data)))
         (archive-url (render-template *archive-url-template* template-data)))
    (assert (fad:directory-exists-p projects-path))
    (ensure-directories-exist dist-path :verbose t)
    (ensure-directories-exist archive-path :verbose t)
    (create-dist projects-path dist-path archive-path archive-url
                 (mapcar #'stringify-list black-alist))
    (let ((distinfo (render-template *distinfo-template* template-data)))
      (dolist (path (list (make-pathname :name "distinfo" :type "txt" :defaults dist-path)
                          distinfo-path))
        (write-string-into-file distinfo path :if-exists :supersede))

      (values distinfo-path))))
