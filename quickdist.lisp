(in-package quickdist)

(defparameter *distinfo-template*
  "name: {{name}}
version: {{version}}
distinfo-subscription-url: {{base-url}}/{{name}}.txt
distinfo-template-url: {{base-url}}/{{name}}/{{=<< >>=}}{{version}}<<={{ }}=>>/distinfo.txt
release-index-url: {{base-url}}/{{name}}/{{version}}/releases.txt
system-index-url: {{base-url}}/{{name}}/{{version}}/systems.txt
")
(defparameter *distinfo-file-template* "{{dists-dir}}/{{name}}.txt")
(defparameter *dist-dir-template*      "{{dists-dir}}/{{name}}/{{version}}")
(defparameter *archive-dir-template*   "{{dists-dir}}/{{name}}/archive")
(defparameter *archive-url-template*   "{{base-url}}/{{name}}/archive")

(defparameter *gnutar*
  #+os-macosx "/usr/local/bin/gtar"
  #-os-macosx "/bin/tar"
  "Location of the GNU TAR program")

(defparameter *project-path* nil
  "During building of the distribution, this special variable will point to a currently processed project.")


(defparameter *blacklisted-systems* nil
  "A list of globally blacklisted systems. To block systems on per-project basis, pass a black-alist
   argument to quickdist function.")

(defparameter *blacklisted-dependencies*
  ;; Some projects specify an implementation dependent dependencies
  ;; like here: https://github.com/fukamachi/quri/blob/76b75103f21ead092c9f715512fa82441ef61185/quri.asd#L23
  ;; and we need to exclude them from the distribution.
  #+sbcl
  (list :sb-aclrepl
        :sb-bsd-sockets
        :sb-capstone
        :sb-cltl2
        :sb-concurrency
        :sb-cover
        :sb-executable
        :sb-gmp
        :sb-grovel
        :sb-introspect
        :sb-md5
        :sb-mpfr
        :sb-posix
        :sb-queue
        :sb-rotate-byte
        :sb-rt
        :sb-simple-streams
        :sb-sprof)
  #-sbcl
  nil
  "A list of globally blacklisted systems. To block systems on per-project basis, pass a black-alist
   argument to quickdist function.")


(defclass system-info ()
  ((path :initarg :path
         :reader get-path)
   (project-name :initarg :project-name
                 :reader get-project-name)
   (filename :initarg :filename
             :reader get-filename)
   (name :initarg :name
         :reader get-name)
   (dependencies :initarg :dependencies
                 :reader get-dependencies)))


(defclass release-info ()
  ((project-name :initarg :project-name
                 :reader get-project-name)
   (project-url :initarg :project-url
                :reader get-project-url)
   (archive-path :initarg :archive-path
                 :documentation "A path on the local filesystem where archive is located."
                 :reader get-archive-path)
   (file-size :initarg :file-size
              :reader get-file-size)
   (md5sum :initarg :md5sum
           :reader get-md5sum)
   (content-sha1 :initarg :content-sha1
                 :reader get-content-sha1)
   (project-prefix :initarg :project-prefix
                   :reader get-project-prefix)
   (system-files :initarg :system-files
                 :reader get-system-files)))


(defmacro def-print-method (((obj class)) format-string &body body)
  "Defines a print-object method which uses 'print-unreadable-object only when *print-pretty* is True."
  (let ((plain-format-string (concatenate 'string format-string "~%")))
    `(defmethod print-object ((,obj ,class) stream)
       (if *print-pretty*
           (print-unreadable-object (,obj stream :type t)
             (format stream ,format-string
                     ,@body))
           (format stream ,plain-format-string
                   ,@body)))))


(def-print-method ((obj system-info))
                  "~A ~A ~A~{ ~A~}"
  (get-project-name obj)
  (get-filename obj)
  (get-name obj)
  (get-dependencies obj))


(def-print-method ((obj release-info))
                  "~A ~A ~A ~A ~A ~A~{ ~A~}"
  (get-project-name obj)
  (get-project-url obj)
  (get-file-size obj)
  (get-md5sum obj)
  (get-content-sha1 obj)
  (get-project-prefix obj)
  (get-system-files obj))


(defun render-template (template data)
  (mustache:render* template
                    (alexandria:plist-alist data)))

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
  (flet ((system-name->filename (name)
           (concatenate 'string name ".asd")))
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


(defcached get-external-dependencies (system-name)
  "Returns direct external dependencies for the system.

   If system is of package inferred class, then
   this function will go through all it's internal components recursively
   and collects their external dependencies.

   For external dependendencies, which are subcomponents of other package
   inferred system, a name of a primary system is returned.

   Resulting value is a list of lists where first item is a system-name or it's
   subsystem's name and the reset is their direct depdencies names sorted alphabetically."
  (check-type system-name string)
  (log:debug "Retrieving external dependencies for" system-name)

  (let ((already-collected (make-hash-table :test 'equal)))
    (labels ((recursive-get-external-dependencies (system-name)
               (let* ((system (asdf:find-system system-name))
                      (primary-name (asdf:primary-system-name system))
                      (defsystem-dependencies (asdf:system-defsystem-depends-on system))
                      (usual-dependencies (asdf:system-depends-on system))
                      (all-direct-dependencies (nconc defsystem-dependencies
                                                      usual-dependencies))
                      (normalized-deps (mapcar #'normalize-dependency-name
                                               all-direct-dependencies))
                      (deps (delete-duplicates normalized-deps
                                               :test 'string-equal))
                      (subsystems (loop for dep in deps
                                        for dep-primary-name = (asdf:primary-system-name dep)
                                        ;; We only expand inner component of the current
                                        ;; system, because for quicklisp distribution we
                                        ;; need to specify only direct dependencies
                                        when (and (not (gethash dep already-collected))
                                                  (string-equal primary-name
                                                                dep-primary-name))
                                          do (setf (gethash dep already-collected)
                                                   t)
                                          and appending (recursive-get-external-dependencies dep))))
                 (list* (list* system-name deps)
                        subsystems))))
      (sort (recursive-get-external-dependencies system-name)
            #'string<
            :key #'first))))


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
    (let* ((systems-before '("asdf" "uiop"))
           (loaded-system-name (pathname-name asd-path))
           (asdf/system-registry:*registered-systems*
             (copy-hash-table-partially
              asdf/system-registry:*registered-systems*
              :keys systems-before)))
      (tagbody
       asd-loader
         (handler-case (asdf:load-asd asd-path)
           (asdf:missing-dependency (condition)
             (let ((missing-system-name (asdf/find-component:missing-requires condition)))
               #+quicklisp
               (progn
                 ;; We need this to process dependencies from :defsystem-depends-on
                 ;; argument of the `defsystem', like
                 (log:info "Loading a missing dependency" missing-system-name)
                 (ql:quickload missing-system-name)
                 (log:info "Restarting to load asd file again" asd-path)
                 (go asd-loader))
               #-quicklisp
               (log:error "Unable to a missing dependency because quicklisp is unavailable" missing-system-name)))))
     
      (flet ((was-loaded-before (system-name)
               (member system-name
                       systems-before
                       :test #'string-equal)))
        (log:debug "Collecting dependencies" asd-path)
        (let ((dependencies (loop for system-name in (remove-if #'was-loaded-before (asdf:registered-systems))
                                  for primary-name = (asdf:primary-system-name system-name)
                                  when (string-equal primary-name
                                                     loaded-system-name)
                                    do (log:info "Dependencies for" system-name "are collected")
                                    and appending (get-external-dependencies system-name))))
          (log:debug "Dependencies are collected")
          (sort dependencies
                #'string-lessp
                :key #'first))))))


(defun unix-filename (path)
  (format nil "~a.~a" (pathname-name path) (pathname-type path)))


(defun unix-filename-relative-to (base path)
  (let ((base-name (native-namestring (truename base)))
        (path-name (native-namestring (truename path))))
    (subseq path-name (mismatch base-name path-name))))


(defun get-blacklist-info (project-name black-alist)
  (let ((project-string (stringify project-name)))
    (when-let ((blacklisted (assoc project-string
                                   black-alist
                                   :test #'string-equal)))
      (rest blacklisted))))


(defun get-blacklisted-systems (project-name black-alist)
  (append
   (getf (get-blacklist-info project-name black-alist)
         :systems)
   *blacklisted-systems*))


(defun get-blacklisted-dependencies (project-name black-alist)
  (append
   (getf (get-blacklist-info project-name black-alist)
         :dependencies)
   *blacklisted-dependencies*))


(defun blacklisted-system-p (project-name system-name black-alist)
  (find (stringify system-name)
        (get-blacklisted-systems project-name black-alist)
        :test #'string-equal ))


(defun filter-blacklisted-dependencies (project-name black-alist dependencies)
  (let ((blacklisted (get-blacklisted-dependencies project-name black-alist)))
    (remove-if (lambda (item)
                 (member item blacklisted :test #'string-equal))
               dependencies)))


(defun make-archive (project-path project-name system-files archive-path archive-url)
  (let* ((tgz-path (archive archive-path project-path))
        (project-prefix (pathname-name tgz-path))
        (project-url (format nil "~a/~a" archive-url (unix-filename tgz-path))))
    (make-instance 'release-info
                   :project-name project-name
                   :project-url project-url
                   :archive-path tgz-path
                   :file-size (file-size tgz-path)
                   :md5sum (md5sum tgz-path)
                   :content-sha1 (tar-content-sha1 tgz-path)
                   :project-prefix project-prefix
                   :system-files system-files)))


(defun make-systems-info (project-path &key black-alist)
  (let* ((project-name (last-directory project-path))
         (system-files (find-system-files project-path
                                          (get-blacklisted-systems project-name
                                                                   black-alist))))
    (cond ((not system-files)
           (log:warn "No .asd files found in" project-path))
          (t
           (with-simple-restart (skip-project "Skip project ~S, continue with the next."
                                              project-path)
             (log:info "Processing make-systems-info" project-name)

             (loop with *print-case* = :downcase
                   with systems-info = nil
                   for system-file in system-files
                   for relative-system-file = (unix-filename-relative-to project-path
                                                                         system-file)
                   for filename = (pathname-name system-file)
                   do (loop for name-and-dependencies in (get-systems system-file)
                            for name = (first name-and-dependencies)
                            for dependencies = (rest name-and-dependencies)
                            for filtered-dependencies = (filter-blacklisted-dependencies project-name
                                                                                         black-alist
                                                                                         dependencies)
                            unless (blacklisted-system-p project-name filename black-alist)
                              do (push (make-instance 'system-info
                                                      :path system-file
                                                      :project-name project-name
                                                      :filename relative-system-file
                                                      :name name
                                                      :dependencies filtered-dependencies)
                                       systems-info))
                   finally (return systems-info)))))))


(defmethod get-system-files ((systems-info list))
  (mapcar #'get-filename systems-info))


(defun create-dist (projects-path dist-path archive-path archive-url black-alist)
  ;; Here we need to add an additional slash to the end of the path
  ;; to the sources, to make ASDF search recursively for available systems
  (let ((registry-path (concatenate 'string (native-namestring projects-path) "/")))
    (asdf:initialize-source-registry registry-path))
  
  (with-open-file (release-index (make-pathname :name "releases" :type "txt" :defaults dist-path)
                                 :direction :output :if-exists :supersede)
    (write-line "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]" release-index)
    (with-open-file (system-index (make-pathname :name "systems" :type "txt" :defaults dist-path)
                                  :direction :output :if-exists :supersede)
      (write-line "# project system-file system-name [dependency1..dependencyN]" system-index)
      (dolist (*project-path* (fad:list-directory projects-path))
        (when (fad:directory-pathname-p *project-path*)
          (let* ((project-name (last-directory *project-path*)))
            (log:info "Processing create-dist" project-name)
            
            (with-simple-restart (skip-project "Skip project ~S, continue with the next."
                                               *project-path*)
              (let* ((systems-info (make-systems-info *project-path* :black-alist black-alist))
                     (release-info (make-archive *project-path*
                                                 project-name
                                                 (get-system-files systems-info)
                                                 archive-path
                                                 archive-url)))
                (write release-info
                       :stream release-index
                       :pretty nil)
                (loop for system-info in systems-info
                      do (write system-info
                                :stream system-index
                                :pretty nil))))))))))


(defun quickdist (&key name (version :today) base-url projects-dir dists-dir black-alist)
  "Build a distribution.

   black-alist is a list of the following form:
   (list (list \"quri\" :systems (list \"quri-examples\")
                        :dependencies (list \"foo-bar\")))

   Some systems and dependencies are ignored by default, you'll find their list in the
   *blacklisted-systems* and *blacklisted-dependencies* variables."
  (let* ((version (if (not (eq version :today)) version (format-date (get-universal-time))))
         (projects-path (fad:pathname-as-directory (probe-file projects-dir)))
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
                 black-alist)
    (let ((distinfo (render-template *distinfo-template* template-data)))
      (dolist (path (list (make-pathname :name "distinfo" :type "txt" :defaults dist-path)
                          distinfo-path))
        (write-string-into-file distinfo path :if-exists :supersede))

      (values distinfo-path))))
