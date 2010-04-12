(declare (usual-integrations))

(load-option 'sos)
(with-loader-base-uri (system-library-uri "pstore/")
  (lambda ()
    (load-package-set "pstore")))

(add-subsystem-identification! "PStore" '(0 9))
		      