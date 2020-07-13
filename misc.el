;; backup files
(setq backup-directory-alist `(("." . "~/.backups")))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; shell script indentation
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; kubernetes hacks for ladders
(setenv "KUBERNETES_SERVICE_HOST" "true")
(setenv "KUROSAWA_S3_CONFIG_URI" "s3://ladders-config/local/")
(setenv "CLJ_CONFIG" "/home/mattk/dev/ladders/services")

;;
;; switch java
;;
(setq JAVA_BASE "/usr/lib/jvm")

;;
;; This function returns the list of installed
;;
(defun switch-java--versions ()
  "Return the list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a ".") (equal a "..")))
   (directory-files JAVA_BASE)))

(defun switch-java--save-env ()
  "Store original PATH and JAVA_HOME."
  (when (not (boundp 'SW_JAVA_PATH))
    (setq SW_JAVA_PATH (getenv "PATH")))
  (when (not (boundp 'SW_JAVA_HOME))
    (setq SW_JAVA_HOME (getenv "JAVA_HOME"))))

(defun switch-java ()
  "List the installed JDKs and enable to switch the JDK in use."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  (let ((ver (completing-read
              "Which Java: "
              (seq-map-indexed
               (lambda (e i) (list e i)) (switch-java--versions))
              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (concat JAVA_BASE "/" ver))
    (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin")
                           ":" SW_JAVA_PATH)))
  ;; show version
  (switch-java-which-version?))

(defun switch-java-default ()
  "Restore the default Java version."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  ;; switch java version
  (setenv "JAVA_HOME" SW_JAVA_HOME)
  (setenv "PATH" SW_JAVA_PATH)
  ;; show version
  (switch-java-which-version?))

(defun switch-java-which-version? ()
  "Display the current version selected Java version."
  (interactive)
  ;; displays current java version
  (message (concat "Java HOME: " (getenv "JAVA_HOME"))))
