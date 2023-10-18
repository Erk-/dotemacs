(require 'transient)

;; PUBLIC ;;

(defvar erk/cargo-profiles '("dev" "release" "test" "bench")
  "List of possible cargo profiles")

(defvar erk/rust-versions '("stable" "nightly")
  "list of rust versions to use")

;; PUBLIC END ;;

;;;###autoload (autoload 'erk/rust-tools "erk/rust-tools" nil t)
(transient-define-prefix erk/rust-tools ()
  "Run various Rust commands to aid development."
  ["Flags"
   (erk/rustc-version)
   (erk/cargo-profile)]
  ["Commands"
   ("e" "emacs test" erk/cargo)
   ("b" "build"   (lambda () (interactive) (erk/cargo "build")))
   ("c" "check"   (lambda () (interactive) (erk/cargo "check")))
   ("t" "test"    (lambda () (interactive) (erk/cargo "test")))])

(defun erk/get-arg (name)
  (transient-arg-value
   name
   (transient-args transient-current-command)))


(defun erk/cargo (command)
  "Command for running nextest."
  (interactive "P")
  (let* ((version (erk/get-var 'erk/p--rust-version))
         (buf "erk/cargo-buffer")
         (proc "erk/cargo-process")
         (profile (erk/get-var 'erk/p--cargo-profile))
         (profile-fmt (if profile '("--profile" profile) nil))
         (mode (pcase command
                 ("build" 'rustic-compilation-mode)
                 ("check" 'rustic-compilation-mode)
                 ("test"  'rustic-cargo-test-mode)
                 (_       'rustic-compilation-mode)))
         (c (remq nil (-flatten (list (rustic-cargo-bin) version command profile-fmt)))))
    (message (format "profile: %S" profile))
    (message (format "profile-fmt: %S" profile-fmt))
    (message (format "c: %S" c))
    (rustic-compilation c (list :buffer buf :process proc :mode mode))))

(defun erk/get-var (var)
  (setq-local tmp (symbol-value var))
  (set var nil)
  tmp)

(defclass erk/switch-alt (transient-switches)
  ((variable :initarg :variable)
   (set-value :initarg :set-value :initform #'set))
  "")

(cl-defmethod transient-infix-set ((obj erk/switch-alt) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)))


(defvar erk/p--cargo-profile nil)

(transient-define-argument erk/cargo-profile ()
  :argument "erk/cargo-profile="
  :description "Cargo profile for build"
  :variable 'erk/p--cargo-profile
  :class 'erk/switch-alt
  :key "p"
  :argument-format "--profile %s"
  :argument-regexp (regexp-opt erk/cargo-profiles)
  :choices erk/cargo-profiles)

(defvar erk/p--rust-version nil)

(transient-define-argument erk/rustc-version ()
  :argument "erk/rustc-version="
  :description "Rustc version"
  :variable 'erk/p--rust-version
  :class 'erk/switch-alt
  :key "v"
  :argument-format "+%s"
  :argument-regexp (regexp-opt erk/rust-versions)
  :choices erk/rust-versions)

(provide 'rust-transient)
