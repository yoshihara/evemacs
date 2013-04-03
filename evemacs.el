(defvar evemacs-log-file-path "~")

(defvar evemacs-notebook-name nil)

(defvar evemacs-evernote-token nil)

(defvar evemacs-info-file (expand-file-name "~/.evemacs.gpg"))

(setq evemacs-el-path
      (file-name-directory (or load-file-name (buffer-file-name))))

(defun evemacs ()
  (interactive)
  "Init evemacs. If ~/.evemacs.gpg exists, it reads it and token in it stores to evemacs-token. If doesn't, it runs authorize app (with sinatra) and waits your input as token."
  (if (not (null evemacsevernote-token))
      (message "evemacs is already initialized.")
  (cond ((file-exists-p evemacs-info-file) (evemacs-load-info-file))
        (t
         (when (y-or-n-p "Authorize Evernote? (Using 'browse-url')")
           (let ((authorize_command (concat "ruby " evemacs-el-path "bin/authorize_evernote")))
             (start-process-shell-command "authorize-evernote" "*Messages*"
                                          authorize_command))
           (sleep-for 2)
           (browse-url "http://localhost:4567")
           (setq evemacs-evernote-token (read-string "Your token?: "))
           (write-region evemacs-evernote-token nil evemacs-info-file nil nil)
           (kill-process "authorize-evernote"))))))

(defun evemacs-init ()
  (evemacs))

(defun evemacs-load-info-file ()
  (epa-file-disable)
  (with-temp-buffer
    (insert-file-contents evemacs-info-file)
    (setq evemacs-evernote-token (buffer-string)))
)

(defun evemacs-shell-executable-string(shell-string)
  (concat "\"" shell-string "\"")
)

(defun evemacs-command(message notebook)
  (if (null notebook)
      (concat evemacs-el-path "bin/evemacs"
              " -m " (evemacs-shell-executable-string message)
              " -t " (evemacs-shell-executable-string evemacs-evernote-token))
      (concat evemacs-el-path "bin/evemacs"
              " -m " (evemacs-shell-executable-string message)
              " -n " (evemacs-shell-executable-string notebook)
              " -t " (evemacs-shell-executable-string evemacs-evernote-token))))

(defun evemacs-send-message(message)
  (interactive "sMessage:")
  (if (null evemacs-evernote-token)
      (evemacs-init))
  (start-process-shell-command "send-to-evernote" "*Messages*"
                               (evemacs-command message evemacs-notebook-name))
  (set-process-sentinel
   (get-process "send-to-evernote")
   '(lambda (process signal)
      (if (string= signal "finished\n")
          (message "Finished.")))))

(provide 'evemacs)
