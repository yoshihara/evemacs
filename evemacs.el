(defvar log-file-path "~")

(defvar notebook-name nil)

(defvar evernote-token nil)

(defvar evemacs-info-file (expand-file-name "~/.evemacs.gpg"))

(setq el-path (file-name-directory (or load-file-name (buffer-file-name))))

(defun evemacs ()
  (interactive)
  "Init evemacs. If ~/.evemacs.gpg exists, it reads it and token in it stores to evemacs-token. If doesn't, it runs authorize app (with sinatra) and waits your input as token."
  (if (not (null evernote-token)) (message "evemacs is already initialized.")
  (cond ((file-exists-p evemacs-info-file) (evemacs-load-info-file))
        (t
         (when (y-or-n-p "Authorize Evernote? (Using 'browse-url')")
           (let ((authorize_command (concat "ruby " el-path "bin/authorize_evernote")))
             (start-process-shell-command "authorize-evernote" "*Messages*"
                                          authorize_command))
           (sleep-for 2)
           (browse-url "http://localhost:4567")
           (setq evernote-token (read-string "Your token?: "))
           (write-region evernote-token nil evemacs-info-file nil nil)
           (kill-process "authorize-evernote"))))))

(defun evemacs-load-info-file ()
  (epa-file-disable)
  (with-temp-buffer
    (insert-file-contents evemacs-info-file)
    (setq evernote-token (buffer-string)))
)

(defun shell-executable-string(shell-string)
  (concat "\"" shell-string "\"")
)

(defun evemacs-command(message notebook)
  (if (null notebook)
      (concat el-path "bin/evemacs"
              " -m " (shell-executable-string message)
              " -t " (shell-executable-string evernote-token))
      (concat el-path "bin/evemacs"
              " -m " (shell-executable-string message)
              " -n " (shell-executable-string notebook)
              " -t " (shell-executable-string evernote-token))))

(defun evemacs-send-message(message)
  (interactive "sMessage:")
  (if (null evernote-token)
      (evemacs))
  (start-process-shell-command "send-to-evernote" "*Messages*"
                               (evemacs-command message notebook-name))
  (set-process-sentinel
   (get-process "send-to-evernote")
   '(lambda (process signal)
      (if (string= signal "finished\n")
          (message "Finished.")))))

(provide 'evemacs)
