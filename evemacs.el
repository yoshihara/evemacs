(defvar evemacs-notebook-name nil)

(defvar evemacs-evernote-token nil)

(defvar evemacs-info-file (expand-file-name "~/.evemacs.gpg"))

(setq evemacs-el-path
      (file-name-directory (or load-file-name (buffer-file-name))))

(defun evemacs-init ()
  (interactive)
  "Init evemacs. If ~/.evemacs.gpg exists, it reads it and token in it stores to evemacs-token. If doesn't, it runs authorize app (with sinatra) and waits your input as token."
  (if (not (null evemacs-evernote-token))
      (message "evemacs is already initialized.")
  (cond ((file-exists-p evemacs-info-file) (evemacs-load-info-file))
        (t
         (when (y-or-n-p "Authorize Evernote? (Using 'browse-url')")
           (browse-url "http://authorize-evemacs.herokuapp.com")
           (let ((evemacs-input-token (read-string "Your token?: ")))
             (cond ((string= "" evemacs-input-token)
                    (message "Please input your token in your browser."))
                   (t
                    (setq evemacs-evernote-token evemacs-input-token)
                    (write-region evemacs-evernote-token nil evemacs-info-file nil nil)))))))))

(defun evemacs ()
  (evemacs-init))

(defun evemacs-load-info-file ()
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
