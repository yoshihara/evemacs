(defvar log-file-path "~")

(defvar notebook-name nil)

(defvar evernote-token nil)

(defvar evemacs-info-file (expand-file-name "~/.evemacs.gpg"))

(defun shell-executable-string(shell-string)
  (concat "\"" shell-string "\"")
)

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

(defun evemacs-command(message notebook)
  (if (null notebook)
      (concat el-path "bin/evemacs"
              " -m " (shell-executable-string message)
              " -t " (shell-executable-string evernote-token))
      (concat el-path "bin/evemacs"
              " -m " (shell-executable-string message)
              " -n " (shell-executable-string notebook)
              " -t " (shell-executable-string evernote-token))))

(defun write-comment(message)
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

(global-set-key "\C-ch" 'write-comment)


;; ここは別にすること（2013/01からの半年の目標用）
(defun write-tension(string reason)
  (interactive "sテンション:\nsその理由:")
  (setq encode-string (encode-coding-string string 'utf-8))
  (setq encode-reason (encode-coding-string reason 'utf-8))
  (start-process-shell-command "write-tension" "*Messages*"
                               (ruby-script-path "write-tension.rb") encode-string encode-reason log-file-path)
)
(global-set-key (kbd "ESC M-h") 'write-tension)

(provide 'evemacs)

;; (defun date()
;;   (format-time-string "%y%m%d" (current-time))
;; )

;; (defun time()
;;   (format-time-string "%H:%M:%S" (current-time))
;; )

;;  (setq spent_second (+ (* (car second) (expt 2 16)) (cadr second)))

;;  2回呼ぶときの  (interactive "BBuffer test:\nsString:")

;; 文字列連結とフォーマット
;; (setq insert-message (format "%s\n%s\n\n" encode-string (time)))

;; バッファへの書き込み
;;  (write-region insert-message nil comment-filename t)

;; バッファ経由する場合
;; (defun buffer-input(string)
;;   (interactive "sMessage:")
;;   (with-temp-buffer
;;     (insert string "\n")
;;     (insert (time))
;;     (write-file (format "%s.txt" (date)))
;;     )
;; )
