;; -*- coding: utf-8 -*-
;;
;; Copyright (C) 2013  Haruka Yoshihara <yshr04hrk@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'ert)
(setq default-directory (expand-file-name (file-relative-name  "../")))
(add-to-list 'load-path default-directory)
(require 'evemacs)

(setq evernote-token "evernote-token")
(setq evemacs-evernote-token evernote-token)

(ert-deftest evemacs:evemacs-command-with-notebook ()
  (let ((message "This is message!")
        (notebook "evemacs-notebook"))
    (setq expected-command
          (format "%sbin/evemacs -m \"%s\" -n \"%s\" -t \"%s\""
                  default-directory message notebook evernote-token))
    (should (equal expected-command (evemacs-command message notebook)))))

(ert-deftest evemacs:evemacs-command-without-notebook ()
  (let ((message "This is message!")
        (notebook nil))
    (setq expected-command
          (format "%sbin/evemacs -m \"%s\" -t \"%s\""
                  default-directory message evernote-token))
    (should (equal expected-command (evemacs-command message notebook)))))

(ert-run-tests-batch-and-exit)
