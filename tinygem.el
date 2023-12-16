;; -*- lexical-binding:t -*-

;;; tinygem.el --- Emacs Lisp packages built directly from source

;; Copyright 2023, Alexandre Avanian

;; Author: Alexandre Avanian
;; URL: https://github.com/aavanian/tinygem.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Helper functions to interact with TinyGem.org

;; See the README for more info:
;; https://github.com/aavanian/tinygem.el/blob/main/README.org

;;; Requirements:

;;; Code:

(require 'request)

;; --- customs / variables ---------------------------------------------------
(defcustom tinygem/token-source 'auth-source
  "Source of the token for the HTTP request.
Can be a string (the token itself) or the symbol 'auth-source
(which means it should use the auth-source package to find the token)."
  :type '(choice (string :tag "Token")
                 (const :tag "Use auth-source package" auth-source))
  :group 'tinygem)

(defcustom tinygem/user nil
  "TinyGem user to search for in auth-source. If nil, it will use the first
entry for tinygem.org."
  :type '(choice (nil :tag "First entry")
                 (string :tag "Username to search in auth-source"))
  :group 'tinygem)

;; --- helpers ---------------------------------------------------------------
(defun tinygem/get-token (&optional host user)
  "Get the token depending on the value of 'tinygem/token-source'.
If 'tinygem/token-source' is a string, return that string.
If 'tinygem/token-source' is the symbol 'auth-source', search auth-source
for the optional arguments HOST and USER and return the secret.
Return an error if it can't find an entry or finds several."
  (cond
   ((stringp tinygem/token-source) tinygem/token-source)
   ((eq tinygem/token-source 'auth-source)
    (let* ((host (or host "tinygem.org"))
           (user (or user t))
           (found (auth-source-search :host host :user user :max 1)))
      (if found
          (let ((first-found (car found)))
            (or (funcall (plist-get first-found :secret))
                (error "Found auth-source entry, but it has no secret")))
        (error "No auth-source entry found for host %s and user %s" host user))))
   (t
    (error "Invalid token source: %s" tinygem/token-source))))

;; --- main functions-- ------------------------------------------------------

;;;###autoload
(defun create-tinygem (url title &optional is_private tags note)
  "Create a TinyGem for URL.

IS_PRIVATE may be t or nil.

TITLE is a string.

TAGS may be a string of comma-separated list of tags.

NOTE may be a string."
  (let* ((is_private (or is_private nil))
         (data `(("apikey" . ,(tinygem/get-token))
                 ("is_private" . ,(if is_private "true" "false"))
                 ("url" . ,url)
                 ("title" . ,title)))
         (output))
    (when tags
      (setq data (cons data `("tags" . ,tags))))
    (when note
      (setq data (cons data `("note" . ,note))))
    (request
      "https://tinygem.org/api/create/"
      :type "POST"
      :data data
      :parser 'json-read
      :status-code
      `((201 . ,(lambda (&rest _)
                  (setq output (format "TinyGem successfully created for %s." url))))
        (400 . ,(cl-function (lambda (&key data &key response &allow-other-keys)
                               (message "Error 400: %s when trying to create TinyGem for %s." data url))))
        (500 . ,(cl-function (lambda (&key data &allow-other-keys)
                               (setq output (format "Error 500: %s when trying to create TinyGem for %s." data url))))))) output))

(provide 'tinygem)

;;; tinygem.el ends here
