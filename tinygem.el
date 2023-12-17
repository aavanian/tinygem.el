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

(require 'plz)

;; --- customs / variables ---------------------------------------------------
(defcustom tinygem-token-source 'auth-source
  "Source of the token for the HTTP request.
Can be a string (the token itself) or the symbol 'auth-source
(which means it should use the auth-source package to find the token)."
  :type '(choice (string :tag "Token")
                 (const :tag "Use auth-source package" auth-source))
  :group 'tinygem)

(defcustom tinygem-user nil
  "TinyGem user to search for in auth-source. If nil, it will use the first
entry for tinygem.org."
  :type '(choice (nil :tag "First entry")
                 (string :tag "Username to search in auth-source"))
  :group 'tinygem)

;; --- helpers ---------------------------------------------------------------
(defun tinygem-get-token (&optional host user)
  "Get the token depending on the value of 'tinygem-token-source'.
If 'tinygem-token-source' is a string, return that string.
If 'tinygem-token-source' is the symbol 'auth-source', search auth-source
for the optional arguments HOST and USER and return the secret.
Return an error if it can't find an entry or finds several."
  (cond
   ((stringp tinygem-token-source) tinygem-token-source)
   ((eq tinygem-token-source 'auth-source)
    (let* ((host (or host "tinygem.org"))
           (user (or user t))
           (found (auth-source-search :host host :user user :max 1)))
      (if found
          (let ((first-found (car found)))
            (or (funcall (plist-get first-found :secret))
                (error "Found auth-source entry, but it has no secret")))
        (error "No auth-source entry found for host %s and user %s" host user))))
   (t
    (error "Invalid token source: %s" tinygem-token-source))))

(defun parse-url-retrieve ()
  "Parse the buffer returned by `url-retrieve' and return the
status code and body."
  (goto-char (point-min))
  (let (status-code message)
    (when (re-search-forward
           "^HTTP/\\(1\\.0\\|1\\.1\\|2\\) \\([0-9]+\\) \\(.*\\)$" nil t)
      (setq status-code (string-to-number (match-string 2))))
    (goto-char url-http-end-of-headers)
    ;; (previous-line)
    (right-char)
    (setq message (buffer-substring-no-properties (point) (point-max))) 
    (list status-code message)))

;; --- main functions-- ------------------------------------------------------

;;;###autoload
(defun create-tinygem (url title &optional is_private tags note)
  "Create a TinyGem for URL.

IS_PRIVATE may be t or nil.

TITLE is a string.

TAGS may be a string of comma-separated list of tags.

NOTE may be a string."
  (pcase-let* ((url-request-method "POST")
               (url-request-extra-headers
                '(("Content-Type" .  "application/x-www-form-urlencoded")))
               (is_private (or is_private nil))
               (data `(("apikey" ,(tinygem-get-token))
                       ("is_private" ,(if is_private "true" "false"))
                       ("url" ,url)
                       ("title" ,title)))
               (data (if tags (cons data `("tags" ,tags)) data))
               (data (if note (cons data `("note" ,note)) data))
               (url-request-data (url-build-query-string data)))
    (url-retrieve "https://tinygem.org/api/create" 
                  (lambda (alist)
                    (pcase-let
                        ((`(,status-code ,resp) (parse-url-retrieve))
                         (fdb (format "TinyGem for %s" url)))
                      (cond ((eq status-code 201)
                             (message (concat fdb " successfully created.")))
                            ((eq status-code 400)
                             (message (concat fdb (format
                                                   " failed with error %s: %s."
                                                   status-code resp))))
                            (t
                             (message (concat fdb (format
                                                   ": unknown condition (%s, %s)"
                                                   status-code resp))))))))))
  
(provide 'tinygem)

;;; tinygem.el ends here
