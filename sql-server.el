;;; sql-server.el --- Yet another SQL IDE                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  jtbm37

;; Author: jtbm37
;; URL: https://github.com/jtbm37/sql-server.el
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3") (swiper "0.8.0") (ctable "0.1.2") (s "1.10.0"))
;; Keywords: sql

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is still under heavy development and should not be
;; used against production sql server.

;;; Code:
(require 's)
(require 'ctable)

(setq-default sql-server-temp-buffer nil)

(defvar sql-server-temp-buffer-name " *TEMP SQL - %s*")
(defgroup sql-server nil
  "Sql Server"
  :group 'SQL)

(defcustom sql-server-confirm-delete t
  "Whether it asks to confirm execution of delete/truncate statements"
  :group 'sql-server)

(defcustom sql-server-ivy-columns-visible nil
  "Whether the table columns are currently shown in minibuffer. Dictates the available action depending on whether tables or columns are shown"
  :group 'sql-server)

(defcustom sql-server-sqlcmd "/opt/mssql-tools/bin/sqlcmd"
  "Holds the path of the `sqlcmd' executable"
  :group 'sql-server)

(setq-default sql-server-connection
  '((login . nil)
    (password . nil)
    (server . nil)
    (db . nil)
    (port . nil)))

(defconst sql-server-ivy-table-columns-query "select COLUMN_NAME, DATA_TYPE, IS_NULLABLE, COLUMN_DEFAULT, CHARACTER_MAXIMUM_LENGTH from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '%s';"
  "Defines the query to retrieve the columns of a table")

(defvar sql-server-ivy-current-table nil
  "Holds current previously selected table when on columsn view")

(defvar ivy--sql-table nil
  "Current table when completing columns")

(defvar sql-server-history nil
  "Holds history of past sql statements executed")

(defvar sql-server-tables-cache nil
  "Holds a cache of the database tables")

(defvar sql-server-sps-cache nil
  "Holds a cache of the database stored procs")

(advice-add 'sql-send-region :override #'sql-server-send-region)

;;;###autoload
(defun sql-server-set-defaults (entry)
  "Sets default sql variable using `auth-source'.
Add the following entry to your `.authinfo' file:
machine someName login `your_login' db `your_database' password `your_password' server `server_address'
"
  (interactive)
  (-if-let* ((login (plist-get entry :user))
	     (server (plist-get entry :server))
	     (db (plist-get entry :db))
	     (port (or (plist-get entry :port) 1433))
	     (secretfun (plist-get entry :secret))
	     (secret (funcall secretfun)))
      (progn
	(setq-local sql-server-connection `((login . ,login)
					    (db . ,db)
					    (port . ,port)
					    (server . ,server)
					    (password . ,secret)))
	(message "sql-server connection set"))
    (user-error "Credentials not found")))

(defun sql-server-reset-connection ()
  (sql-server-disconnect)
  (setq-local sql-server-connection '((login . nil)
				      (password . nil)
				      (server . nil)
				      (db . nil)
				      (port . nil))
	      header-line-format nil)
  (message "Connection reset"))

(defun sql-server--command-args (&optional leave-trailing-spaces)
  "Returns sqlcmd args preformatted for process call"
  (let ((args (list "-S" (format "%s,%s" (alist-get 'server sql-server-connection) (alist-get 'port sql-server-connection))
		    "-U" (alist-get 'login sql-server-connection)
		    "-P" (alist-get 'password sql-server-connection)
		    "-d" (alist-get 'db sql-server-connection)
		    "-s" "\^E"
		    "-I")))
    (unless leave-trailing-spaces (push "-W" args))
    args))

(defun sql-server-connect (&rest arg)
  "Establishes connection to database specified in `sql-server-connection'.
Call it with a prefix to create a new connection.
It will fetch connection details from `.authinfo' and prompt to choose one of
the `db' record."
  (interactive "p")

  (unless (local-variable-p 'sql-server-connection)
    (make-local-variable 'sql-server-connection))
  (unless (local-variable-p 'sql-server-temp-buffer)
    (make-local-variable 'sql-server-temp-buffer))

  (when (equal arg '(4))
    (sql-server-reset-connection))
  
  (unless (alist-get 'login sql-server-connection)
    (let ((dbs (mapcar (lambda (x) (propertize (plist-get x :db) 'value x))
		       (auth-source-search :max 50 :require '(:db)))))
      (ivy-read "Connect to: "
		dbs
		:require-match 'confirm-after-completion
		:action (lambda (x) (sql-server-set-defaults (get-text-property 1 'value x))))))
  (if-let (get-buffer-process sql-server-temp-buffer)
      (user-error "Already connected to %s." (alist-get 'db sql-server-connection))
    (let* ((db (alist-get 'db sql-server-connection))
	   (buffer-name (format sql-server-temp-buffer-name db))
	   (process (apply 'start-process ;; dino
			   "sql-server"
			   buffer-name
			   sql-server-sqlcmd
			   (sql-server--command-args))))
      (setq sql-server-temp-buffer buffer-name)
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel
       process
       (lambda (proc change)
	 (when (string-match "\\(finished\\|exited\\|exited abnormally with code\\)" change)
	   (kill-buffer (process-buffer proc))
	   (message (concat (process-name proc) " exited")))))
      (when (process-live-p process)
	(let ((db (cdr (assoc 'db sql-server-connection))))
	  (message (concat "Connected to " db))
	  (sql-server-set-header-line-format db))))))

(defun sql-server-disconnect ()
  "Disconnects from current connection associated with buffer."
  (let ((process (get-buffer-process sql-server-temp-buffer)))
    (when process
      (kill-buffer (process-buffer process)))))

(defun sql-server-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (sql-server-send (buffer-substring-no-properties start end)))

(defun sql-server-send (sql)
  "Sends string `sql' to server.
When `nocount' is t, the last line with the row count is excluded."
  (when (or (not sql-server-confirm-delete) (not (string-match "\\<delete\\>" sql)) (yes-or-no-p "Are you sure you wish to executed this query ?"))
    (let ((result (sql-server-get-result-list (sql-server-sanitize-query sql))))
      (when (> (length result) 1)
	(sql-server-add-to-history sql)
	(save-excursion (display-buffer (ctbl:create-table-buffer-easy
					   (cdr result)
					   (car result))))))))
;;;###autoload
(defun sql-server-send-buffer (&optional file)
  "Sends buffer to server.
When FILE is not set it will default to current buffer."
  (interactive)
  (unless file
    (setq file (buffer-file-name)))
  (let* ((args (-reduce (lambda (x y) (format "%s %s" x y)) (sql-server--command-args t)))
	 (cmd (format "sqlcmd %s -i \"%s\""args file)))
    (shell-command cmd)))

(defun sql-server-sanitize-query (sql)
  ;; Remove empty lines as they cause additional prompt added to the result buffer
  (replace-regexp-in-string "^[[:blank:]]*\n" " " sql))

(defun sql-server--execute-query (sql)
  "Returns a list with 3 elements:
- the beginning point of the result
- the end point of the result
- the result count"
  (let ((process (get-buffer-process sql-server-temp-buffer)))
    ;; (when (string-match "\\(.*\\);[ \t]*" sql) (setq sql (match-string 1 sql)))
    (with-current-buffer (process-buffer process)
      (delete-region (point-min) (point-max))
      ;; We add 3 here to exclude the prompt from the result
      (let ((start (+ 3 (point-min)))
	    (row-changed-regexp "(\\([0-9]+\\) rows affected)")
	    (error-msg-regexp "Msg [0-9]+, Level [0-9]+, State [0-9]+, Server .+, Line [0-9]+
\\(.*\\)$")
	    (end-flg-regexp "^1> ")
	    end
	    (count 0))
	(goto-char (point-max))
	(process-send-string process (format "%s ;\n" sql))
	(process-send-string process "go\n")
	(goto-char start)
	(while (not (re-search-forward end-flg-regexp nil t 1))
	  (when (accept-process-output process 300 0 nil)
	    (goto-char (+ 3 (point-min)))))
	;; find the starting point of the query result which is everything before `row-changed-regexp'
	(if (re-search-forward row-changed-regexp nil t -1)
	    (progn
	      (setq count (string-to-number (match-string 1)))
	      (setq end (max start (1- (match-beginning 0)))))
	  ;; an error occurred let's find the error message
	  (if (re-search-forward error-msg-regexp nil t -1)
	      (user-error "Query failed: %s" (match-string 1))
	    (user-error "Query failed")))
	(list start end count)))))

(defun sql-server-get-result-list (sql)
  "Returns the result of the query in a list"
  (unless (s-contains? "*Minibuf" (format "%s" (current-buffer)))
    (message "Sending query: %s" sql))
  (let* ((query-result (sql-server--execute-query sql))
	 (result-beg (car query-result))
	 (result-end (cadr query-result))
	 (result-count (caddr query-result))
	result row line-count)
    (let ((temp-buffer sql-server-temp-buffer))
     (with-temp-buffer
       (insert-buffer-substring-no-properties temp-buffer result-beg result-end)
       (setq line-count (count-lines (point-min) (point-max)))
       (goto-char (point-min))
       (when (> line-count 1)
	 (while (<= (line-number-at-pos) line-count)
	   (setq row (split-string (buffer-substring-no-properties
				    (point-at-bol) (point-at-eol)) "\^E" t))
	   (setq result (append result (list row)))
	   (forward-line)))))
    (when (and result (> (length result) 1))
      (setcdr result (cddr result)))
    (cond ((and result-count (< (length result) 2) (eq 0 result-count))
	   (message "No record found"))
	  ((and result-count (< (length result) 2) (> result-count 0))
	   (message "%s rows affected" result-count)))
    result))

(defun sql-server-get-result-value (sql)
  "Returns the result of a query which returns a single value"
  (let* ((query-result (sql-server--execute-query sql))
	 (result-beg (car query-result))
	 (result-end (cadr query-result))
	 (result-count (caddr query-result))
	result row line-count)
    (with-temp-buffer
      (insert-buffer-substring-no-properties sql-server-temp-buffer result-beg result-end)
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (point-max)))))

(defvar sql-server-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap ivy-alt-done] 'sql-server-ivy-table-columns)
    (define-key map (kbd "C-DEL") 'sql-server-ivy-tables)
    map)
  "Keymap used in sql-server minibuffer")

(define-key sql-server-map (kbd "C-h") 'sql-server-ivy-tables)

(defun sql-server-add-to-history (sql)
  "Adds query to `sql-server-history'"
  (let ((sql (replace-regexp-in-string "\n" "" sql)))
    (when (not (-any? (lambda (x) (string-equal x sql)) sql-server-history))
      (push sql sql-server-history))))

(defun sql-server-show-history ()
  "Shows previous sql executed"
  (interactive)
  (ivy-read "sql history:"
            sql-server-history
            :action 'insert))

(defun ivy--reset-minibuffer ()
  (setq ivy--old-cands nil)
  (setq ivy--old-re nil)
  (setq ivy--index 0)
  (setq ivy--all-candidates nil)
  (setq ivy-text "")
  (delete-minibuffer-contents))

(defun sql-server-ivy-align-text (text col-length)
  "Fills string with blank if less than length"
  (s-pad-right col-length " " text))

(defun sql-server-set-header-line-format (string)
  "Set the header-line using STRING."
  (let* ((header-line
          (concat (propertize " "
                              'display
                              '(space :align-to 0))
                  string
                  (propertize
                   " "
                   'display
                   `(space :width (+ left-fringe
                                     left-margin
                                     ,@(and (eq (car (window-current-scroll-bars))
                                                'left)
                                            '(scroll-bar)))))))
         (len (length header-line)))
    (setq header-line-format
          (propertize header-line
                      'face
                      'magit-header-line))))

(provide 'sql-server)
;;; sql-server.el ends here
