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

;; NOTICE: the underlying isql program has been modified and recompiled to accomodate
;; some requirements of this configuration. For example, in the original version if you
;; pass a column modifier (like we do '|') then the SQLRowCount is never output. This
;; is unfortunate and had to be changed.
;; isql version 2.3.4 is required. http://www.unixodbc.org/

;; This package is still under heavy development and should not be
;; used against production sql server.

;;; Code:
(require 's)
(require 'ctable)

(defconst sql-server-temp-buffer " *TEMP SQL*")
(defgroup sql-server nil
  "Sql Server"
  :group 'SQL)

(defcustom sql-server-confirm-delete t
  "Whether it asks to confirm execution of delete/truncate statements"
  :group 'sql-server)

(defcustom sql-server-ivy-columns-visible nil
  "Whether the table columns are currently shown in minibuffer. Dictates the available action depending on whether tables or columns are shown"
  :group 'sql-server)

(defcustom sql-server-connection
  '((login . nil)
    (password . nil)
    (server . nil)
    (db . nil))
  "sql-server connection details"
  :group 'sql-server
  :type 'alist)

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

;; (defun sql-server-comint (product options)
;;   "Create comint buffer and connect to Sql Server.
;; Command is to `isql DB username password'"
;;   (let ((params
;;          (append
;; 	  (list "-S" sql-server)
;;           (if (not (string= "" sql-database))
;;               (list "-d" sql-database))
;;           (if (not (string= "" sql-user))
;;               (list "-U" sql-user))
;;           (if (not (string= "" sql-password))
;;               (list "-P" sql-password))
;;           options)))
;;     (sql-comint product params)))


(advice-add 'sql-send-region :override #'sql-server-send-region)

;; (sql-set-product-feature 'ms :sqli-program "sqlcmd")
;; nil will not ask for any input from user and get the defaults
;; (sql-set-product-feature 'ms :sqli-login nil)
;; (sql-set-product-feature 'ms :sqli-options (list "-W" "-s" "	"))
;; (sql-set-product-feature 'ms :sqli-options nil)
;; (sql-set-product-feature 'ms :sqli-comint-func 'sql-server-comint)
;; (sql-set-product-feature 'ms :prompt-regexp "[0-9]> ")
;; (sql-set-product-feature 'ms :prompt-length 3)
;; (sql-set-product-feature 'ms :terminator "go")
;; (sql-set-product-feature 'ms :input-filter nil)

(defun sql-server-input-filter (input)
  "Removes GO from input"
  ;; (replace-regexp-in-string "^go" ";" input)
  )

(defun sql-server-set-defaults ()
  "Sets default sql variable using `auth-source'.
Add the following entry to your `.authinfo' file:
machine sqllocal login `yourlogin' db `yourdatabase' password `yourpassword'
"
  (interactive)
  (-if-let* ((entry (nth 0 (auth-source-search :max 1 :host "sqllocal")))
         (login (plist-get entry :user))
         (server (plist-get entry :server))
         (db (plist-get entry :db))
         (secretfun (plist-get entry :secret))
         (secret (funcall secretfun)))
      (progn
	(setcdr  (assoc 'login sql-server-connection) login)
	(setcdr  (assoc 'password sql-server-connection) secret)
	(setcdr  (assoc 'db sql-server-connection) db)
	(setcdr  (assoc 'server sql-server-connection) server)
	(message "sql-server connection set"))
    (user-error "Credentials not found")))


(defun sql-server--command-args ()
  "Returns sqlcmd args preformatted for process call"
  (list "-S" (cdr (assoc 'server sql-server-connection))
	"-U" (cdr (assoc 'login sql-server-connection))
	"-P" (cdr (assoc 'password sql-server-connection))
	"-d" (cdr (assoc 'db sql-server-connection))
	"-s" "\^E"
	"-W"))

(defun sql-server-connect ()
  "Establishes connection to database specified in `sql-server-connection'"
  (interactive)
  (let ((process (apply 'start-process ;; dino
		  "sql-server"
		  sql-server-temp-buffer
		  "sqlcmd"
		  (sql-server--command-args))))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\|exited abnormally with code\\)" change)
	 (kill-buffer (process-buffer proc))
	 (message (concat (process-name proc) " exited")))))
    ;; (list process
    ;; 	  (process-buffer process)
    ;; 	  connection-info)
    ))

(defun sql-server-get-table-at-point (start end)
  (interactive "r")
  (sql-server-get-table-columns (buffer-substring-no-properties start end)))

(defun sql-server-get-table-columns (table)
  "Returns the list of columns for `table'"
  (sql-server-send (format sql-server-ivy-table-columns-query table)))

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
	(save-excursion (switch-to-buffer (ctbl:create-table-buffer-easy
					   (cdr result)
					   (car result))))))))

(defun sql-server-sanitize-query (sql)
  ;; Remove empty lines as they cause additional prompt added to the result buffer
  (replace-regexp-in-string "\n" " " sql)
  ;; (let ((sql-fixed (s-trim
  ;;                   (replace-regexp-in-string "\n" " "
  ;;                                             ;;remove GOs'
  ;;                                             (replace-regexp-in-string "^go\w*$" ""
  ;;                                                                       ;; delete comments
  ;;                                                                       (replace-regexp-in-string "^--.*$" "" sql))))))
  ;;   (unless (s-suffix? ";" sql-fixed)
  ;;     (setq sql-fixed (concat sql-fixed ";")))
  ;;   sql-fixed)
  )

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
	    (end-flg-regexp "1> ")
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
    (with-temp-buffer
      (insert-buffer-substring-no-properties sql-server-temp-buffer result-beg result-end)
      (setq line-count (count-lines (point-min) (point-max)))
      (goto-char (point-min))
      (when (> line-count 1)
	(while (<= (line-number-at-pos) line-count)
	  (setq row (split-string (buffer-substring-no-properties
				   (point-at-bol) (point-at-eol)) "\^E" t))
	  (setq result (append result (list row)))
	  (forward-line))))
    (when (and result (> (length result) 1))
      (setcdr result (cddr result)))
    (cond ((and result-count (< (length result) 2) (eq 0 result-count))
	   (message "No record found"))
	  ((and result-count (< (length result) 2) (> result-count 0))
	   (message "%s rows affected" result-count)))
    result)
  ;; (let ((raw-lines (sql-server-get-result-lines sql)))
  ;;   (mapcar (lambda (x)
  ;;             (split-string x "|"))
  ;;           raw-lines))
  )

;; (defun sql-server-get-result-lines (sql)
;;   "Returns a list of each output lines"
;;   (with-current-buffer "*SQL*"
;;     (comint-redirect-results-list sql "\\(.+\\)$" 1)))

(defvar sql-server-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap ivy-alt-done] 'sql-server-ivy-table-columns)
    (define-key map (kbd "C-DEL") 'sql-server-ivy-tables)
    map)
  "Keymap used in sql-server minibuffer")

(define-key sql-server-map (kbd "C-h") 'sql-server-ivy-tables)

(defun sql-server-add-to-history (sql)
  "Adds query to `sql-server-history'"
  (push sql sql-server-history))

(defun sql-server-show-history ()
  "Shows previous sql executed"
  (interactive)
  (ivy-read "sql history:"
            sql-server-history
            :action 'insert))

(defun sql-server-tables (&optional table)
  "Displays list of all tables"
  (interactive)
  (ivy-read (format "%s table: " sql-database)
            'sql-server-get-tables
            :initial-input (unless prefix-arg
                             table)
            :keymap sql-server-map
            :require-match 'confirm-after-completion
            :caller 'sql-server-tables
            ))

(defun sql-server-ivy-insert-where-query (selection)
  "Inserts a query based on selection.
If it is a column it will insert `select * from =table= where =column='.
`selection' is either a table or a column name depending on whether `sql-server-ivy-current-table'
is set. "
  (if sql-server-ivy-current-table
      (insert (format "select * from %s where %s " sql-server-ivy-current-table (get-text-property 1 'name selection)))
    (insert (concat "select * from " selection))))

(ivy-set-actions
 'sql-server-tables
 '(("i" sql-server-ivy-insert-where-query "insert - select where")))

(defun sql-server-tables-at-point  (start end)
  "Display list of table prefiltered by selected region"
  (interactive "r")
  (sql-server-tables (buffer-substring-no-properties start end)))

;; (defun sql-server-tablep (x)
;;   (string= (nth 3 x) "TABLE"))

(defun sql-server-get-tables (&optional input ok we)
  (ivy--reset-tables)
  (when (or (and current-prefix-arg (= (car current-prefix-arg) 4)) (not sql-server-tables-cache))
    (message "Refreshing tables")
    (setq sql-server-tables-cache (-flatten (cdr (sql-server-get-result-list "SELECT name from sys.tables order by name")))))
  ;; (->> sql-server-tables-cache (-filter 'sql-server-tablep) (-map (lambda (x) (nth 2 x))))
  sql-server-tables-cache)

(defun sql-server-ivy-tables ()
  (interactive)
  (ivy--reset-tables)
  (ivy--reset-minibuffer)
  (setq ivy--all-candidates (sql-server-get-tables))

  (ivy-set-prompt 'sql-server-tables (lambda () (ivy-add-prompt-count
                                             (format "%stable: " "%-4d"))))
  (setf (ivy-state-preselect ivy-last) ivy--sql-table)
  (ivy--exhibit))

(defun sql-server-ivy-table-columns (&optional arg)
  (interactive "P")
  (unless sql-server-ivy-columns-visible
    (setq sql-server-ivy-columns-visible t)
    (ivy--reset-minibuffer)
    (setq sql-server-ivy-current-table ivy--current
          ivy--sql-table ivy--current)
    (ivy-set-prompt 'sql-server-tables (lambda () (ivy-add-prompt-count
                                               (format "%s[%s] columns: " "%-4d" sql-server-ivy-current-table))))
    (setq ivy--all-candidates (sql-server-ivy-get-table-columns ivy--current))
    (ivy--exhibit)))

(defun ivy--reset-tables ()
  "Resets variables used by `sql-server-tables'"
  (setq sql-server-ivy-columns-visible nil
        sql-server-ivy-current-table nil))

(defun ivy--reset-minibuffer ()
  (setq ivy--old-cands nil)
  (setq ivy--old-re nil)
  (setq ivy--index 0)
  (setq ivy--all-candidates nil)
  (setq ivy-text "")
  (delete-minibuffer-contents))

(defun sql-server-ivy-get-table-columns (table)
  "Returns a list of the table columns"
  ;;TODO Cache it
  (-map 'sql-server-ivy-format-table-column (cdr (sql-server-get-result-list (format "select COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '%s';"
										     table)))))
(defun sql-server-ivy-format-table-column (col)
  "Formats ivy table columns"
  (let* ((name (car col))
        (length (nth 2 col))
        (type (if (s-blank-str? length)
                  (cadr col)
                (format "%s(%s)" (cadr col) length))))
    (propertize
     (concat
      (propertize (sql-server-ivy-align-text type 20) 'face 'font-lock-doc-face) name)
     'name name
     'type type)))

(defun sql-server-ivy-align-text (text col-length)
  "Fills string with blank if less than length"
  (s-pad-right col-length " " text))

(provide 'sql-server)
;;; sql-server.el ends here
