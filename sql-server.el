;;; sql-server.el --- Yet another SQL IDE                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  jtbm37

;; Author: jtbm37
;; URL: https://github.com/jtbm37/sql-server.el
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3") (swiper "0.8.0") (ctable "0.1.2"))
;; Keywords:

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

(defconst sql-server-temp-buffer " *TEMP SQL*")

(defvar sql-server-ivy-current-table nil
  "Holds current previously selected table when on columsn view")
(defvar ivy--sql-table nil
  "Current table when completing columns")

(defvar sql-server-history nil
  "Holds history of past sql statements executed")

(defvar sql-server-tables-cache nil
  "Holds a cache of the database tables")

(defvar sql-server-ivy-columns-visible nil
  "Whether the table columns are currently shown in minibuffer. Dictates the available action depending on whether tables or columns are shown")

(defconst sql-server-ivy-table-columns-query "select COLUMN_NAME, DATA_TYPE, IS_NULLABLE, COLUMN_DEFAULT, CHARACTER_MAXIMUM_LENGTH from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '%s';"
  "Defines the query to retrieve the columns of a table")

(defun sql-server-comint (product options)
  "Create comint buffer and connect to Sql Server.
Command is to `isql DB username password'"
  (let ((params
         (append
          (if (not (string= "" sql-database))
              (list sql-database))
          (if (not (string= "" sql-user))
              (list sql-user))
          (if (not (string= "" sql-password))
              (list sql-password))
          options)))
    (sql-comint product params)))


(advice-add 'sql-send-region :override #'sql-server-send-region)

(spacemacs/set-leader-keys-for-major-mode 'sql-mode
  "d" 'sql-server-set-defaults)

(sql-set-product-feature 'ms :sqli-program "isql")
;; nil will not ask for any input from user and get the defaults
(sql-set-product-feature 'ms :sqli-login nil)
(sql-set-product-feature 'ms :sqli-options (list "-n" "-x0x7c" "-c"))
(sql-set-product-feature 'ms :sqli-comint-func 'sql-server-comint)
(sql-set-product-feature 'ms :prompt-regexp "[0-9] SQL> ")
(sql-set-product-feature 'ms :prompt-length 7)
(sql-set-product-feature 'ms :terminator ";")
(sql-set-product-feature 'ms :input-filter nil)

(defun sql-server-input-filter (input)
  "Removes GO from input"
  (replace-regexp-in-string "^go" ";" input))

(defun sql-server-set-defaults ()
  "Sets default sql variable using `auth-source'.
Add the following entry to your `.authinfo' file:
machine sqllocal login `yourlogin' db `yourdatabase' password `yourpassword'
"
  (interactive)
  (-if-let* ((entry (nth 0 (auth-source-search :max 1 :host "sqllocal")))
         (login (plist-get entry :user))
         (db (plist-get entry :db))
         (secretfun (plist-get entry :secret))
         (secret (funcall secretfun)))
      (progn (setq sql-user login
                   sql-password secret
                   sql-database db
                   sql-server db
                   sql-send-terminator t
                   sql-product 'ms)
             (message "Sql-server defaults set"))
    (user-error "Credentials not found")))

(defun sql-server-get-table-at-point (start end)
  (interactive "r")
  (sql-server-get-table-columns (buffer-substring-no-properties start end)))

(defun sql-server-get-table-columns (table)
  "Returns the list of columns for `table'"
  (sql-server-send (format sql-server-ivy-table-columns-query table) t))

(defun sql-server-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (sql-server-send (buffer-substring-no-properties start end)))

(defun sql-server-send (sql &optional nocount)
  "Sends string `sql' to server.
When `nocount' is t, the last line with the row count is excluded."
  (let ((result (sql-server-get-result-list (sql-server-sanitize-query sql))))
    (when nocount
      (setq result (-drop-last 1 result)))
    (cond ((not (seq-empty-p result))
           (let (buffer window)
             (save-excursion (setq buffer (ctbl:create-table-buffer-easy
                                           (cdr result)
                                           (car result))))
             (select-window (display-buffer-at-bottom buffer nil) nil)
             ))
          (t (user-error "SQL empty")))))

(defun sql-server-sanitize-query (sql)
  ;; Remove the GO statements and replace them with ';'
  (let ((sql-fixed (s-trim
                    (replace-regexp-in-string "\n" " "
                                              ;;remove GOs'
                                              (replace-regexp-in-string "^go\w*$" ""
                                                                        ;; delete comments
                                                                        (replace-regexp-in-string "^--.*$" "" sql))))))
    (unless (s-suffix? ";" sql-fixed)
      (setq sql-fixed (concat sql-fixed ";")))
    sql-fixed))

(defun sql-server-get-result-list (sql)
  "Returns the result of the query in a list"
  (unless (s-contains? "*Minibuf" (format "%s" (current-buffer)))
    (message "Sending query: %s" sql))
  (let ((raw-lines (sql-server-get-result-lines sql)))
    (mapcar (lambda (x)
              (split-string x "|"))
            raw-lines)))

(defun sql-server-get-result-lines (sql)
  "Returns a list of each output lines"
  (with-current-buffer "*SQL*"
    (comint-redirect-results-list sql "\\(.+\\)$" 1)))

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

(defun sql-server-tablep (x)
  (string= (nth 3 x) "TABLE"))

(defun sql-server-get-tables (&optional input ok we)
  (ivy--reset-tables)
  (when (or (and current-prefix-arg (= (car current-prefix-arg) 4)) (not sql-server-tables-cache))
    (message "Refreshing tables")
    (setq sql-server-tables-cache (sql-server-get-result-list "\\tables;")))
  (->> sql-server-tables-cache (-filter 'sql-server-tablep) (-map (lambda (x) (nth 2 x)))))

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
  (-map 'sql-server-ivy-format-table-column (-drop-last 1 (cdr (sql-server-get-result-list (format "select COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '%s';"
                                                                                     table))))))
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
