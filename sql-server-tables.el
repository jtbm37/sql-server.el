(require 'sql-server)

(defun sql-server-get-tables (&optional input ok we)
  (ivy--reset-tables)
  (when (or (and current-prefix-arg (= (car current-prefix-arg) 4)) (not sql-server-tables-cache))
    (message "Refreshing tables")
    (setq sql-server-tables-cache (-flatten (cdr (sql-server-get-result-list "SELECT name from sys.tables order by name")))))
  ;; (->> sql-server-tables-cache (-filter 'sql-server-tablep) (-map (lambda (x) (nth 2 x))))
  sql-server-tables-cache)

;;;###autoload
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

(defun sql-server-tables-at-point  (start end)
  "Display list of table prefiltered by selected region"
  (interactive "r")
  (sql-server-tables (buffer-substring-no-properties start end)))

(defun sql-server-ivy-get-table-columns (table)
  "Returns a list of the table columns"
  ;;TODO Cache it
  (-map 'sql-server-ivy-format-table-column (cdr (sql-server-get-result-list (format "select COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, IS_NULLABLE from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '%s';"
										     table)))))
(defun sql-server-ivy-format-table-column (col)
  "Formats ivy table columns"
  (let* ((name (car col))
	 (length (nth 2 col))
	 (nullable (string= "YES" (nth 3 col)))
	 (type (if (string= "NULL" length)
		   (cadr col)
		 (format "%s(%s)" (cadr col) length))))
    (propertize
     (concat
      name (format " (%s,%snull)" (propertize type 'face 'font-lock-doc-face) (if nullable " " " not ")))
     'name name
     'type type)))

(defun sql-server-get-table-at-point (start end)
  (interactive "r")
  (sql-server-get-table-columns (buffer-substring-no-properties start end)))

(defun sql-server-get-table-columns (table)
  "Returns the list of columns for `table'"
  (sql-server-send (format sql-server-ivy-table-columns-query table)))

(ivy-set-actions
 'sql-server-tables
 '(("i" sql-server-ivy-insert-where-query "insert - select where")))

(defun sql-server-ivy-insert-where-query (selection)
  "Inserts a query based on selection.
If it is a column it will insert `select * from =table= where =column='.
`selection' is either a table or a column name depending on whether `sql-server-ivy-current-table'
is set. "
  (if sql-server-ivy-current-table
      (insert (format "select * from %s where %s " sql-server-ivy-current-table (get-text-property 1 'name selection)))
    (insert (concat "select * from " selection))))

(provide 'sql-server-tables)
