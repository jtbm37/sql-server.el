(require 'sql-server)

(defun sql-server-get-sps (&optional input ok we)
  "Returns list of stored procs"
  (when (or (and current-prefix-arg (= (car current-prefix-arg) 4)) (not sql-server-sps-cache))
    (message "Refreshing tables")
    (setq sql-server-sps-cache (-flatten (cdr (sql-server-get-result-list "select name from sys.procedures order by name")))))
  sql-server-sps-cache)

(defun sql-server--get-sp-definition (proc)
  "Returns the definition by calling sqlcmd directly rather than using an existing session.
This is because we need to pass extra parameter to sqlcmd to prevent the stored proc content
from being truncated."
  (let* ((sql (format "select definition from sys.all_objects as sp LEFT OUTER JOIN sys.sql_modules AS smsp ON smsp.object_id = sp.object_id where name = '%s'" proc))
	 (args (-reduce (lambda (x y) (format "%s %s" x y)) (sql-server--command-args t)))
	(cmd (format "sqlcmd %s %s -Q \"%s\"" args "-y0" sql)))
    (message (concat "Executing " cmd))
    (shell-command-to-string cmd)))

(defun sql-server--display-proc (proc)
  "Show stored proc in new buffer"
  (let ((buffer (get-buffer-create (format "proc:%s" proc)))
	(definition (sql-server--get-sp-definition proc)))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert definition)
      (goto-line (count-lines (point-min) (point-max)))
      (kill-whole-line)
      (beginning-of-buffer)
      (replace-regexp "" "")
      (beginning-of-buffer))
    (save-excursion (switch-to-buffer buffer)
		    (sql-mode))))

;;;###autoload
(defun sql-server-stored-procs ()
  "Displays list of stored procs"
  (interactive)
  (ivy-read (format "%s sps: " sql-database)
	    'sql-server-get-sps
	    :action 'sql-server--display-proc
	    :require-match 'confirm-after-completion
	    :caller 'sql-server-stored-procs))

(provide 'sql-server-sps)
