;;; sql-server-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "sql-server" "sql-server.el" (22611 8043 802434
;;;;;;  285000))
;;; Generated autoloads from sql-server.el

(autoload 'sql-server-set-defaults "sql-server" "\
Sets default sql variable using `auth-source'.
Add the following entry to your `.authinfo' file:
machine sqllocal login `yourlogin' db `yourdatabase' password `yourpassword'

\(fn)" t nil)

(autoload 'sql-server-send-buffer "sql-server" "\
Sends current buffer file to server.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sql-server" '(#("sql-server-" 0 11 (fontified nil)) #("ivy--" 0 5 (fontified nil)))))

;;;***

;;;### (autoloads nil "sql-server-sps" "sql-server-sps.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from sql-server-sps.el

(autoload 'sql-server-stored-procs "sql-server-sps" "\
Displays list of stored procs

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sql-server-sps" '("sql-server-")))

;;;***

;;;### (autoloads nil "sql-server-tables" "sql-server-tables.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from sql-server-tables.el

(autoload 'sql-server-tables "sql-server-tables" "\
Displays list of all tables

\(fn &optional TABLE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sql-server-tables" '(#("sql-server-" 0 11 (fontified nil face font-lock-function-name-face)) #("ivy--reset-tables" 0 2 (face font-lock-function-name-face fontified t) 2 17 (face font-lock-function-name-face fontified t)))))

;;;***

(provide 'sql-server-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sql-server-autoloads.el ends here
