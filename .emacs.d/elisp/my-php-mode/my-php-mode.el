(defun my-php-mode ()
  "My own php mode setup"
  (interactive)
  (php-mode)
  (message "My own php mode setup initialized")
  (set 'tab-width 4)
  (set 'c-basic-offset 4)
  (set 'indent-tabs-mode 1)     ; Reenabling tabs instead of spaces
)

(provide 'my-php-mode)
