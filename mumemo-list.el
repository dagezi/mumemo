;;; mumemo-list
;;; SASAKI, Takesi (dagezi@gmail.com)

(defun mumemo-list-mode ()
  ;;; TODO: key bindings or so...
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mumemo-list-mode)
  (use-local-map mumemo-list-mode-map))

(defvar mumemo-list-mode-map
  (make-sparse-keymap))

(define-key mumemo-list-mode-map "\C-m" 'mumemo-list-open)
(define-key mumemo-list-mode-map "\C-j" 'mumemo-list-open)


(defun mumemo-put-item-property-to-string (string item)
  (put-text-property 0 (length string) :mumemo-item item string)
  string)

(defun mumemo-list-format-item (item)
  (let ((string
	 (format "%10s|%s| %s\n"
		 (mumemo-universe-name (mumemo-item-universe item))
		 (format-time-string "%m%d" (mumemo-item-mtime item))
		 (mumemo-item-path item))))   ;; TODO: add snipet1
    (mumemo-put-item-property-to-string string item)))


(defun mumemo-list-new-buffer (buffer-name items)
  ;; TODO: support custom format
  (let ((buffer (get-buffer-create buffer-name)))
    (set-buffer buffer)
    (setq buffer-undo-list t)
    (erase-buffer)
    (mapc #'(lambda (item)
	      (insert (mumemo-list-format-item item)))
	  items)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (mumemo-list-mode)
    (switch-to-buffer buffer)))

(defun mumemo-list-open (position)
  (interactive "d")
  (let ((item (get-text-property position :mumemo-item)))
    (when item
      (mumemo-open-item item))))

(provide 'mumemo-list)
