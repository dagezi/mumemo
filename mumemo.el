;;; Mulit Univalse Memo
;;;
;;; Just small tool for takeing mamo.
;;;
;;; SASAKI, Takesi  dagezi@gmail.com

(require 'cl)

(defstruct mumemo-universe 
  name directory path-template)  ;; TODO: remember symbolic dir like "~/howm"

(defvar mumemo-default-universe
  (make-mumemo-universe 
   :name "mumemo" 
   :directory (expand-file-name "~/mumemo")
   :path-template "%Y/%m/%d-%H%M%S.md" ))
(defvar mumemo-universes (list mumemo-default-universe)
  "The list of mumemo universes")

(defvar mumemo-current-universe nil "Universe which current file belongs to")

(defun mumemo-get-universe (name)
  (some #'(lambda (univ) (and (equal name (mumemo-universe-name univ)) univ))
	mumemo-universes))

(defun mumemo-universes-names ()
  (mapcar #'mumemo-universe-name mumemo-universes))

(defun mumemo-interactively-get-universe (arg)
  (if (and (not arg) mumemo-current-universe)
      mumemo-current-universe
    (let ((name (completing-read "Which universe: " 
				 (mumemo-universes-names)
				 nil t 
				 (mumemo-universe-name (or mumemo-current-universe
							   (car mumemo-universes))))))
      (mumemo-get-universe name))))

(defun mumemo-create-file (universe)
  "Create new mumemo file"
  (interactive (list (mumemo-interactively-get-universe current-prefix-arg)))
  (let* ((dir (mumemo-universe-directory universe))
	 (path (format-time-string (mumemo-universe-path-template universe)))
	 (a-path (concat dir "/" path)))
    (make-directory (file-name-directory a-path) t)
    (find-file a-path)
    ;; TODO: add temlate?
))


(defstruct mumemo-item 
  universe  ; universe to which this item belongs
  path      ; relative path from universe's directory
  mtime)    ; mtime in Elisp way (high low usec psec)

(defun mumemo-create-item (universe path)
  (let* ((a-path (concat (mumemo-universe-directory universe) "/" path))
	 (mtime (nth 5 (file-attributes a-path))))
    (make-mumemo-item :universe universe :path  path :mtime mtime)))

(defun mumemo-universe-get-all-items (universe)
  (let ((default-directory (mumemo-universe-directory universe))
	(dir-stack '("."))
	result)
    (while dir-stack
      (let* ((dir (pop dir-stack))
	     (files (directory-files dir nil nil t))) ;; TODO: add match
	(mapc
	 #'(lambda (file)
	     (unless (string-match "^\\.\\.?$" file)
	       (let ((path (if (equal dir ".") file (concat dir "/" file))))
		 (if (file-directory-p path)
		     (push path dir-stack)
		   (push (mumemo-create-item universe path) result)))))
	 files)))
    result))

(defvar mumemo-list-recent-days 14)

(defun mumemo-show-recent-files (days)
  "Show recent modified files for all universes"
  (interactive "P")
  (when (null days)
    (setq days mumemo-list-recent-days))
  
  (let* ((threshold (time-subtract (current-time) (days-to-time days)))
	 (items
	  (mapcan
	   #'(lambda (universe)
	       (remove-if 
		#'(lambda (item) 
		    (time-less-p (mumemo-item-mtime item) threshold))
		(mumemo-universe-get-all-items universe)))
	   mumemo-universes))
	 (sorted-items 
	  (sort items 
		#'(lambda (item0 item1)
		    (not (time-less-p (mumemo-item-mtime item0)
				     (mumemo-item-mtime item1)))))))
    (mumemo-list-new-buffer "*mumemo*" sorted-items)))

(defun mumemo-list-mode ()
  ;;; TODO: key bindings or so...
  (interactive)
  )

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
    (erase-buffer)
    (mapc #'(lambda (item)
	      (insert (mumemo-list-format-item item)))
	  items)
    (mumemo-list-mode)
    (switch-to-buffer buffer)))
		       

(provide 'mumemo)
