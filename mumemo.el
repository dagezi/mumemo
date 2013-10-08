;;; Mulit Universe Memo
;;;
;;; Just small tool for taking memo.
;;;
;;; SASAKI, Takesi  dagezi@gmail.com

(require 'cl)

(require 'mumemo-list)

(defstruct mumemo-universe 
  name directory path-template)  ;; TODO: remember symbolic dir like "~/howm"

(defvar mumemo-universe-default
  (make-mumemo-universe 
   :path-template "%Y/%m/%d-%H%M%S.md" ))

(defvar mumemo-universes nil "*The list of mumemo universes")

(defvar mumemo-current-universe nil "Universe which current file belongs to")

(defun* mumemo-universe-create 
    (name 
     &key 
     (directory (mumemo-universe-directory mumemo-universe-default))
     (path-template (mumemo-universe-path-template mumemo-universe-default)))
  (make-mumemo-universe :name name :directory directory :path-template path-template))

(defun mumemo-universe-get (name)
  (some #'(lambda (univ) (and (equal name (mumemo-universe-name univ)) univ))
	mumemo-universes))

(defun mumemo-universe-names ()
  (mapcar #'mumemo-universe-name mumemo-universes))

(defun mumemo-universe-get-interactively (arg)
  (if (and (not arg) mumemo-current-universe)
      mumemo-current-universe
    (let ((name (completing-read "Which universe: " 
				 (mumemo-universe-names)
				 nil t 
				 (mumemo-universe-name (or mumemo-current-universe
							   (car mumemo-universes))))))
      (mumemo-universe-get name))))

(defstruct mumemo-item 
  universe  ; universe to which this item belongs
  path      ; relative path from universe's directory
  snippet   ; snippet string cache
  mtime)    ; mtime in Elisp way (high low usec psec)

(defun mumemo-item-create (universe path)
  (let* ((a-path (concat (mumemo-universe-directory universe) "/" path))
	 (mtime (nth 5 (file-attributes a-path))))
    (make-mumemo-item :universe universe :path  path :mtime mtime)))

(defun mumemo-item-get-absolute-path (item)
  (concat (mumemo-universe-directory (mumemo-item-universe item)) "/"
	  (mumemo-item-path item)))

(defun mumemo-item-get-snippet (item)
  ;; TODO: when to validate snippet cache?
  (or (mumemo-item-snippet item)
      (let ((snippet 
	     (with-temp-buffer
	       (insert-file-contents (mumemo-item-get-absolute-path item) nil 0 1024)
	       (goto-char (point-min))
	       (save-match-data
		 (looking-at ".*")
		 (match-string 0)))))
	(setf (mumemo-item-snippet item) snippet)
	snippet)))

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
		   (push (mumemo-item-create universe path) result)))))
	 files)))
    result))

(defun mumemo-item-open (item)
  (find-file (mumemo-item-get-absolute-path item)))


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


(defun mumemo-create-file (universe)
  "Create new mumemo file"
  (interactive (list (mumemo-universe-get-interactively current-prefix-arg)))
  (let* ((dir (mumemo-universe-directory universe))
	 (path (format-time-string (mumemo-universe-path-template universe)))
	 (a-path (concat dir "/" path)))
    (make-directory (file-name-directory a-path) t)
    (find-file a-path)
    ;; TODO: add temlate?
))

(provide 'mumemo)
