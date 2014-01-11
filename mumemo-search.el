;; Hack for search 

;; TODO:
;;; 1. Sort result by last modified date
;;; 2. If target matched on the snippet of the content, reise it.
;;;    Maybe this recognize header.
;;; 3. One line for one file.
;;; 3.1. Present like tree-structure.


(defun mumemo-search (regexp flags)
  "Searches given REGEXP in UNIVERSE and returns intents"
  (interactive (list (read-regexp "Regexp")
                     (let ((default-flag "-i"))
                       (if current-prefix-arg 
                           (read-string "Option: " default-flag)
                         default-flag))))
  (let ((args `("find"
                ,@(mapcar #'(lambda (univ) 
                              (file-name-as-directory (mumemo-universe-directory univ)))
                          mumemo-universes)
                "-type" "f" "-exec" "grep" "-nH" ,flags "-e" ,regexp "{}" "+")))
    (compilation-start (combine-and-quote-strings args))))

(provide 'mumemo-search)
