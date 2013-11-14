;;; test code for mumeml.el
;;; 
(mumemo-deftest test-mumemo-item-create ()
  (let ((item (mumemo-item-create mumemo-test-universe "hoge")))
    (should (equal (expand-file-name "hoge" mumemo-test-content-directory)
			(mumemo-item-get-absolute-path item)))))


(mumemo-deftest test-mumemo-item-by-filename ()
  (let ((item (mumemo-item-by-filename 
	       (expand-file-name "test1" mumemo-test-content-directory))))
    (should (eq mumemo-test-universe (mumemo-item-universe item)))
    (should (equal "test1" (mumemo-item-path item))))
  (let ((item (mumemo-item-by-filename "/tmp/nothing/test1")))
    (should (null item))))

(mumemo-deftest test-mumemo-find-fild-hook ()
  (let ((mumemo-referred-items nil))
    (let ((item (mumemo-item-create mumemo-test-universe mumemo-test-filename))
	  (item1 (mumemo-item-create mumemo-test-universe mumemo-test-filename-1)))
      (should (equal mumemo-referred-items nil))
      
      (find-file (expand-file-name mumemo-test-filename mumemo-test-content-directory))
      (should (equal mumemo-referred-items (list item)))

      ;; 2nd time
      (find-file (expand-file-name mumemo-test-filename mumemo-test-content-directory))
      (should (equal mumemo-referred-items (list item)))

      ;; new file
      (find-file (expand-file-name mumemo-test-filename-1 mumemo-test-content-directory))
      (should (equal mumemo-referred-items (list item1 item))))))
