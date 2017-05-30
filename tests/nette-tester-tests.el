;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'nette-tester)


(defun nette-tester-test--get-regexp ()
  (cadr (assq 'nette-tester compilation-error-regexp-alist-alist)))


(describe "In compilation buffer"

  (it "should detect an error without a diff without a stack trace"
    (let ((regexp (nette-tester-test--get-regexp)))
      (with-temp-buffer
        (insert "-- FAILED: Report/Agent/SortStage.phpt
   Exited with error code 255 (expected 0)

   Parse error: syntax error, unexpected '$container' (T_VARIABLE), expecting ',' or ';' in /home/matus/dev/application/tests/Test.phpt on line 13
")
        (goto-char (point-min))
        (expect (re-search-forward regexp) :to-be-truthy)
        (expect (match-string 2) :to-equal "/home/matus/dev/application/tests/Test.phpt")
        (expect (match-string 3) :to-equal "13"))))


  (it "should detect an error without a diff with a stack trace"
    (let ((regexp (nette-tester-test--get-regexp)))
      (with-temp-buffer
        (insert "-- FAILED: Report/Agent/SortStage.phpt
   Exited with error code 255 (expected 0)
   Nette\\DI\\MissingServiceException: Service of type Tests\\ResourceProvider not found.

   in src/DI/Container.php(210)
   in tests/Test.phpt(85) Nette\\DI\\Container->getByType()
")
        (goto-char (point-min))
        (expect (re-search-forward regexp) :to-be-truthy)
        (expect (match-string 2) :to-equal "tests/Test.phpt")
        (expect (match-string 3) :to-equal "85"))))


  (it "should detect an error with a diff with a stack trace"
    (let ((regexp (nette-tester-test--get-regexp)))
      (with-temp-buffer
        (insert "-- FAILED: Report/Agent/SortStage.phpt
   Exited with error code 255 (expected 0)
   Nette\\DI\\MissingServiceException: Service of type Tests\\ResourceProvider not found.

   diff 'foo' 'bar'

   in src/DI/Container.php(210)
   in tests/Test.phpt(85) Nette\\DI\\Container->getByType()
")
        (goto-char (point-min))
        (expect (re-search-forward regexp) :to-be-truthy)
        (expect (match-string 1) :to-equal "diff 'foo' 'bar'")
        (expect (match-string 2) :to-equal "tests/Test.phpt")
        (expect (match-string 3) :to-equal "85"))))


  (it "should detect an error with a stack trace and multiple calls in the same phpt file"
    (let ((regexp (nette-tester-test--get-regexp)))
      (with-temp-buffer
        (insert "-- FAILED: Report/Agent/SortStage.phpt
   Exited with error code 255 (expected 0)
   Error: Call to undefined method Tests\\Utils\\ResourceProvider::getActiveCampaign()

   in tests/Test.phpt(42)
   in [internal function]Tests\\Test->testCreateSortStageBySpecification()
   in src/Framework/TestCase.php(152) call_user_func_array()
   in src/Framework/TestCase.php(55) Tester\\TestCase->runTest()
   in tests/php/bootstrap.php(24) Tester\\TestCase->run()
   in tests/Test.phpt(86) run()
")
        (goto-char (point-min))
        (expect (re-search-forward regexp) :to-be-truthy)
        (expect (match-string 2) :to-equal "tests/Test.phpt")
        (expect (match-string 3) :to-equal "42")))))
