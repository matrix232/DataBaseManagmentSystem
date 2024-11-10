(load "./main.lsp")
(format t "Файл main.lsp загружен~%")


(defun test-create-table ()
  (create-table "test-table" '(("id" integer primary key)
			       ("name" string)
			       ("email" string)))
  (format t "~%Тестовая таблица 'test-table' создана.~%"))

(defun test-insert-into (table-name columns row expected-row)
  (create-table table-name columns)
  (insert-into table-name row)
  (let ((table (gethash table-name *database*)))
    (if table
        (let ((actual-row (first (table-rows table))))
          (as-eq "test-insert-into" actual-row expected-row))
        (format t "ERROR: Table ~a не найдена в базе данных~%" table-name))))
  

(defun test-select-from (table-name columns rows selected-columns expected-result)
  (create-table table-name columns)
  (dolist (row rows)
    (insert-into table-name row))
  (let* ((result (select-from table-name selected-columns)))
    (as-eq "test-select-from" result expected-result)))


(defun as-eq (test-name result expected)
  (if (equal result expected)
      (format t "~a: PASSED~%" test-name)
      (format t "~a: FAILED - ожидалось ~a, но было полученно ~a~%" test-name expected result)))
  

(defun run-unittest ()
  (format t "Начало тестирования...~%")
  (test-insert-into "users" '(("id" integer) ("name" string) ("age" integer))
		    '(("id" . 1) ("name" . "adm") ("age" . 24))
		    '(("id" . 1) ("name" . "adm") ("age" . 24)))
  (test-select-from "test-table4" 
                    '(("id" integer) ("name" string) ("age" integer))
                    '((("id" . 1) ("name" . "adm") ("age" . 24))
                      (("id" . 2) ("name" . "egr") ("age" . 30)))
                    "name"
                    '((("name" . "egr"))
                      (("name" . "adm"))))
  (format t "Тестирования завершенно.~%"))
