(load "../main.lsp")
(format t "Файл main.lsp загружен~%")


(defun test-create-table ()
  (create-table "test-table" '(("id" integer primary key)
			       ("name" string)
			       ("email" string)))
  (format t "~%Тестовая таблица 'test-table' создана.~%"))

(defun test-insert-into ()
  (test-create-table)
  (insert-into "test-table" '(("id" . 1) ("name" . "admin") ("email" . "admin@example.com")))
  (let ((table (gethash "test-table" *database*))
	(expected-row '(("id" . 1) ("name" . "admin") ("email" . "admin@example.com"))))
    (if (and table (equal (first (table-rows table)) expected-row))
	 (format t "test-insert-into: PASSED~%")
         (format t "test-insert-into: FAILED - ожидалось ~a, но было ~a~%" expected-row (table-rows table)))))

(defun test-select-from ()
  (create-table "test-table1" '(("id" integer primary key)
			       ("name" string)
			       ("email" string)))
  (insert-into "test-table1" '(("id" . 1) ("name" . "Egor") ("email" . "egor@mail.ru")))

  (let* ((result (select-from "test-table1" "name" "email"))
	 (expected '(("name" . "Egor") ("email" . "egor@mail.ru"))))
    (format t "Результат выборки: ~a~%" (first result))
    (if (equal (first result) expected)
	(format t "test-select-from: PASSED~%")
	(format t "test-select-from: FAILED - ожидалось ~a, но было получено ~a~%" expected result))))

(defun test-delete-from ()
  ;Удаляем элемент с id == 1
  (create-table "test-table2" '(("id" integer primary key)
			       ("name" string)))
  (insert-into "test-table2" '(("id" . 1) ("name" . "Egor")))
  (insert-into "test-table2" '(("id" . 2) ("name" . "admin")))
  (let* (
	 (expected '(("id" . 2) ("name" . "admin")))
	 (result (delete-from "test-table2" '(("name" . "Egor")))))
    (if (equal (first result) expected)
	(format t "test-delete-from: PASSED~%")
	(format t "test-delete-from: FAILED - ожидалось ~a, но было получено ~a~%" expected result))))
    

(defun run-unittest ()
  (format t "Начало тестирования...~%")
  (test-insert-into)
  (test-select-from)
  (test-delete-from)
  (format t "Тестирования завершенно.~%"))
