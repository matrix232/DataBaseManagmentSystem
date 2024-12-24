(load "./main.lsp")
(format t "Файл main.lsp загружен~%")
;(in-package :DBMS-package)

(defun test-create-table (table-name &optional columns expected-result)
  (let ((result (create-table table-name columns)))
    (as-eq "test-create-table" result expected-result)))

(defun test-insert-into (table-name columns row expected-row)
  (create-table table-name columns)
  (insert-into table-name row)
  (let ((table (gethash table-name *database*)))
    (if table
        (let ((actual-row (first (table-rows table))))
          (as-eq "test-insert-into" actual-row expected-row))
        (format t "ERROR: Table ~a не найдена в базе данных~%" table-name))))
  
(defun test-select-from (table-name columns rows selected-columns expected-result &optional condition)
  (create-table table-name columns)
  (dolist (row rows)
    (insert-into table-name row))
  (let ((result (select-from table-name selected-columns condition)))
    (as-eq "test-select-from" result expected-result)))

(defun test-select-all (table-name columns rows expected-result)
  (create-table table-name columns)
  (dolist (row rows)
    (insert-into table-name row))
  (let ((result (select-all table-name)))
    (as-eq "test-select-all" result expected-result)))

(defun test-drop-table (table-name columns expected-result)
  (create-table table-name columns)
  (let ((result (drop-table table-name)))
    (as-eq "test-drop-table" result expected-result)))

(defun test-update-data (table-name columns rows id field-name new-value expected-result)
  (create-table table-name columns)
  (dolist (row rows)
    (insert-into table-name row))
  (let ((result (update table-name id field-name new-value))) 
    (as-eq "test-update" result expected-result)))

(defun test-backup-table (table-name columns rows backup-name expected-result)
  (create-table table-name columns)
  (dolist (row rows)
    (insert-into table-name row))
  (let ((result (backup-table table-name backup-name)))
    (as-eq "test-backup-table" result expected-result)))

(defun test-save-table-to-file (table-name columns rows path expected-result)
  (create-table table-name columns)
  (dolist (row rows)
    (insert-into table-name row))
  (let ((result (save-table-to-file table-name path)))
    (as-eq "test-save-table-to-file" result expected-result)))

(defun test-create-condition ()
  (let* ((rows '(((id . 1) (name . "Alice") (age . 25))
                 ((id . 2) (name . "Egor") (age . 30))
                 ((id . 3) (name . "Alex") (age . 35))))
         ;; Условие: возраст < 30
         (condition1 (create-condition '((< age 30))))
         ;; Условие: возраст между 20 и 40
         (condition3 (create-condition '((>= age 30) (/= name "Alex")))))
    (format t "Rows matching condition1: ~a~%" (remove-if-not condition1 rows))
    (format t "Rows matching condition3: ~a~%" (remove-if-not condition3 rows))))

(defun mk-sel-from ()
  (create-table 'tab1 '((id integer) (name string) (age integer)))  
  (insert-into 'tab1 '((id . 1) (name . "alex") (age . 25)))     
  (insert-into 'tab1 '((id . 2) (name . "egor") (age . 54)))
  (insert-into 'tab1 '((id . 3) (name . "vova") (age . 14)))
  (insert-into 'tab1 '((id . 4) (name . "daniil") (age . 18)))
  (insert-into 'tab1 '((id . 5) (name . "alex") (age . 27)))
  
  (let ((result (make-select-from 'tab1 '(name age)
  (and (string= (cdr (assoc 'name row)) "alex")
       (> (cdr (assoc 'age row)) 20)))))
    (format t "Result make-select-from: ~a~%" result)))


(defun as-eq (test-name result expected)
  (if (equal result expected)
      (format t "~a: PASSED~%" test-name)
      (format t "~a: FAILED - ожидалось ~a, но было полученно ~a~%" test-name expected result)))
  
(defun run-unittest ()
  (format t "Начало тестирования...~%~%")
  (test-create-condition)
  ;; Тестирование создания таблицы.
  (test-create-table 'test-table01 '((id . 1) (name . "adm")) 1)
  ;; Тестирование вставки данных в таблицу.
  (test-insert-into 'users '((id integer) (name string) (age integer))
		    '((id . 1) (name . "adm") (age . 24))
		    '((id . 1) (name . "adm") (age . 24)))
  (test-drop-table 'test-table111 '((id integer) (name string) (age integer)) 1)
  ;; Тестирование чтения данных из таблицы без условия.
  (test-select-from 'test-table111
		    '((id integer) (name string) (age integer))
		    '(((id . 1) (name . "egor") (age . 19))
		      ((id . 2) (name . "admin") (age . 24)))
		    '(name)
		    '(((name . "admin")) ((name . "egor")))
		    nil)
  ;; Тестирование чтения данных из таблицы с условием.
  (test-drop-table 'test-table1211 '((id integer) (name string)) 1)
  (test-select-from 'test-table1211
		    '((id integer) (name string) (age integer))
		    '(((id . 1) (name . "egor") (age . 19))
		      ((id . 2) (name . "admin") (age . 24))
		      ((id . 3) (name . "alex") (age . 26)))
		    '(name)
		    '(((name . "admin")))
		    '((> age 23) (/= name "alex")))
  ;; Тестирование удаления таблицы.
  (test-drop-table 'table1 '((id integer) (name string)) 1)
  ;; Тестирование чтения всех данных из таблицы.
  (test-select-all 'table1 '((id integer) (name string))
		   '(((id . 1) (name . "adm"))
		     ((id . 2) (name . "adm1")))
		   '(((id . 2) (name . "adm1"))
		     ((id . 1) (name . "adm"))))
  ;; Тестирование функции обновления данных по id
  (test-drop-table 'table999 '((id integer) (name string) (age integer)) 1)
  (test-update-data 'table999 '((id integer) (name string) (age integer))
		    '(((id . 1) (name . "egor") (age . 19))
		      ((id . 2) (name . "admin") (age . 24)))
		    1
		    'name
		    "update-egor"
		    '((id . 1) (name . "update-egor") (age . 19)))
  ;; Тестирование функции создания копии таблицы
  (test-backup-table 'table2213 '((id integer) (name string) (age integer))
		     '(((id . 1) (name . "egor"))
		       ((id . 2) (name . "alex"))
		       ((id . 3) (name . "asd")))
		     'backup-table2213
		     1)
  ;; Тестирование функции записи таблицы в CSV файл
  (test-drop-table 'table2214 '((id integer) (name string) (age integer)) 1)
  (test-save-table-to-file 'table2214 '((id integer) (name string) (age integer))
			   '(((id . 1) (name . "alex") (age . 23))
			     ((id . 2) (name . "egor") (age . 24))
			     ((id . 3) (name . "vova") (age . 25))
			     ((id . 4) (name . "daniil") (age . 12)))
			   "output.txt"
			   1)
  (test-drop-table 'tab1 '((id integer) (name string) (age integer)) 1)
  (mk-sel-from)
  		   
  (format t "~%Тестирования завершенно.~%"))
