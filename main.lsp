(defstruct table
  name
  rows
  columns)

(defvar *database* (make-hash-table :test 'equal))

; Функции для работы с таблицей
(defun create-table (name &optional columns)
  (unless (gethash name *database*)
    (setf (gethash name *database*) (make-table :name name :columns columns :rows nil)))
  (format t "TABLE CREATED~%"))

(defun insert-into (table-name row-data)
  (let ((table (gethash table-name *database*)))
    (if table
	(let* ((columns (table-columns table))
	  (column-names (get-column-names columns)))
	  ;;Проверка, что все переданные ключи есть в колонках таблицы
	  (if (every (lambda (col) (member col column-names :test 'equal)) (mapcar #'car row-data))
	      (progn
		(push row-data (table-rows table))
		(format t "Row ~a inserted into table ~a.~%" row-data table-name))
	      (format t "ERROR: Some columns is not exist in table ~a~%" table-name)))
	(format t "ERROR: Table ~a not found!~%" table-name))))

(defun select-from (table-name &rest columns)
  "Извлекает данные из указанных колонок таблицы."
  (let ((table (gethash table-name *database*)))
    (if table
        (let* ((table-columns (get-column-names (table-columns table)))
               (missing-cols (remove-if (lambda (col) (member col table-columns :test 'equal)) columns)))
          (if (null missing-cols)
              (dolist (row (table-rows table))
                (format t "~a~%" (remove-if-not (lambda (pair) (member (car pair) columns :test 'equal)) row)))
              (format t "ERROR: Columns ~a not found in table ~a~%" missing-cols table-name)))
        (format t "ERROR: Table ~a not found!~%" table-name))))
	      
(defun remove-data (table-name &key index)
  (let ((table (gethash table-name *database*)))
    (when table
      (let ((data-list (table-rows table)))
        (if index
            (setf (table-rows table) (remove (elt data-list index) data-list :count 1))
            (setf (table-rows table) nil))  ; Если не указан индекс, удаляем все данные
        (setf (gethash table-name *database*) table)))))

(defun update-data (table-name index field-name new-value)
  (let ((table (gethash table-name *database*)))
    (when table
      (let ((data (nth index (table-rows table))))
        (setf (getf data field-name) new-value)  ; Обновляем поле в записи
        (setf (gethash table-name *database*) table)))))

(defun table-exist (table-name)
  (not (null (gethash table-name *database*))))

(defun get-column-names (columns)
  (mapcar #'car columns))

;(defun create-database-file (file-name))

;(defun load-database-file (filename))

(defun create-user-table ()
  (create-table "users" '(("id" integer primary key)
                         ("username" string)
                         ("password" string)
                         ("email" string))))

(create-user-table)
(if (table-exist "users")
    (progn
      (format t "Таблица 'users' существует~%")
      ;(insert-into 'users '((id . 1) (username . "admin") (password . "passADM") (email . "adm@mail.ru")))
					;(insert-into 'users '((id . 2) (username . "Egor") (password . "passw") (email . "egor@mail.ru")))
      (select-from "users" "username" "email"))
    (format t "Таблица 'users' не существует!"))
		 


