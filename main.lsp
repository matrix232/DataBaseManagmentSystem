(defstruct table
  name
  rows
  columns)

(defvar *database* (make-hash-table :test 'equal))

; Функции для работы с таблицей
(defun create-table (name &optional columns)
  (unless (gethash name *database*)
    (setf (gethash name *database*) (make-table :name name :columns columns :rows nil))))

(defun insert-into (table-name row-data)
  (let ((table (gethash table-name *database*)))
    (if table
	(let* ((columns (table-columns table))
	  (column-names (get-column-names columns)))
	  ;;Проверка, что все переданные ключи есть в колонках таблицы
	  (if (every (lambda (col) (member col column-names :test 'equal)) (mapcar #'car row-data))
	      (progn
		(push row-data (table-rows table)))
	      (format t "ERROR: Some columns is not exist in table ~a~%" table-name)))
	(format t "ERROR: Table ~a not found!~%" table-name))))

(defun select-from (table-name &rest columns)
  (let ((table (gethash table-name *database*)))
    (if table
        (let* ((table-columns (get-column-names (table-columns table)))
               (missing-cols (remove-if (lambda (col) (member col table-columns :test 'equal)) columns)))
          (if (null missing-cols)
              (mapcar (lambda (row) (remove-if-not (lambda (pair)(member (car pair) columns :test 'equal)) row)) (table-rows table))
              (progn
                (format t "ERROR: Columns ~a not found in table ~a~%" missing-cols table-name)
                nil)))
        (progn
          (format t "ERROR: Table ~a not found!~%" table-name)
          nil))))

	      
(defun delete-from (table-name cond-pair)
  (let ((table (gethash table-name *database*)))
    (if table
	(let ((new-rows (remove-if (lambda (row) (every (lambda (condit) (equal (cdr condit) (cdr (assoc (car condit) row)))) cond-pair)) (table-rows table))))
	  (setf (table-rows table) new-rows)
	  (format t "Rows matching ~a deleted from table ~a.~%" cond-pair table-name) new-rows)
	(format t "ERROR: Table ~a not found!~%" table-name))))

(defun drop-table (table-name)
  (if (gethash table-name *database*)
      (progn
	(remhash table-name *database*)
	(format t "Table ~a dropped!~%" table-name))
      (format t "ERROR: Table ~a not found!~%" table-name)))

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
  (create-table "users1" '(("id" integer primary key)
                         ("username" string)
                         ("password" string)
                         ("email" string))))

;(create-user-table)
;(if (table-exist "users1")
;    (progn
 ;     (format t "Таблица 'users1' существует~%")
;      (insert-into "users1" '(("id" . 1) ("username" . "admin") ("password" . "passADM") ("email" . "adm@mail.ru")))
					;(insert-into 'users '((id . 2) (username . "Egor") (password . "passw") (email . "egor@mail.ru")))
;      (format t "ДАТА ~a~%" (select-from "users1" "username" "password")))
;    (format t "Таблица 'users1' не существует!"))
		 


