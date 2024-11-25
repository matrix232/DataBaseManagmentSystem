(defpackage :DBMS-package
  (:use :cl))

(defstruct table
  name
  rows
  columns)

(defvar *database* (make-hash-table :test 'equal))

; Функции для работы с таблицей
(defun create-table (name &optional columns)
  (progn
    (unless (gethash name *database*)
      (setf (gethash name *database*) (make-table :name name :columns columns :rows nil)))
    1))

(defun insert-into (table-name row-data)
  (let ((table (gethash table-name *database*)))
    (if table
	(let* ((columns (table-columns table))
	  (column-names (get-column-names columns)))
	  ;;Проверка, что все переданные ключи есть в колонках таблицы, ключи приводятся из строкового формата в знаковый
	  (if (every (lambda (col) (member (intern (string-upcase (string col))) column-names :test 'eq)) (mapcar #'car row-data))
	      (progn
		(push row-data (table-rows table)))
		;; (format t "Rows ~a inserted into~%" row-data))
	      (format t "ERROR: Some columns is not exist in table ~a~%" table-name)))
	(format t "ERROR: Table ~a not found!~%" table-name))))

(defun select-from (table-name columns &optional condition)
  (let ((table (gethash table-name *database*)))
    (if table
	(let ((table-columns (mapcar #'car (table-columns table))))
	  (if (every (lambda (col) (member (intern (string-upcase (string col))) table-columns :test 'eq)) columns)
              (let ((filtered-rows
		      ; Фильтрация строк по условиям, если условий нет - возвращает выбранные колонки.
		      (if condition
			  (remove-if-not (lambda (row) (every (lambda (condit) (equal (cdr condit) (cdr (assoc (car condit) row)))) condition)) (table-rows table))
			  (table-rows table))))
		; Отображение выбранных строк.
		(mapcar (lambda (row) (remove-if-not (lambda (pair) (member (car pair) columns :test 'equal)) row)) filtered-rows))
              (progn
		(format t "ERROR: Columns ~a not found in table ~a.~%" columns table-name)
		nil)))
	(progn
	  (format t "ERROR: Table ~a not found!~%" table-name)
	  nil))))
 
(defun select-all (table-name)
  (let ((table (gethash table-name *database*)))
    (if table
	(table-rows table)
	(progn
	  (format t "ERROR: Table ~a not found!~%")
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
	(format t "Table ~a dropped!~%" table-name)
	1)
      (progn
	(format t "ERROR: Table ~a not found!~%" table-name)
	nil)))

(defun update (table-name id field-name new-value)
  "Обновляет значение поля field-name для строки с указанным id."
  (let ((table (gethash table-name *database*)))
    (if table
        (let ((rows (table-rows table))
              (updated-row nil))
          (dolist (row rows (or updated-row nil)) 
            (when (equal (cdr (assoc 'id row)) id)
              (setf (cdr (assoc field-name row)) new-value) 
              (setf updated-row row))) 
          (if updated-row
              (progn
                (setf (table-rows table) rows)
                (setf (gethash table-name *database*) table)
                (format t "UPDATE SUCCESSFUL: Table ~a, ID ~a, Field ~a -> ~a~%"
                        table-name id field-name new-value)
                updated-row)
              (progn
                (format t "ERROR UPDATE~%")
                (format t "No row with id ~a found in table ~a.~%" id table-name)
                nil)))
        (progn
          (format t "ERROR: Table ~a not found!~%" table-name)
          nil))))


(defun table-exist (table-name)
  (not (null (gethash table-name *database*))))

(defun get-column-names (columns)
  (mapcar (lambda (col) (intern (string (car col)))) columns))

;(defun create-database-file (file-name))

;(defun load-database-file (filename))




