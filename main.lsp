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
	  ;;Проверка, что все переданные ключи есть в колонках таблицы, ключи приводятся из  строкового формата в знаковый
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
          (if (every (lambda (col) 
                       (member (intern (string-upcase (string col))) table-columns :test 'eq)) 
		     columns)
              (let* ((filtered-rows
                      ;; Фильтрация строк по условиям
                       (if condition
			   (remove-if-not (create-condition condition) (table-rows table))
                          (table-rows table))))
                ;; Отображение выбранных строк
                (mapcar (lambda (row) (remove-if-not (lambda (pair) (member (car pair) columns :test 'equal))
                    row))
                 filtered-rows))
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

(defun create-index (table-name columns)
  (let ((table (gethash table-name *database*)))
    (if table
	(let ((index (make-hash-table :test 'equal)))
	  (dolist (row (table-rows table))
	    (let ((value (cdr (assoc columns row))))
	      (push row (gethash value index))))
	  (setf (gethash (list table-name columns) *database*) index)
	  (format t "Index created for column ~a in table ~a.~%" columns table-name))
	(progn
	  (format t "ERROR: Table ~a not found!~%" table-name)
	  nil))))

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

(defun backup-table (table-name backup-name)
  (let ((table (gethash table-name *database*)))
    (if table
	(progn
	  (setf (gethash backup-name *database*)
		(make-table    :name (table-name table)
			       :columns (copy-list (table-columns table))
			       :rows (copy-list (table-rows table))))
	  (progn
	    (format t "Backup of table ~a success, create backup table ~a!~%" table-name backup-name)
	    1))
	(progn
	  (format t "ERROR: Table ~a nor found!~%" table-name)
	  nil))))

(defun save-table-to-file (table-name file-path)
  (let ((table (gethash table-name *database*)))
    (if table
        (with-open-file (stream file-path :direction :output 
                               :if-exists :supersede
                               :if-does-not-exist :create)
          (let ((columns (table-columns table)))
            ;; Запись заголовков
            (format stream "~{~A~^,~}~%" (mapcar #'car columns))
            ;; Запись строк
            (dolist (row (table-rows table))
              (format stream "~{~A~^,~}~%" 
                      (mapcar (lambda (col) (cdr (assoc (car col) row)))  columns))))
          1)
        (format t "ERROR: Table ~a not found!~%" table-name))))			 

(defun table-exist (table-name)
  (not (null (gethash table-name *database*))))

(defun get-column-names (columns)
  (mapcar (lambda (col) (intern (string (car col)))) columns))

(defun filter (predicate rows)
  "Фильтрует строки rows, оставляя только те, которые удовлетворяют предикату predicate."
  (remove-if-not predicate rows))

(defun gen-vars (table)
  (mapcar (lambda (col)
	    `(,col (cdr (assoc ',col row))))
	  (mapcar #'car (table-columns table))))

(defmacro create-condition (conditions)
  `(lambda (row)
     (every (lambda (condition)
              (cond
                ;; Условие вида (< age 30)
                ((and (listp condition)
                      (symbolp (car condition))
                      (symbolp (cadr condition)))
                 (let* ((col (cadr condition))
                        (op (car condition))
                        (val (caddr condition))
                        (row-val (cdr (assoc col row))))
                   (case op
                     ((=) (equal row-val val))
                     ((>) (> row-val val))
                     ((<) (< row-val val))
                     ((>=) (>= row-val val))
                     ((<=) (<= row-val val))
                     ((/=) (not (equal row-val val)))
                     (otherwise (error "Unsupported operator: ~a~%" op)))))

                ;; Условие вида (name . "Egor")
                ((and (consp condition)
                      (symbolp (car condition)))
                 (let ((key (car condition))
                       (value (cdr condition)))
                   (equal value (cdr (assoc key row)))))

                ((or (stringp condition) (not (consp condition)))
                 (error "Unsupported condition format: ~a~%" condition))

                (t (error "Unsupported condition format: ~a~%" condition))))
            ,conditions)))

(defmacro make-select-from (table-name columns &body body)
  `(let ((table (gethash ,table-name *database*)))
     (if table
         (let ((table-columns (mapcar #'car (table-columns table))))
	   ;; Проверка наличия всех выбранных столбцов
           (if (every (lambda (col) 
                        (member (intern (string-upcase (string col))) table-columns :test 'eq)) 
                      ,columns)
               (let* ((filtered-rows
                       (remove-if-not (lambda (row) 
                                        (let () ;;,(gen-vars table) (id (cdr (assoc 'id row)))
                                          ,@body)) ;; Выполняем действия полученные из body
                                      (table-rows table))))
		 ;; Вывод столбцов
                 (mapcar (lambda (row) 
                           (remove-if-not (lambda (pair) 
                                            (member (car pair) ,columns :test 'equal))
                                          row))
                         filtered-rows))
               (progn
                 (format t "ERROR: Columns ~a not found in table ~a.~%" ,columns ,table-name)
                 nil)))
         (progn
           (format t "ERROR: Table ~a not found!~%" ,table-name)
           nil))))

(defmacro make-update (table-name updates &body body)
  `(let* ((table (gethash ,table-name *database*))
          (rows (table-rows table)))
     (let ((updated-rows
            (mapcar
             (lambda (row)
               (let ,(mapcar (lambda (col)
                               `(,col (cdr (assoc ',col row))))
                             (mapcar #'car (table-columns table)))
                 (if (progn ,@body) ; Выполняем условия
                     (let ((new-row (copy-list row)))
                       (dolist (update ',updates)
                         (let ((col (car update))
                               (val (cdr update)))
                           (setf (cdr (assoc col new-row)) val)))
                       new-row)
                     row))) ; Если условие не выполнено, строка остаётся неизменной
             rows)))
       (setf (table-rows table) updated-rows)
       1)))

(defmacro make-delete-from (table-name &body body)
  `(let ((table (gethash ,table-name *database*)))
     (if table
	 (let ((filtered-rows (remove-if-not (lambda (row) (let () ,@body))
					     (table-rows table))))
	   (setf (table-rows table) filtered-rows)
	   (format t "Rows deleted from table ~a based on condition.~%" ,table-name)
	   filtered-rows)
	 (progn
	   (format t "ERROR: Table ~a not found!~%" ,table-name)
	   nil))))




