(defvar command)
(defstruct table
  name
  rows
  columns)

(defvar *database* (make-hash-table :test 'equal)) 

(format t "Enter command: ~%")
(setq command (read))

; Функции для работы с таблицей
(defun create-table (name &optional columns)
  (unless (find name *tables* :key #'first)
    (push (cons name columns) *database*))
  (format t "TABLE CREATED~%"))

(defun create-column (table-name column-name column-type)
  (let ((columns (second (assoc table-name *database*))))
    (push (cons column-name column-type) columns) ;(column-name . column-type)
    (setf (second (assoc table-name *database*)) columns)))

; Функции для работы с данными
(defun add-data (table-name data)
  (let ((data-list (getf *database* table-name)))
    (push data data-list)
    (setf (getf *database* table-name) data-list)))

(defun read-data (table-name &key index)
  ; Чтение данных. Если index не указан или nil - вернет все записи таблицы
  (let ((data-list (getf *database* table-name)))
    (when data-list
      (when index (return-from read-data (nth index data-list)))
      (return-from read-data data-list))))

(defun remove-data (table-name &key index)
  (let ((data-list (getf *database* table-name)))
    (when data-list
      (if index
	  (setf (getf *database* table-name) (remove (elt data-list index) data-list :count 1))
	  (setf (getf *database* table-name) (remove data-list :count 1))))))

(defun update-data (table-name index field-name new-value)
  (let ((data-list (read-data table-name :index index)))
    (setf (slot-value data-list field-name) new-value))) 
	

(defun handler (comnd)
  ; Обработчик команд 
  (cond
    ((string= comnd "CREATE")
     (progn
       (print "CREATE command")
       (create-table "users" nil 3)))
    ((string= comnd "READ")
     (progn
       (print "READ command")
       (read-data 0 0)))
    ((string= comnd "WRITE")
     (progn
       (print "WRITE command")
       (write-data 0 0)
       (read-data 0 0)))
    (t (print "Unknow command"))))
  

(handler command)



