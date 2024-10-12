(defvar command)
(defstruct table
  name
  rows
  columns)

(defvar *table*)
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
  (let ((datas (getf *database* table-name)))
    (push data datas)
    (setf (getf *database* table-name) datas)))

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



