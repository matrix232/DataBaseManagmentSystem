(defvar command)
(defvar *table*)
(defvar *database* (make-hash-table :test 'equal)) 

(defstruct table
  name
  rows
  columns)

(format t "Enter command: ~%")
(setq command (read))

(defun create-table (name rows cells)
  (let ((new-table (make-table :name name :rows rows :columns cells)))
    (setf (gethash name *database*) new-table)
    (format t "Table ~a created with size ~a and ~a~%" name rows cells)))

(defun write-data (row column)
  (format t "Enter value: ")
  (let ((value (read)))
      (setf (aref *table* row column) value)))

(defun read-data (row column)
  (print (aref *table* row column)))

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



