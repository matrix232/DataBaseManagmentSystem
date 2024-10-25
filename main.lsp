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
  (unless (gethash name *database*)
    (setf (gethash name *database*) (make-table :name name :columns columns :rows nil)))
  (format t "TABLE CREATED~%"))

(defun create-column (table-name column-name column-type)
  (let ((table (gethash table-name *database*)))
    (when table
      (push (cons column-name column-type) (table-columns table))  ; Добавляем колонку в список столбцов таблицы (column-name . column-type)
      (setf (gethash table-name *database*) table))))  ; Обновляем таблицу в базе данных

; Функции для работы с данными
(defun add-data (table-name data)
  (let ((table (gethash table-name *database*)))
    (when table
      (push data (table-rows table))  ; Добавляем данные в строки таблицы
      (setf (gethash table-name *database*) table))))

(defun read-data (table-name &key index)
  "Чтение данных. Если index не указан или nil - вернет все записи таблицы."
  (let ((table (gethash table-name *database*)))
    (when table
      (let ((data-list (table-rows table)))
        (if index
            (nth index data-list)  ; Возвращаем запись по индексу
            data-list)))))

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

;(defun create-database-file (file-name))

;(defun load-database-file (filename))

(defun handler (comnd)
  "Обработчик команд."
  (cond
    ((string= comnd "CREATE")
     (progn
       (print "CREATE command")))
    ((string= comnd "READ")
     (progn
       (print "READ command")))
    ((string= comnd "WRITE")
     (progn
       (print "WRITE command")))
    (t (print "Unknown command"))))

(defun create-user-table ()
  (create-table 'users '((id integer primary key)
                         (username string)
                         (password string)
                         (email string))))

(create-user-table)
(if (table-exist 'users)
    (progn
      (format t "Таблица 'users' существует~%")
      ;(add-data 'users '(0 "admin" "adminpass" "admin@admin.com"))
					;(add-data 'users '(1 "admin1" "admin1pasww" "admin1@admin.com"))
      ;(remove-data 'users :index  2)
      (format t "Данные таблицы 'users': ~A~%" (read-data 'users)))
    (format t "Таблица 'users' не существует!"))
		 


