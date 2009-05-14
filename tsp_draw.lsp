;выводит конкретную стоянку
(defun getNote (x y n koef LCircle LText)
  (entmake (list (vl-list* 0 "CIRCLE") (list 10 x y 0.0) (vl-list* 40 0.25) (vl-list* 8 LCircle)))
  (entmake (list (vl-list* 0 "MTEXT") (vl-list* 100 "AcDbEntity") (vl-list* 100 "AcDbMText") (list 10 (- x 5) (- y (/ koef 2)) 0.0) (vl-list* 1 (strcat "Cm." (itoa n))) (vl-list* 8 LText))) ;ЗАЧЕМ 100 ДВА РАЗА???
  )

;выводит все позиции
(defun makeStr (L k m Bkr Bsr koef rasst kolvo / x y i n flag par s LCircle LText)
  (setq LCircle "TSP_position" LText "TSP_txt")
  (entmake (list (vl-list* 0 "LAYER") (vl-list* 100 "AcDbSymbolTableRecord") (vl-list* 100 "AcDbLayerTableRecord") (vl-list* 2 LCircle) (vl-list* 70 0) (vl-list* 62 1)))
  (entmake (list (vl-list* 0 "LAYER") (vl-list* 100 "AcDbSymbolTableRecord") (vl-list* 100 "AcDbLayerTableRecord") (vl-list* 2 LText) (vl-list* 70 0) (vl-list* 62 3)))
  ;масштабирование
  (setq L (* L koef) k (* k koef) Bkr (* Bkr koef) Bsr (* Bsr koef))
  (setq i 1 j 1 n 1)

  ;позиция 1
  ;стоянки для крана при монтаже колонн
  (while (<= j (1+ m))
	 ;разделение на четные и не четные ряды.
	 ;в нечетных нумерация слева-направа, в четных - справа-налево
	 (if (= (/ j 2) (/ j 2.0)) (setq flag 0) (setq flag 1))
	 ;(if (or (= j 1) (= j (1+ m))) (setq s Bkr) (setq s Bsr)) 
	 (setq s Bkr)
	 ;начальная координата по x в зависимости от четности ряда
	 (if (= flag 1) (setq x (- 0 (* s 1.5)) par 1)
	   (setq x (+ (* s 1.5) L) par -1)
	   )
	 ;если шаг колонн крайних и средних рядов различается, 
	 ;то на средних координата по x ставится под колоннами
	 (if (and (/= Bkr Bsr) (/= j 1) (/= j (1+ m))) (setq x (- x (* s 0.5 par))))
	 (setq i 1)
	 (setq y (- (* k (- m j -1)) (* 4 koef)))
	 (while (<= i (/ L (* 2 s)))
		(setq x (+ x (* 2 s par)))
		(getNote x y n koef LCircle LText)
		(setq i (1+ i) n (1+ n))
		)
	 ;если количество колонн вдлину четное число,
	 ;то добавляем еще одну подпись под последней колонной
	 (if (= (/ L Bkr 2) (/ L Bkr 2.0))
	   (progn
	     (if (= par 1) (setq x L) (setq x 0))
	     (getNote x y n koef LCircle LText)
	     (setq n (1+ n))
	     )
	   )
	 (setq j (1+ j))
	 ) 

  ;2 позиция
  ;монтаж элементов покрытия
  ;расстояние и количество нужно считать вручную
  ;и задать в диалоговом окне
  (setq rasst (* rasst koef))
  (setq i 1 j 1)
  (while (<= j m)
	 (if (= (/ j 2) (/ j 2.0)) (setq flag 0) (setq flag 1))
	 (setq s rasst)
	 (if (= flag 1) (setq x (- 0 Bsr (* -1 s)) par 1)
	   (setq x (+ s (* -1 s) Bsr L) par -1)
	   )
	 (setq i 1)
	 (setq y (* k (- m j -0.5)))
	 (while (<= i kolvo)
		(setq x (+ x (* Bsr par)))
		(getNote x y n koef LCircle LText)
		(setq i (1+ i) n (1+ n)) 
		)
	 (setq j (1+ j))
	 )


  ;3 позиция
  ;монтаж стеновых панелей
  ;верхние стояки
  (setq s Bkr i 1 y (+ (* k m) (* 7 koef)) x (* -1 s))
  (while (<= i (/ L (* 2 s)))
	 (setq x (+ x (* 2 s)))
	 (getNote x y n koef LCircle LText)
	 (setq n (1+ n) i (1+ i))
	 )
  (if (/= (/ L Bkr 2) (/ L Bkr 2.0))
    (progn
      (setq x L)
      (getNote x y n koef LCircle LText)
      (setq n (1+ n))
      )
    )
  ;нижние стоянки
  (setq i 1 y (* -7 koef) x (+ L s))
  (while (<= i (/ L (* 2 s)))
	 (setq x (- x (* 2 s)))
	 (getNote x y n koef LCircle LText)
	 (setq n (1+ n) i (1+ i))
	 )
  (if (/= (/ L Bkr 2) (/ L Bkr 2.0))
    (progn
      (setq x 0)
      (getNote x y n koef LCircle LText)
      (setq n (1+ n))
      )
    )

  (princ)
  )

(defun makeMark (L k m Bkr koef / i x y LName mark)
  (setq mark (list "А" "Б" "В" "Г" "Д" "Е"))
  (setq L (* L koef) k (* k koef) Bkr (* Bkr koef) LName "TSP_mark")
  (entmake (list (vl-list* 0 "LAYER") (vl-list* 100 "AcDbSymbolTableRecord") (vl-list* 100 "AcDbLayerTableRecord") (vl-list* 2 LName) (vl-list* 70 0) (vl-list* 62 5)))

  ;горизонтальные отметки: 1,2,3,....
  (setq i 1)
  (setq y (- (* -7 koef) 17))
  (while (<= i (1+ (/ L Bkr)))
	 (setq x (* (1- i) Bkr))
	 (entmake (list (vl-list* 0 "LINE") (list 10 x y) (list 11 x (- y 7)) (vl-list* 8 LName)))
	 (entmake (list (vl-list* 0 "CIRCLE") (list 10 x (- y 12)) (vl-list* 40 5) (vl-list* 8 LName)))
	 (entmake (list (vl-list* 0 "MTEXT") (vl-list* 100 "AcDbEntity") (vl-list* 100 "AcDbMText") (list 10 (- x 1) (- y 10)) (vl-list* 1 (itoa i)) (vl-list* 8 LName)))
	 (setq i (1+ i))
	 )

  ;вертикальные отметки: А,Б,В,...
  (setq i 1)
  (setq x (- (/ k -2) 17))
  (while (<= i (1+ m))
	 (setq y (* k (1- i)))
	(entmake (list (vl-list* 0 "LINE") (list 10 x y) (list 11 (- x 7) y) (vl-list* 8 LName)))
	 (entmake (list (vl-list* 0 "CIRCLE") (list 10 (- x 12) y) (vl-list* 40 5) (vl-list* 8 LName)))
	 (entmake (list (vl-list* 0 "MTEXT") (vl-list* 100 "AcDbEntity") (vl-list* 100 "AcDbMText") (list 10 (- x 13) (1+ y)) (vl-list* 1 (chr (+ 191 i))) (vl-list* 8 LName)))
	 (setq i (1+ i))
	 )
  )

;main()
(defun tsp_draw ( / L k m Bkr Bsr dcl_id koef)
  (load "tsp.lsp")
  (if (< (setq dcl_id (load_dialog "tsp.dcl")) 0) (exit))
  (if (not (new_dialog "tsp" dcl_id)) (exit))

  (action_tile "accept"
	       (strcat
		 "(myfunc 0 0 (setq L (atoi (get_tile \"L\"))) (* (setq k (atoi (get_tile \"k\"))) (setq m (atoi (get_tile \"m\"))))"
		 "(setq Bkr (atoi (get_tile \"Bkr\"))) (* k m) (setq koef (/ 1000.0 (atoi (get_tile \"scale\")))))"
		 "(myfunc 0 k L (* k (- m 2)) (setq Bsr (atoi (get_tile \"Bsr\"))) k koef)"
		 "(makeStr L k m Bkr Bsr koef (atoi (get_tile \"rasst\")) (atoi (get_tile \"kolvo\")))"
		 "(makeMark L k m Bkr koef)"
		 "(done_dialog)"
		 )
	       )
  (action_tile "cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
  )
