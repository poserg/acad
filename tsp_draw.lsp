(defun tsp_draw ( / dcl_id koef txt i L k m Bkr Bsr x y)
  (load "tsp.lsp")
  (if (< (setq dcl_id (load_dialog "tsp.dcl")) 0) (exit))
  (if (not (new_dialog "tsp" dcl_id)) (exit))
  (setq koef (/ 1000.0 (atoi (get_tile "scale"))))
  (setq L (* (atoi (get_tile "L")) koef))
  (setq k (* (atoi (get_tile "k")) koef))
  (setq m (atoi (get_tile "m")))
  (setq Bkr (* (atoi (get_tile "Bkr")) koef))
  (setq Bsr (* (atoi (get_tile "Bsr")) koef))
 
  (setq txt "")

  (setq i 1)
  (while (<= i (/ L (* 2 Bkr)))
	 (setq x (rtos (+ (- 0 (* Bkr 1.5)) (* Bkr i 2)) 2 2))
	 (setq y (rtos (- (* k m) (* 4 koef)) 2 2))
	 (setq txt (strcat txt "(entmake '((0 . \"CIRCLE\") (10 " x " " y " 0.0) (40 . 0.25))) "))
	 (setq txt (strcat txt "(entmake '((0 . \"MTEXT\") (100 . \"AcDbEntity\") (100 . \"AcDbMText\") (10 " x " " y " 0.0) (1 . \"Cm. " (itoa i) "\")))"))
	 (setq i (1+ i))
  )
  (action_tile "accept"
    (strcat
      "(myfunc 0 0 L (* k m) Bkr (* k m) koef) "
      "(myfunc 0 k L (* k (- m 2)) Bsr k koef) "
      txt
      "(done_dialog)"
    )
  )
  (action_tile "cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
)
