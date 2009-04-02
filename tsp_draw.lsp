(defun tsp_draw ( / dcl_id)
  (load "d:\\sergej\\src\\acad\\tsp.lsp")
  (if (< (setq dcl_id (load_dialog "d:\\sergej\\src\\acad\\tsp.dcl")) 0) (exit))
  (if (not (new_dialog "tsp" dcl_id)) (exit))
  
  (action_tile "accept"
    (strcat
      "(myfunc 0 0 (atoi (get_tile \"L\")) (* (atoi (get_tile \"k\")) (atoi (get_tile \"m\")))"
      "(atoi (get_tile \"Bkr\")) (* (atoi (get_tile \"k\")) (atoi (get_tile \"m\"))))"
      "(myfunc 0 (atoi (get_tile \"k\")) (atoi (get_tile \"L\")) (* (atoi (get_tile \"k\")) (- (atoi (get_tile \"m\")) 2))"
      "(atoi (get_tile \"Bsr\")) (atoi (get_tile \"k\")))"
      "(done_dialog)"
    )
  )
  (action_tile "cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
)
