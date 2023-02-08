(progn
  (find-file "./zemp.el")
  (load-file "./zemp-player-mplayer.el")
  (eval-buffer)
  (add-hook 'after-save-hook #'(lambda ()(eval-buffer)) nil t)
  (select-window (split-window (get-buffer-window) nil 'right))
  (call-interactively 'zemp-open-dir)
  (select-window (get-buffer-window "zemp.el"))
  )
