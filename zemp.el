;;; zemp.el --- A simple emacs player   -*- lexical-binding: t; -*-

;; Copyright (C) 2022 GZJ

;; Author: GZJ <gzj00@outlook.com>
;; Keywords: music player

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; zemp is a minimalist music player.
;; It uses a three-party multimedia player as the backend(default: mplayer), buffer as the frontend, and uses the directory as a playlist to play music.

;;; Code:
;;;; --------------------- require -------------------
(require 'button)

;;customize
(defgroup zemp nil
  "zemp group"
  :prefix "zemp-"
  :group 'multimedia
  :group 'applications
  )

(defcustom zemp-player-package "mplayer"
  "zemp player"
  :type 'string
  :set (lambda (symbol value)
	 (let ((p (intern (concat "zemp-player-" value))))
	   (if (featurep p)
	       (progn (unload-feature p 'force)
		      (require p))
	     (require p)
	     )
	   )
	 (setq zemp-player-package value)
	 )
  :group 'zemp)

;;;; --------------------- var -------------------
;; var buffer-local
(setq zemp-path "")

;; var global
(setq zemp-active-buffer nil)
(setq zemp-current-track nil)
(setq zemp-current-track-name "")
(setq zemp-current-track-path "")

(setq zemp-play-mode 'Seq) ;;Seq Shuffle Loop Repeat
(setq zemp-play-mode-table '((Seq . zemp-play-seq) (Shuffle . zemp-play-shuffle) (Loop . zemp-play-loop) (Repeat . zemp-play-repeat)))

(defconst zemp-version "1.1.0"
  "Version number of zemp.")
;;;; ---------------------mode key map -------------------
;;;;; mode map
(defvar zemp-mode-map nil "Keymap for `zemp-mode'")
(progn
  (setq zemp-mode-map (make-sparse-keymap))
  (define-key zemp-mode-map (kbd "j") 'next-line)
  (define-key zemp-mode-map (kbd "k") 'previous-line)

  (define-key zemp-mode-map (kbd "g") 'zemp-playlist-update)
  (define-key zemp-mode-map (kbd "n") 'zemp-playlist-next)
  (define-key zemp-mode-map (kbd "p") 'zemp-playlist-prev)
  (define-key zemp-mode-map (kbd "J") 'zemp-playlist-next)
  (define-key zemp-mode-map (kbd "K") 'zemp-playlist-prev)

  (define-key zemp-mode-map (kbd "s") 'zemp-stop)
  (define-key zemp-mode-map (kbd "SPC") 'zemp-toggle)
  (define-key zemp-mode-map (kbd "-") 'zemp-volumn-decrease)
  (define-key zemp-mode-map (kbd "+") 'zemp-volumn-increase)
  (define-key zemp-mode-map (kbd "<right>") 'zemp-seek-forward)
  (define-key zemp-mode-map (kbd "<left>") 'zemp-seek-back)
  (define-key zemp-mode-map (kbd "l") 'zemp-seek-forward)
  (define-key zemp-mode-map (kbd "h") 'zemp-seek-back)
  )

;;;;; define mode
;;;###autoload
(define-derived-mode zemp-mode nil
  "zemp simple emacs player"
  (use-local-map zemp-mode-map)
  )
;;;; --------------------- function -------------------
(defun zemp-version()
  (interactive)
  (message zemp-version)
  )

(defun zemp-player-packages-list ()
  (let ((package-prefix "zemp-player-")
        (found-packages '()))
    (dolist (dir load-path)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir nil "^[^.]"))
          (when (and (string-prefix-p package-prefix file)
                     (string-suffix-p ".el" file))
            (let ((package-name (substring file (length package-prefix) -3)))
              (add-to-list 'found-packages package-name t))))))
    found-packages))

(defun zemp-player-package-select ()
  (let ((pkgs (zemp-player-packages-list)))
    (if pkgs
        (completing-read "player: " pkgs)
      (error "No package found."))))

(defun zemp-player-package-set ()
  "set zemp player"
  (interactive)
  (let ((pkg (zemp-player-package-select)))
    (customize-set-variable 'zemp-player-package pkg)
    (message pkg)
    )
  )

(defun zemp-kill ()
  "Delete all buffers starting with \"zemp-\"."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "zemp-" (buffer-name buffer))
      (kill-buffer buffer))))
;;;;; playlist buffer
(defun zemp-open-dir(path)
  (interactive "strack path:")
  (setq b  (switch-to-buffer (format "%s-%s" "zemp" (file-name-nondirectory path))))
  (with-current-buffer b
    (progn
      (print (buffer-name (current-buffer)))
      )
    (zemp-playlist-init (expand-file-name path))
    )
  b
  )

(defun zemp-playlist-init (path)
  (zemp-mode)
  (setq zemp-path path)
  (make-local-variable 'zemp-path)
  (set-face-underline 'button nil)

  (let ((inhibit-read-only t))
    (erase-buffer)
    (zemp-insert-track (zemp-get-mp3 path))
    )
  (read-only-mode 1)
  (zemp-modeline)
  )

(defun zemp-get-mp3(path)
  (let ((files  (directory-files path 'full))
	(notes ()))
    (mapcar (lambda (file)
	      (if (string-match-p "\\.mp3$" file)
		  (add-to-list 'notes file t)
		)
	      )
	    files
	    )
    notes
    ))

(defun zemp-insert-track(playlist)
  (mapcar (lambda (f)
	    (insert-button  (format "%s\n" (file-name-base f))
			    'action (lambda (x)
				      (zemp-play (button-get x 'url))
				      (if zemp-current-track
					  (overlay-put zemp-current-track 'face 'button)
					)
				      (setq zemp-active-buffer (current-buffer))
				      (setq zemp-current-track x)
				      (setq zemp-current-track-name (file-name-base (button-get x' url)))
				      (setq zemp-current-track-path (file-name-directory (button-get x' url)))

				      (overlay-put zemp-current-track 'face 'highlight )
				      (force-mode-line-update)
				      (zemp-play-mode-set zemp-play-mode)
				      )
			    'url f)
	    )
	  playlist
	  )
  )

(defun zemp-playlist-update()
  (interactive)
  (with-current-buffer (current-buffer)
    (print zemp-path)
    (zemp-playlist-init zemp-path)
    )
  )

(defun zemp-playlist-prev (&optional f)
  (interactive)
  (with-current-buffer zemp-active-buffer
    (let ((pb (previous-button (button-start zemp-current-track))))
      (if pb
	  (button-activate pb)
	(eval f)
	)
      )
    )
  )

(defun zemp-playlist-next (&optional n)
  (interactive)
  (with-current-buffer zemp-active-buffer
    (let ((nb (next-button (button-start zemp-current-track))))
      (if nb
	  (button-activate nb)
	(eval n)
	)
      )
    )
  )

;;;;; modeline
(defun zemp-modeline ()
  (setq mode-line-format
	'(
	  (:eval
           (propertize  (concat "mode:" (symbol-name zemp-play-mode))
			'mouse-face 'mode-line-highlight
			'keymap zemp-modeline-playmode-map))
	  " | "
	  (:eval
           (propertize "play"
		       'mouse-face 'mode-line-highlight
		       'keymap zemp-modeline-play-map))
	  " | "
	  "♬"
	  " "
	  zemp-current-track-name
	  )
	)
  )

(defvar zemp-modeline-play-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'zemp-resume)
    map))

(defvar zemp-modeline-playmode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'zemp-play-mode-toggle)
    map))


;;;; --------------- play mode --------------------
;;;;; set play mode
(defun zemp-play-mode-toggle()
  (interactive)
  (let ((seq (cl-position  (assoc zemp-play-mode zemp-play-mode-table) zemp-play-mode-table)))
    (if ( eq (length zemp-play-mode-table) (+ seq 1))
	(setq seq 0)
      (setq seq (+ seq 1))
      )
    (zemp-play-mode-set (car (car (nthcdr seq zemp-play-mode-table))))
    )
  )

(defun zemp-play-mode-seq()
  (interactive)
  (zemp-play-mode-set 'Seq)
  )

(defun zemp-play-mode-loop()
  (interactive)
  (zemp-play-mode-set 'Loop)
  )

(defun zemp-play-mode-shuffle ()
  (interactive)
  (zemp-play-mode-set 'Shuffle)
  )

(defun zemp-play-mode-repeat()
  (interactive)
  (zemp-play-mode-set 'Repeat)
  )

(defun zemp-play-mode-set(mode)
  (setq zemp-play-mode mode)
  (unless (eq nil (get-buffer-process zemp-player-buffer))
    (set-process-sentinel (get-buffer-process zemp-player-buffer) (cdr (assoc mode zemp-play-mode-table)))
    )
  )

;;;;; sequential playlist
(defun zemp-play-seq (process event)
  (zemp-process-run process event '(zemp-playlist-next))
  )

;;;;; loop playlist
(defun zemp-play-loop (process event)
  (zemp-process-run process event '(zemp-playlist-next '(button-activate (button-at (point-min)))))
  )

;;;;; random playlist
(defun zemp-play-shuffle(process event)
  (zemp-process-run process event
		    '(progn
		       (goto-line (random (count-lines (point-min) (point-max))))
		       (if (button-at (point))
			   (button-activate (button-at (point)))
			 (if (next-button (point))
			     (button-activate (nex-button (point)))
			   (button-activate (button-at (point-min)))
			   )
			 )
		       )
		    )
  )

;;;;; repeat a song
(defun zemp-play-repeat (process event)
  (zemp-process-run process event
		    '(with-current-buffer zemp-active-buffer
		       (button-activate  zemp-current-track))
		    )
  )

(defun zemp-process-run(process event c)
  (cond ((string= event "finished\n")
	 (eval c)
	 )
	((string= event "killed\n"))
	)
  )

(provide 'zemp)
;;; zemp.el ends here
