;;; zemp-player-mpv.el --- zemp mpv -*- lexical-binding: t; -*-

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

;; zemp mpv backend.

;;; Code:

(setq zemp-player "mpv")
(setq zemp-player-buffer "zemp-mpv")
(setq zemp-player-mpvsocket "\\\\.\\pipe\\mpvsocket")
(defvar zemp-player-paused nil
  "A variable to track the player's pause state.")

(defun zemp-player-mpv-write-to-mpvsocket (data)
  (let* ((pipe-name zemp-player-mpvsocket)
         (command (format "echo %s > %s" data pipe-name))
         (proc (start-process-shell-command "zemp-player-mpv-pipe" nil command)))
    (set-process-sentinel proc (lambda (process event)
                                 (when (string= event "finished\n")
                                   (message "Data sent to mpv."))))))

(defun zemp-play(track)
  (interactive)
  (if (get-buffer zemp-player-buffer)
      (zemp-stop)
    )
  (message track)
  (start-process zemp-player zemp-player-buffer zemp-player "--no-config" "--no-loop-file"  "--no-video"  "--no-audio-display" "--force-window=no" (format "--input-ipc-server=%s" zemp-player-mpvsocket) track)
  )

(defun zemp-toggle ()
  "Toggle the player pause state."
  (interactive)
  (if zemp-player-paused
      (zemp-resume)
    (zemp-pause)
    )
  )

(defun zemp-resume()
  (interactive)
  (zemp-player-send-cmd "{\"command\": [\"set_property\", \"pause\", false]}")
  (setq zemp-player-paused nil)
  )

(defun zemp-pause()
  (interactive)
  (zemp-player-send-cmd "{\"command\": [\"set_property\", \"pause\", true]}")
  (setq zemp-player-paused t)
  )

(defun zemp-stop()
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer zemp-player-buffer)
    )
  (message "zemp stop")
  )

(defun zemp-seek-forward()
  (interactive)
  (zemp-player-send-cmd "{\"command\": [\"seek\", 10, \"relative\"]}")
  )

(defun zemp-seek-back()
  (interactive)
  (zemp-player-send-cmd "{\"command\": [\"seek\", -10, \"relative\"]}")
  )

(defun zemp-volumn (num)
  (interactive)
  (zemp-player-send-cmd (format "{\"command\": [\"set_property\", \"volume\", %s]}" num))  
  )

(defun zemp-volumn-decrease ()
  (interactive)
  (zemp-player-send-cmd "{\"command\": [\"add\", \"volume\", -10]}") 
  )

(defun zemp-volumn-increase ()
  (interactive)
  (zemp-player-send-cmd "{\"command\": [\"add\", \"volume\", 10]}")
  )

(defun zemp-player-send-cmd(cmd)
  (zemp-player-mpv-write-to-mpvsocket cmd)
  )

(provide 'zemp-player-mpv)
;;; zemp-player-mpv.el ends here
