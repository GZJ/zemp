;;; zemp-player-mplayer.el --- zemp mplayer -*- lexical-binding: t; -*-

;; Copyright (C) 2022 GZJ

;; Author: GZJ <gzj00@outlook.com>
;; Keywords: music player
;; Version: 1.0.0

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

;; zemp mplayer backend.

;;; Code:

(setq zemp-player "mplayer")
(setq zemp-player-buffer "zemp-mplayer")

(defun zemp-play(track)
  (interactive)
  (if (get-buffer "zemp-mplayer")
      (zemp-stop)
    )
  (message track)
  (start-process "mplayer" "zemp-mplayer" "mplayer" "-slave" "-quiet" "-really-quiet" track)
  )

(defun zemp-resume()
  (interactive)
  (zemp-player-send-cmd "pause\n")
  )

(defun zemp-pause()
  (interactive)
  (zemp-player-send-cmd "pause\n")
  )

(defun zemp-stop()
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer "zemp-mplayer")
    )
  )

(defun zemp-seek-forward()
  (interactive)
  (zemp-player-send-cmd "seek 10\n")
  )

(defun zemp-seek-back()
  (interactive)
  (zemp-player-send-cmd "seek -10\n")
  )

(defun zemp-volumn (num)
  (interactive)
  (zemp-player-send-cmd (format "volume %d 1\n" num))
  )

(defun zemp-volumn-decrease ()
  (interactive)
  (zemp-player-send-cmd "volume -10\n")
  )

(defun zemp-volumn-increase ()
  (interactive)
  (zemp-player-send-cmd "volume +10\n")
  )

(defun zemp-player-send-cmd(cmd)
  (process-send-string (get-buffer-process zemp-player-buffer) cmd)
  )

(provide 'zemp-player-mplayer)
;;; zemp-player-mplayer.el ends here
