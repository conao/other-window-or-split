;;; other-window-or-split.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Conao

;; Author: Conao
;; Keywords: 

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

;; (global-set-key (kbd "C-t") 'ws-other-window-or-split)

;;; Code:

;; variables
(defvar ws-split-window-or-split-version 3.0
  "split-window-or-split version")

(defvar ws-split-window-width-with-em 90
  "minimam window width with split-window-dwim")

(defvar ws-split-window-hight-with-em 50
  "minimam window hight with split-window-dwim")

;; functions
(defun ws-split-window-vertically-n (num_wins)
  (interactive "nsplitnum:")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))

(defun ws-split-window-horizontally-n (num_wins)
  (interactive "nsplitnum:")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

(defun ws-enstandard-split-num (split-num)
  (if (<= 2 split-num)
      split-num
    2))

(defun ws-split-window-dwim ()
  (interactive)
    (if (>= (window-body-width) (*(window-body-height) 2))
      (split-window-horizontally-n
       (enstandard-split-num (floor (/ (window-body-width) split-window-width-with-em))))
    (split-window-vertically-n
     (enstandard-split-num (floor (/ (window-body-height) split-window-hight-with-em))))))

(defun ws-other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-dwim))
  (other-window 1))

(defun ws-previous-other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-dwim))
  (other-window -1))

(defun ws-adjust-windows-size ()
  (interactive)
  (balance-windows-area)
  (balance-windows))

;; http://d.hatena.ne.jp/mooz/20100119/p1
;; http://d.hatena.ne.jp/khiker/20100119/window_resize
(defun ws-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
(provide 'other-window-or-split)
;;; other-window-or-sprit.el ends here
