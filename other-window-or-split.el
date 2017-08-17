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

;; (global-set-key (kbd "C-t") 'other-window-or-split)

;;; Code:

(defun split-window-vertically-n (num_wins)
  (interactive "nsplitnum:")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))

(defun split-window-horizontally-n (num_wins)
  (interactive "nsplitnum:")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

(defun enstandard-split-num (split-num)
  (if (<= 2 split-num)
      split-num
    2))

(defun split-window-dwim ()
  (interactive)
    (if (>= (window-body-width) (*(window-body-height) 2))
      (split-window-horizontally-n
       (enstandard-split-num (floor (/ (window-body-width) 70))))
    (split-window-vertically-n
     (enstandard-split-num (floor (/ (window-body-height) 50))))))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-dwim))
  (other-window 1))

(defun previous-other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-dwim))
  (other-window -1))

(defun adjust-windows-size ()
  (interactive)
  (balance-windows-area)
  (balance-windows))

(provide 'other-window-or-split)
;;; other-window-or-sprit.el ends here












