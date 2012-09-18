;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun kill-all-buffers-mercilessly ()
  "*DANGEROUS* function that kills all the buffers mercilessly

I suggest you to DO NOT bind it to any keyboard shortcut and
please, be careful, once called, it can't be stopped!"
  (interactive)
  (mapcar '(lambda (b)
             (ignore-errors
               (revert-buffer 1 1))
             (kill-buffer b))
          (buffer-list)))
