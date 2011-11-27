;;; creoletable.el --- orgtbl code for creole

;; Copyright (C) 2011  Nic Ferrier

;; Author: Nic Ferrier;; stuff <nferrier@ferrier.me.uk>
;; Keywords: hypermedia, outlines

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

;; Just some demo orgtbl stuff

;;; Code:


(ert-deftest nictest-orgtbl-export ()
  (with-temp-buffer
    (insert "
| col1 | col2 |
|   15 |   20 |
|   35 |      |
#+TBLFM: @3$1=@2$1 + 20
")
    (goto-char (point-min))
    (re-search-forward "^|")
    ;; Requires that we're back in the table
    (org-table-recalculate t)
    (let ((tbl (org-table-to-lisp)))
      (orgtbl-to-csv tbl '()))))

(provide 'creoletable)

;;; creoletable.el ends here
