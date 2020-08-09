;;; elmpd-tests.el --- ERT tests for elmpd  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO(sp1ff): COMMENTARY-HERE

;;; Code:

(require 'ert)

(ert-deftest elmpd-test-smoke-test ()
  "Sample test."
    (should (eq 2 (+ 1 1))))

(provide 'elmpd-tests)

;;; elmpd-tests.el ends here
