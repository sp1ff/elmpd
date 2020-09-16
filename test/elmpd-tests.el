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

;; Consistent with the preliminary nature of the `elmpd' library, this
;; test suite is really just an outline.  It does, at least,
;; demonstrate how a more complete test suite *could* be constructed
;; by running an in-process mock MPD server.

;;; Code:

(require 'ert)
(require 'elmpd)

(ert-deftest elmpd-test-log ()
  "Test the `elmpd' logging facility."
  (let ((elmpd-log-level 'info)
        (elmpd-log-buffer-name "*elmpd-test-log*"))
    (elmpd-log 'info "%s %s %s" 'foo 'bar 'splat)
    (elmpd-log 'debug "%s %s %s" 'x 'y 'z)
    (let ((text
           (with-current-buffer elmpd-log-buffer-name
             (buffer-string))))
      (should (string-match "\\[[-0-9: ]+\\]\\[info\\] foo bar splat\n" text)))
    (elmpd-clear-log)
    (let ((text
           (with-current-buffer elmpd-log-buffer-name
             (buffer-string))))
      (should (eq 0 (length text)))))
  (let* ((long-text "foobarsplat")
         (short-text (elmpd--pp-truncate-string long-text 8)))
    (should (eq 8 (length short-text)))
    (should (string= short-text "fooba...")))
  (let* ((long-text "foobarsplat")
         (short-text (elmpd--pp-truncate-string long-text 2)))
    (message "short-text: %s" short-text)
    (should (eq 2 (length short-text)))
    (should (string= short-text "fo"))))

(ert-deftest elmpd-test-with-mock-server ()
  "Trivial test that nevertheless demonstrates testing
`elmpd-connection against a mocked server."

  (let ((elmpd-log-level 'debug)
        (elmpd-log-buffer-name "*elmpd-test-with-mock-server*"))
    (let* ((msgs '(("consume 1\n" . "OK\n")
                   ("consume 2\n" . "ACK [50@1] {consume} value must be 0 or 1\n")))
           (server
            (make-network-process
             :name "elmpd-test-with-mock-server"
             :server t
             :service t
             :family 'ipv4
             :sentinel
             (lambda (proc event)
               (if (and (> (length event) 8)
	                      (string= (substring event 0 9) "open from"))
                   (process-send-string proc "OK MPD 256.256.256\n")))
             :filter
             (lambda (proc text)
               (let ((in (caar msgs))
                     (out (cdar msgs)))
                 (setq msgs (cdr msgs))
                 (should (string= in text))
                 (process-send-string proc out)))))
           (port (process-contact server :service))
           (conn (elmpd-connect :host "localhost" :port port)))
      (message "elmpd-test-with-mock-server listening on port %d" port)
      (elmpd-send conn "consume 1")
      (elmpd-send conn "consume 2")
      (while (accept-process-output))
      (delete-process server)
      (should (not (process-live-p server))))))

(provide 'elmpd-tests)
;;; elmpd-tests.el ends here
