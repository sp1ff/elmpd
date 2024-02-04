;;; elmpd-tests.el --- ERT tests for elmpd -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the basic `elmpd' unit test suite.  I'm developing tests
;; for the new command list features in test-cmd-lists.el.

;;; Code:

(require 'ert)
(require 'elmpd)
(require 'with-elmpd-test)

(ert-deftest elmpd-test-log ()
  "Test the `elmpd' logging facility."
  (let ((elmpd-log-level 'info)
        (elmpd-log-buffer-name "*elmpd-test-log*"))
    (elmpd-log 'info 'elmpd-test "%s %s %s" 'foo 'bar 'splat)
    (elmpd-log 'debug 'elmpd-test "%s %s %s" 'x 'y 'z)
    (let ((text
           (with-current-buffer elmpd-log-buffer-name
             (buffer-string))))
      (should (string-match "\\[[-0-9: ]+\\]\\[elmpd-test\\]\\[info\\] foo bar splat\n" text)))
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
    (should (eq 2 (length short-text)))
    (should (string= short-text "fo"))))

(ert-deftest elmpd-test-macro ()
  "Exercise `with-elmpd-test'."
  (let ((some-var 1))
    (with-elmpd-test
     "elmpd-test-macro"
     '(("ping\n" . "pong\nOK\n"))
     nil
     (elmpd-send conn "ping" (lambda (_c _o _t) (setq some-var 2))))
    (should (eq some-var 2))))

(ert-deftest elmpd-test-basics ()
  "Trivial test that nevertheless demonstrates testing
`elmpd-connection' against a mocked server."

  (with-elmpd-test
   "elmpd-test-basics"
   '(("consume 1\n" . "OK\n")
     ("consume 2\n" . "ACK [50@1] {consume} value must be 0 or 1\n"))
   nil
   (elmpd-send conn "consume 1")
   (elmpd-send conn "consume 2")))

(ert-deftest elmpd-test-idling ()
  "Test an idling `elmpd-connection' against a mocked server."

  (let* ((invoked)
         (cb
          (lambda (_conn subsys)
            (setq invoked t)
            (should (equal subsys '(player options)))))
         (idle-spec
          (cons 'all cb)))
    (with-elmpd-test
     "elmpd-test-idling"
     '(("idle\n" . "changed: player\nchanged: options\nOK\n")
       ("idle\n" . ""))
     idle-spec
     (should conn))
    (should invoked)))

(ert-deftest elmpd-test-issue-2 ()
  "Test my fix to <https://github.com/sp1ff/elmpd/issues/2>."

  (let ((msgs `(("get some stuff\n" .
                 ,(concat (make-string 49139 ?x) "\nOK\n")))))
    (with-elmpd-test
     "elmpd-test-issue2"
     msgs
     nil
     (let ((start (current-time)))
       (elmpd-send conn "get some stuff")
       (while (accept-process-output))
       (let* ((end (current-time))
              (elapsed (- (float-time end) (float-time start))))
         (message "elapsed is %f" elapsed)
         (should (< elapsed 1.0)))))))

(ert-deftest elmpd-test-local ()
  "Trivial test of `elmpd-connection' against a local (Unix) server."

  (let ((elmpd-log-level 'debug)
        (elmpd-log-buffer-name "*elmpd-test-local*"))
    (let* ((msgs '(("consume 1\n" . "OK\n")
                   ("consume 2\n" . "ACK [50@1] {consume} value must be 0 or 1\n")))
           (server
            (make-network-process
             :name "elmpd-test-local"
             :server t
             :local (format "/tmp/elmpd-tests.%d.sock" (emacs-pid))
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
           (sock (process-contact server :local))
           (conn (elmpd-connect :local sock)))
      (sit-for 1)
      (elmpd-send conn "consume 1")
      (elmpd-send conn "consume 2")
      (while (accept-process-output))
      (delete-process server)
      (should (not (process-live-p server))))))

(ert-deftest elmpd-test-connect-keywords ()
  "Test `elmpd-connect' arg validation."

  (should-error
   (elmpd-connect :host "localhost" :port 1234 :fozhizzle 'yes)))

(ert-deftest elmpd-test-pong ()
  "Test a corner case I found in error handling."

  (let ((got-error))
    (with-elmpd-test
     "elmpd-test-pong"
     '(("ping\n" . "OK\n")
       ("pong\n" . "ACK [5@0] {} unknown command \"pong\"\n"))
     nil
     (elmpd-send conn "ping")
     (elmpd-send conn "pong" (lambda (_conn ok _text)
                               (should (not ok)) (setq got-error t))))
    (should got-error)))

(ert-deftest elmpd-test-new-command ()
  "Test `elmpd--new-command."

  (let ((cmd (elmpd--new-command "ping")))
    (should (string= "ping" (elmpd-command--simple-command cmd)))
    (should (not (elmpd-command--compound-command cmd)))
    (should (not (elmpd-command--callback cmd)))
    (should (not (elmpd-command--callback-style cmd))))

  (let ((cmd (elmpd--new-command '("ping" "consume 0"))))
    (should (not (elmpd-command--simple-command cmd)))
    (should (equal (elmpd-command--compound-command cmd)
                '("ping" "consume 0")))))

(ert-deftest elmpd-test-parsing-sub-responses ()
  "Test `elmpd--parse-sub-responses'."

  (let ((sub (elmpd--parse-sub-responses "")))
    (should (equal (car sub) '()))
    (should (string= (cdr sub) "")))

  (let ((sub (elmpd--parse-sub-responses "foo\nlist_OK\nbar")))
    (should (equal (car sub) '("foo")))
    (should (string= (cdr sub) "bar")))

  (let ((sub (elmpd--parse-sub-responses "foo\nlist_OK
bar\nlist_OK\nOK\n")))
    (should (equal (car sub) '("foo" "bar")))
    (should (string= (cdr sub) "OK\n"))))

(ert-deftest elmpd-test-noidle ()
  "Test sending the noidle command."
  (with-elmpd-test
   "elmpd-test-noidle"
   '((idle "player")
     ("noidle\n" . "OK\n")
     ("ping\n" . "pong\nOK\n"))
   (cons 'player (lambda (_conn _ok _rsp)))
   (accept-process-output (elmpd-connection--fd conn))
   (elmpd-log 'info 'elmpd-tests "sending ping on %s..." (elmpd--pp-conn conn))
   (elmpd-send conn "ping")
   (elmpd-log 'info 'elmpd-tests "sending ping on %s...done." (elmpd--pp-conn conn))
   (elmpd-log 'error 'elmpd-tests "I am leaving the body forms.")))

(provide 'elmpd-tests)

;;; elmpd-tests.el ends here
