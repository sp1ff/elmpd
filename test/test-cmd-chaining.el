;;; test-cmd-chaining.el --- ERT tests for elmpd command chaining  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Michael Herstine <sp1ff@pobox.com>

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

;; These are the unit tests for `elmpd' command-list support.  The
;; primary unit test suite is in test-elmpd.el.

;;; Code:

(require 'ert)
(require 'elmpd)
(require 'with-elmpd-test)

(ert-deftest test-chain-smoke-0 ()
  "Smoke tests for `elmpd-chain'."

  ;; Start exercising the macro, building up complexity as we go:
  (with-elmpd-test
   "elmpd-chain-smoke-0"
   '(("ping\n" . "pong\nOK\n"))
   nil ; no idle
   (elmpd-chain conn "ping"))) ;; `with-elmpd-test' checks that the queue has been drained.

(ert-deftest test-chain-smoke-1 ()
  "Smoke tests for `elmpd-chain'."
  
  (let ((invoked))
    (with-elmpd-test
     "elmpd-chain-smoke-1"
     '(("ping\n" . "pong\nOK\n"))
     nil ; no idle
     (elmpd-chain
      conn
      ("ping"
       (lambda (_conn text)
         (should (string= text "pong\n"))
         (setq invoked t))
       default)))
    (should invoked)))

(ert-deftest test-chain-smoke-2 ()
  "Smoke tests for `elmpd-chain'."
  
  (let ((or-else-invoked)
        (and-then-invoked))
    (with-elmpd-test
     "elmpd-chain-smoke-2"
     '(("ping\n" . "pong\nOK\n")
       ("consume 1\n" . "OK\n"))
     nil ; no idle
     (elmpd-chain
      conn
      "ping"
      :or-else
      (lambda (_conn _err) (setq or-else-invoked t))
      :and-then
      ("consume 1"
       (lambda (_conn _text)
         (setq and-then-invoked t)))))
    (should (not or-else-invoked))
    (should and-then-invoked)))

(ert-deftest test-chain-smoke-3 ()
  "Smoke tests for `elmpd-chain'."

  (let ((invocation-count 0))
    (with-elmpd-test
     "elmpd-chain-smoke-3"
     '(("command_list_ok_begin\nping\nping\ncommand_list_end\n" .
        "pong\nlist_OK\npong\nlist_OK\nOK\n"))
     nil ; no idle
     (elmpd-chain
      conn
      ('("ping" "ping")
       (lambda (_conn rsp)
         (should (listp rsp))
         (should (eq 2 (length rsp)))
         (setq invocation-count (1+ invocation-count)))
       list)))
    (should (eq 1 invocation-count))))

(ert-deftest test-chain-smoke-4 ()
  "Smoke tests for `elmpd-chain'."

  (let ((invocation-count 0))
    (with-elmpd-test
     "elmpd-chain-smoke-4"
     '(("command_list_ok_begin\nping\nping\ncommand_list_end\n" .
        "pong\nlist_OK\npong\nlist_OK\nOK\n"))
     nil ; no idle
     (elmpd-chain
      conn
      ('("ping" "ping")
       (lambda (_conn rsp)
         (should (stringp rsp))
         (should (string= rsp "pong"))
         (setq invocation-count (1+ invocation-count)))
       stream)))
    (should (eq 2 invocation-count))))

(ert-deftest test-chain-smoke-5 ()
  "Smoke test sfor `elmpd-chain'."

  (let ((invoked))
    (with-elmpd-test
     "elmpd-chain-smoke-5"
     '(("consume 2\n" . "ACK [50@1] {consume} value must be 0 or 1\n"))
     nil ; no idle
     (elmpd-chain
      conn
      "consume 2"
      :or-else
      (lambda (_conn text)
        (should (string= text "value must be 0 or 1"))
        (setq invoked t))))
    (should invoked)))

(ert-deftest test-chain-smoke-6 ()
  "Smoke test sfor `elmpd-chain'."

  (let ((or-else-invoked)
        (and-then-invoked))
    (with-elmpd-test
     "elmpd-chain-smoke-6"
     '(("consume 2\n" . "ACK [50@1] {consume} value must be 0 or 1\n"))
     nil ; no idle
     (elmpd-chain
      conn
      "consume 2"
      :or-else
      (lambda (_conn text)
        (should (string= text "value must be 0 or 1"))
        (setq or-else-invoked t))
      :and-then
      ("ping"
       (lambda (_conn _text) (setq and-then-invoked t)))))
    (should or-else-invoked)
    (should (not and-then-invoked))))
 
(ert-deftest test-chain-smoke-7 ()
  (let ((invoked) (or-else-invoked))
    (with-elmpd-test
     "elmpd-chain-smoke-7"
     '(("command_list_ok_begin\nping\nping\ncommand_list_end\n" .
        "pong\nlist_OK\npong\nlist_OK\nOK\n")
       ("consume 1\n" . "OK\n")
       ("consume 2\n" . "ACK [50@1] {consume} value must be 0 or 1\n"))
     nil ; no idle
     (elmpd-chain
      conn
      ('("ping" "ping")
       (lambda (_conn rsp)
         (should ok)
         (should (listp rsp))
         (should (eq 2 (length rsp)))
         (should (string= "pong" (nth 0 rsp)))
         (should (string= "pong" (nth 1 rsp)))
         (setq invoked t))
       list)
      :and-then
      "consume 1"
      :and-then
      "consume 2"
      :or-else (lambda (_conn text)
                 (should (string= text "value must be 0 or 1"))
                 (setq or-else-invoked t))))
    (should invoked)
    (should or-else-invoked)))

(ert-deftest test-chain-smoke-8 ()

  (let ((or-else-1-invoked)
        (or-else-2-invoked)
        (and-then-1-invoked)
        (and-then-2-invoked))
    (with-elmpd-test
     "elmpd-chain-smoke-8"
     '(("command_list_ok_begin\nping\nping\ncommand_list_end\n" .
        "pong\nlist_OK\npong\nlist_OK\nOK\n")
       ("consume 1\n" . "OK\n")
       ("consume 2\n" . "ACK [50@1] {consume} value must be 0 or 1\n"))
     nil ; no idle
     (elmpd-chain
      conn
      ('("ping" "ping")
       (lambda (_conn rsp)
         (should ok)
         (should (listp rsp))
         (should (eq 2 (length rsp)))
         (should (string= "pong" (nth 0 rsp)))
         (should (string= "pong" (nth 1 rsp))))
       list)
      :and-then
      ("consume 1"
       (lambda (_conn _rsp)
         (setq and-then-1-invoked t)))
      :or-else
      (lambda (_conn _rsp)
        (setq or-else-1-invoked t))
      :and-then
      ("consume 2"
       (lambda (_conn _rsp)
         (setq and-then-2-invoked t)))
      :or-else
      (lambda (_conn rsp)
        (should (string= rsp "value must be 0 or 1"))
        (setq or-else-2-invoked t))))
    (should (not or-else-1-invoked))
    (should and-then-1-invoked)
    (should or-else-2-invoked)
    (should (not and-then-2-invoked))))

(ert-deftest test-chain-smoke-9 ()
  "`elmpd-chain' smoke test-- catch 'stream."
  (let ((conn t))
    (should-error
     (elmpd-chain
      conn
      ('("channels" "channels")
       (lambda (_conn rsp) (elmpd-log 'info 'macro-testing "Got ``%s'' (%d)" rsp (length rsp)))
       stream)
      :or-else (lambda (_conn text) (elmpd-log 'error 'macro-testing "Whoah: %s" text))
      :and-then
      ('("channels" "channels")
       (lambda (_conn rsp) (elmpd-log 'info 'macro-testing "2: Got ``%s'' (%d)" rsp (length rsp)))
       list)
      :and-then
      "consume 2"
      :or-else (lambda (_conn text) (elmpd-log 'error 'macro-testing "3: Whoah: %s" text))
      :and-then
      ('("ping" "ping")
       (lambda (_conn rsp) (elmpd-log 'info 'macro-testing "3: Got ``%s'' (%d)" rsp (length rsp)))
       stream)))))

(provide 'test-cmd-lists)

;;; test-cmd-lists.el ends here
