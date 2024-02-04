;;; test-cmd-lists.el --- ERT tests for elmpd command list support -*- lexical-binding: t; -*-

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

(ert-deftest test-command-lists-smoke ()
  "Basic command-list-related tests against a mocked server."

  (let ((cb1-invoked)
        (cb2-invoked)
        (cb3-invoked)
        (cb4-invoked 0)
        (cb5-invoked 0)
        (cb6-invoked 0))
    (with-elmpd-test
     "command-lists-smoke"

     '(("consume 1\n" . "OK\n")
       ("ping\n" . "pong\nOK\n")
       ("command_list_begin\nping\ncrossfade 5\nsetvol 80\ncommand_list_end\n" .
        "pong\nOK\n")
       ("command_list_begin\nping\ncrossfade 5\nsetvol 80\ncommand_list_end\n" .
        "pong\nOK\n")
       ("command_list_ok_begin\nping\ncrossfade 5\nsetvol 80\ncommand_list_end\n" .
        "pong\nlist_OK\nlist_OK\nlist_OK\nOK\n")
       ("command_list_ok_begin\nping\nconsume 2\nsetvol 80\ncommand_list_end\n" .
        "pong\nlist_OK\nACK [50@1] {consume} value must be 0 or 1\n")
       ("command_list_ok_begin\nping\ncrossfade 5\nsetvol 80\ncommand_list_end\n" .
        "pong\nlist_OK\nlist_OK\nlist_OK\nOK\n")
       ("command_list_ok_begin\nping\nconsume 2\nsetvol 80\ncommand_list_end\n" .
        "pong\nlist_OK\nACK [50@1] {consume} value must be 0 or 1\n"))

     nil ; no idle

     ;; This should send the command "consume 1\n", with no callback.
     (elmpd-send conn "consume 1")
     ;; This should send the command "ping\n", with a callback
     (elmpd-send conn "ping"
                 (lambda (_conn ok text)
                   (setq cb1-invoked t)
                   (should ok)
                   (should (string= text "pong\n"))))
     ;; NB. the above callback *will* be invoked, but not likely by this point!
     ;; Now let's send a command list: this form will have no
     ;; callback processing
     (elmpd-send conn '("ping" "crossfade 5" "setvol 80"))
     ;; and with a callback
     (elmpd-send
      conn
      '("ping" "crossfade 5" "setvol 80")
      (lambda (_conn ok text)
        (setq cb2-invoked t)
        (should ok)
        (should (string= text "pong\n"))))
     ;; Now let's specify a different callback interface:
     (elmpd-send
      conn
      '("ping" "crossfade 5" "setvol 80")
      (lambda (_conn ok responses)
        (setq cb3-invoked t)
        (should ok)
        (should (eq 3 (length responses)))
        (should (string= "pong" (nth 0 responses)))
        (should (string= "" (nth 1 responses)))
        (should (string= "" (nth 2 responses))))
      :response 'list)
     ;; if there's an error, there should be two invocations: the
     ;; successes, the the error
     (elmpd-send
      conn
      '("ping" "consume 2" "setvol 80")
      (lambda (_conn ok responses)
        (setq cb4-invoked (1+ cb4-invoked))
        (cond
         ((eq cb4-invoked 1)
          (should (eq 1 (length responses)))
          (should ok)
          (should (string= "pong" (nth 0 responses))))
         ((eq cb4-invoked 2)
          (should (not ok))
          (should (string= "value must be 0 or 1" responses)))
         (t
          (should nil))))
      :response 'list)

      ;; now let's try a slightly different API...
     (elmpd-send
      conn
      '("ping" "crossfade 5" "setvol 80")
      ;; should be called three times, one for each response
      (lambda (_conn ok text)
        (should ok)
        (cond
         ((eq cb5-invoked 0)
          (should (string= text "pong"))
          (setq cb5-invoked (1+ cb5-invoked)))
         ((eq cb5-invoked 1)
          (should (string= text ""))
          (setq cb5-invoked (1+ cb5-invoked)))
         ((eq cb5-invoked 2)
          (should (string= text ""))
          (setq cb5-invoked (1+ cb5-invoked)))
         (t
          (should-not t))))
      :response 'stream
      )
      ;; again with an error
      (elmpd-send
       conn
       '("ping" "consume 2" "setvol 80")
       ;; should be called three times, one for each response
       (lambda (_conn ok text)
         (cond
          ((eq cb6-invoked 0)
           (should ok)
           (should (string= text "pong"))
           (setq cb6-invoked (1+ cb6-invoked)))
          ((eq cb6-invoked 1)
           (should (not ok))
           (should (string= text "value must be 0 or 1"))
           (setq cb6-invoked (1+ cb6-invoked)))
          (t
           (should-not t))))
        :response 'stream)
     )
    (should cb1-invoked)
    (should cb2-invoked)
    (should cb3-invoked)
    (should (eq 2 cb4-invoked))
    (should (eq 3 cb5-invoked))
    (should (eq 2 cb6-invoked))))

(provide 'test-cmd-lists)

;;; test-cmd-lists.el ends here
