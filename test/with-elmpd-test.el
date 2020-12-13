;;; with-elmpd-test.el --- Support for `elmpd' unit tests -*- lexical-binding: t; -*-

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

;; These are the unit tests for `elmpd' command-list support.  The
;; primary unit test suite is in test-elmpd.el.

;;; Code:

(require 'ert)
(require 'elmpd)

;; This is touchy: generally, when our client issues a command, it
;; will be a single line, and the client will not do anything else
;; until we have processed that command & written the response to the
;; socket.

;; There are a few cases where that does not hold:

;; 1. the "idle" command: the client expects no response, and may
;; follow up with a "noidle" quickly enough that it appears in the
;; same message to us. I.e. `text' could look like "idle foo bar
;; splat\nnoidle\n" (Nb. the client will never send anything more than
;; that, since it expects an OK in response to the noidle command).

;; 2. command lists: the client may (and `elmpd' in fact does)
;; choose to write the command list one line at a time; that is,
;; instead of doing something equivalent to:
;;
;;   (process-send-string fd "command_list_begin\nfoo\n\bar\ncommand_list_end\n") (*)
;;
;; they instead do something like:
;;
;;   (process-send-string fd "command_list_begin\n")
;;   (process-send-string fd "foo\n")
;;   (process-send-string fd "\bar\n")
;;   (process-send-string fd "command_list_end\n"
;;
;; While less likely, they *could* do this with *any* command: e.g.
;;
;;   (process-send-string fd "consume ")
;;   (process-send-string fd "1\n")
;;
;; is idiosyncratic, but legitimate.
;;
;; 3. This is unlikely, but depending on the underlying transport and
;; its configuration, if the client issues a lengthy command list, it
;; may show up here in pieces; e.g. even if the client sent the
;; command above as in (*), `text' could be something like
;; "command_list_begin\nfoo\ba", followed by "r\nspla" and finally
;; "t\nend_command_list\n".
;;
;; This is true of *any* command if it's long enough relative to the
;; underlying transport-- I could probably construct a socket with a
;; sufficiently pathological configuration that "idle player
;; stored_playlist options mixer sticker\n" would be broken up into
;; multiple messages.

;; Case 1 requires additional logic when we're expecting an "idle"
;; command. Cases 2 & 3 are the same in that so long as the response
;; received so far is a prefix of the expected response, we can
;; continue to accumulate text in the hope that we'll eventually
;; receive evrything we're looking for.

;; This process auto-terminates: its plist comes equipped with a list
;; of messages (property :msgs)-- each time it consumes one and writes
;; the response, it will check to see if there are any left. If not,
;; it deletes itself.

;; TODO(sp1ff): this function is too long & too hard to read-- re-factor
(defun elmpd-mock-mpd-server-filter (proc text)
  "Mock MPD process filter: PROC is the server process, TEXT the input."
  (let* ((plist (process-plist proc))
         (msgs (plist-get plist :msgs))
         (idle-state (plist-get plist :idle-state))
         (in (caar msgs))
         (out (cdar msgs)))
    (elmpd-log 'info 'mock-server "mock-server: %s|%s|%s|%s" text in out idle-state)
    (if (and (symbolp in) (eq in 'idle))
        ;; We're expecting an "idle" message.
        (if idle-state
            ;; We've seen the initial "idle" & it was fine-- now we're
            ;; expecting a "noidle".
            (progn
              (should (string= "noidle\n" text))
              ;; We're done-- advance our state.
              (plist-put plist :idle-state nil)
              (plist-put plist :msgs (cdr msgs))
              (set-process-plist proc plist)
              (if (eq 1 (length msgs) (delete-process proc))))
          ;; We're expecting an "idle" message (to which we produce no
          ;; response), and the subsequent "noidle" message may or not
          ;; be waiting for us in `text' as well.
          (let* ((lines (split-string text "\n" t))
                 (line (car lines)))
            (should (string= (concat "idle " (mapconcat 'identity out " ")) line))
            (setq lines (cdr lines))
            ;; If we're here, we got the expected "idle"
            ;; message-- anything else waiting for us?
            (if lines
                (let* ((lines (cdr lines))
                       (line (car lines)))
                  (should (eq (length lines) 1))
                  (should (string= "noidle" line))
                  ;; Done
                  (plist-put plist :idle-state nil)
                  (plist-put plist :msgs (cdr msgs))
                  (if (eq 1 (length msgs)) (delete-process proc)))
              ;; The "noidle" didn't show up in this
              ;; message-- record the fact that we're
              ;; expecting it next.
              (plist-put plist :idle-state t)
              (plist-put plist :msgs (cdr msgs))
              (set-process-plist proc plist)
              (elmpd-log 'debug 'mock-server "process plist is now %s" (process-plist proc)))))
      ;; Not expecting an "idle" message-- accumulate the response,
      ;; check it once it's complete, then send the response.
      (let* ((buf (plist-get plist :buf))
             (curr (concat buf text)))
        (should (or (string= curr in) (string-prefix-p curr in)))
        ;; If `curr' == `in', we're done
        (if (string= curr in)
            (progn
              (plist-put plist :msgs (cdr msgs))
              (plist-put plist :buf "")
              (set-process-plist proc plist)
              (process-send-string proc out)
              (if (eq 1 (length msgs)) (delete-process proc)))
          (plist-put plist :buf curr)
          (set-process-plist proc plist)
          (elmpd-log 'debug 'elmpd-mock-server
                     "partial response: process plist now has %d msgs"
                     (length (plist-get (process-plist proc) :msgs))))))))

;; TODO(sp1ff): IN-PROGRESS: finally formalizing my mock MPD server
;; idiom; once I get this working I'd like to hoist it out into a
;; library, somewhere-- but how to re-use it in `mpdmacs' et al?
(defmacro with-elmpd-test (name convo idle-spec &rest body)
  "Run BODY against a mocked MPD server expecting CONVO from test NAME.

The body can test against a mocked MPD connection named `conn',
which will be automatically cleaned-up on exit (clean or not).

TODO(sp1ff): document CONVO."
  (declare (indent defun))
  `(let* ((elmpd-log-level 'debug)
          (elmpd-log-buffer-name (concat "*" ,name "*"))
          (finished nil)
          (server
           ;; This will live on, listening for data, until we kill it.
           (make-network-process
            :name (concat ,name "-server")
            :server t
            :service t
            :family 'ipv4
            :sentinel
            (lambda (proc event)
              (if (string= "deleted\n" event)
                  (setq finished t))
              (if (and (> (length event) 8)
	                     (string= (substring event 0 9) "open from"))
                  (process-send-string proc "OK MPD 256.256.256\n")))
            :filter #'elmpd-mock-mpd-server-filter
            :plist (list :msgs ,convo :idle-state nil :buf "")))
          (port (process-contact server :service))
          (conn
           (elmpd-connect
            :host "localhost" :port port
            :subsystems ,idle-spec)))
     (unwind-protect
         (progn ,@body)
       (accept-process-output (elmpd-connection--fd conn) 0)
       (let ((count 0))
         (while (not finished)
           ;; I can't find an documentation on this, but calling `sit-for'
           ;; seems essential to processing  the input.
           (sit-for 2)
           (accept-process-output (elmpd-connection--fd conn) 0)
           (setq count (1+ count))
           (elmpd-log 'info 'mock-server "%d: %s" count (elmpd--pp-conn conn))
           ;; Fail-safe to prevent deadlock
           (should (< count 8))))
       (should (eq 0 (elmpd-conn-queue-size conn)))
       (delete-process server)
       (should (not (process-live-p server))))))

(provide 'with-elmpd-test)

;;; with-elmpd-test.el ends here.
