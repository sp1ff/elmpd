;;; manual `elmpd' tests -*- lexical-binding: t; -*-

;; assumes there's a live MPD server

(require 'elmpd)
(setq elmpd-log-level 'debug)
(setq conn (elmpd-connect :host "192.168.1.6" :port 6600 :local nil
                          :subsystems '(all . (lambda (_ subsys)
                                                (message "%s changed" subsys)))))
(elmpd-send conn "play" (lambda (_ ok text) (message "play: %s %s" ok text)))
(elmpd-send
 conn
 '("pause" "status" "ping" "stop")
 (lambda (_ ok text)
   (elmpd-log 'info 'smoke-tests "streaming response: %s %s" ok text))
 :response 'stream)
