(straight-use-package 'websocket)
(straight-use-package 'request)

(require 'websocket)
(require 'request)
(require 'lui)

(defvar ec/base-url "https://discord.com/api/v9" "The base url used to connect to Discord.")

(setq ec/ws-url "wss://gateway.discord.gg")

(setq ec/seq 0)

(setq ec/token nil)

(setq ec/buffer (get-buffer-create "*dcord*")
      ec/log-buffer (get-buffer-create "*dcord-log*"))

(defun ec/msg (text)
  (with-current-buffer ec/buffer
    (goto-char (point-max))
    (insert (concat text "\n"))))

(defun ec/log (text)
  (with-current-buffer ec/log-buffer
    (goto-char (point-max))
    (insert (concat text "\n"))))

;; {
;;   "op": 2,
;;   "d": {
;;     "token": "my_token",
;;     "intents": 513,
;;     "properties": {
;;       "$os": "linux",
;;       "$browser": "my_library",
;;       "$device": "my_library"
;;     }
;;   }
;; }

(defun ec/identify ()
  "Construt a identify"
  (json-encode-alist
   `((op . 2)
     (d . ((token . ,ec/token)
           (intents . 4608)
           (properties . (($os . "emacs")
                          ($browser . "ecord")
                          ($device . "ecord"))))))))

(defun ec/send-identify (websocket)
  "Send a identify to the websocket"
  (websocket-send-text websocket (ec/identify)))

(request (concat ec/base-url "/gateway")
  :parser 'json-read
  :sync t
  :success (cl-function
            (lambda (&key data &allow-other-keys)
              (setq ec/ws-url (alist-get 'url data)))))

(defun ec/hbn ()
  "Construct the next heartbeat to send"
  (ec/log "Constructing heartbeat")
  (json-encode-alist `((op . 1) (d . ,ec/seq))))

(defun ec/heartbeater (websocket time)
  "Heartbeat at every ~time~ ms"
  (let ((delay (/ time 1000)))
    (ec/log (format "Starting heartbeater with time: %f" delay))
    (run-with-timer 0 delay 'websocket-send-text websocket (ec/hbn))))

(defvar ec/dcord-on-message nil
  "Abnormal hook that is called on every message received from
  Discord.

The hook gets two parameters (MESSAGE JSON) where MESSAGE will be the
string of the message and JSON will be the data that is sent together
with a message")

(defun ec/dispatch (type json)
  "Handle dispatch payloads"
  (ec/log (format "Type: %S" type))
  (ec/log (format "Json: %S" json))
  (cond ((string= type "MESSAGE_CREATE")
         (run-hook-with-args 'ec/dcord-on-message (gethash "content" json) json))))

(defun ec/ws-handler (websocket frame)
  "Handler for incomming websocket messages"
  (ec/log "called handler")
  (let ((json (json-parse-string (websocket-frame-text frame))))
    (ec/log (format "%S" json))
    (setq ec/seq (gethash "s" json))
    (cond ((eq (gethash "op" json) 0)
           (ec/dispatch (gethash "t" json) (gethash "d" json)))
          ((eq (gethash "op" json) 10)
           (ec/send-identify websocket)
           (ec/log "Starting heartbeater")
           (ec/heartbeater websocket (gethash "heartbeat_interval" (gethash "d" json)))
           (ec/log "Started heartbeater"))
          ((eq (gethash "op" json) 11)
           (ec/log "Got ack")))))


(defun ec/start-dcord (TOKEN)
  "Start a dcord instance with the token TOKEN"
  (setq ec/token TOKEN)
  (setq ec/ws
        (websocket-open (concat ec/ws-url "/?v=9&encoding=json")
                        :on-message (lambda (websocket frame) (ec/ws-handler websocket frame))
                        :on-close (lambda (_websocket) (ec/log "websocket closed")))))

(defun ec/stop-dcord ()
  (websocket-close ec/ws)
  (cancel-function-timers 'websocket-send-text)
  (setq ec/ws nil))

(defun dcord-log (content json)
  "Print messages to *dcord*"
  (let ((name (if (or (null (gethash "member" json))
                      (eq (gethash "member" json) :null)
                      (eq (gethash "nick" (gethash "member" json)) :null))
                  (gethash "username" (gethash "author" json))
                (gethash "nick" (gethash "member" json))))
        (time (format-time-string
               "%T"
               (parse-time-string (gethash "timestamp" json)))))
    (ec/msg (format "%s - %s: %s" time name content))))

;; POST /channels/{channel.id}/messages
(defun ec/send-msg (channel-id message)
  "Send `message` to `channel-id`"
  (request (format "%s/channels/%d/messages" ec/base-url channel-id)
    :type "POST"
    :data (json-encode `(("content" . ,message)))
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bot " ec/token)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message "I sent: %S" (assoc-default 'json data))))))

(defun ec--input (str)
  (ec/send-msg ec/channel str))

(defun ec--format (content json)
  "Print messages to *dchat*"
  (let ((name (if (or (null (gethash "member" json))
                      (eq (gethash "member" json) :null)
                      (null (gethash "nick" (gethash "member" json)))
                      (eq (gethash "nick" (gethash "member" json)) :null))
                  (gethash "username" (gethash "author" json))
                (gethash "nick" (gethash "member" json))))
        (time (format-time-string
               "%X"
               (parse-iso8601-time-string (gethash "timestamp" json)))))
    (lui-insert (format "%s - %s: %s" time name content))))

(define-derived-mode dchat-mode lui-mode "DChat"
  ""
  (lui-set-prompt "> ")
  (goto-char (point-max))
  (add-hook 'ec/dcord-on-message 'ec--format)
  ;; (add-hook 'lui-pre-output-hook 'circe--output-highlight-nick
  ;;           t t)
  (setq lui-input-function 'ec--input))

(add-hook 'ec/dcord-on-message 'dcord-log)

;; Just use https://github.com/twilight-rs/http-proxy to ratelimit
(setq ec/base-url "http://0.0.0.0:3000/api/v9")
(setq ec/token "TOKEN")
(setq ec/channel 285899321705103361)

(ec/send-msg 285899321705103361 "also test")

(ec/start-dcord "TOKEN")

(ec/stop-dcord)

