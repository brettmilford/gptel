;;; gptel-copilot.el --- Copilot chat integration for gptel -*- lexical-binding: t; -*-

;; Author: Your Name <you@example.com>
;; Keywords: convenience, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file provides a backend for gptel that integrates with GitHub Copilot chat.

;;; Code:

(require 'gptel)
(require 'json)
(require 'cl-lib)

(defvar copilot-chat--instance)

(cl-defstruct (gptel-copilot (:constructor gptel--make-copilot)
                            (:copier nil)
                            (:include gptel-backend)))

(defun gptel-copilot--request-login ()
  "Manage GitHub login for Copilot."
  (request "https://github.com/login/device/code"
    :type "POST"
    :data "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}"
    :sync t
    :headers `(("content-type" . "application/json")
               ("accept" . "application/json")
               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
               ("user-agent" . "CopilotChat.nvim/2.0.0")
               ("editor-version" . "Neovim/0.10.0"))
    :parser 'json-read
    :complete (lambda (&key response &key data &allow-other-keys)
                (unless (= (request-response-status-code response) 200)
                  (error "HTTP error: %s" (request-response-status-code response)))
                (let ((device-code (alist-get 'device_code data))
                      (user-code (alist-get 'user_code data))
                      (verification-uri (alist-get 'verification_uri data)))
                  (gui-set-selection 'CLIPBOARD user-code)
                  (read-from-minibuffer
                   (format "Your one-time code %s is copied. Press ENTER to open GitHub in your browser. If your browser does not open automatically, browse to %s."
                           user-code verification-uri))
                  (browse-url verification-uri)
                  (read-from-minibuffer "Press ENTER after authorizing.")
                  (gptel-copilot--request-token device-code)))))

(defun gptel-copilot--request-token (device-code)
  "Request access token using DEVICE-CODE."
  (request "https://github.com/login/oauth/access_token"
    :type "POST"
    :headers `(("content-type" . "application/json")
               ("accept" . "application/json")
               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
               ("editor-version" . "Neovim/0.10.0")
               ("user-agent" . "CopilotChat.nvim/2.0.0"))
    :data (format "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}" device-code)
    :parser 'json-read
    :sync t
    :complete (lambda (&key response &key data &allow-other-keys)
                (unless (= (request-response-status-code response) 200)
                  (error "HTTP error: %s" (request-response-status-code response)))
                (let ((token (alist-get 'access_token data))
                      (token-dir (file-name-directory copilot-chat-github-token-file)))
                  (setf (copilot-chat-github-token copilot-chat--instance) token)
                  (when (not (file-directory-p token-dir))
                    (make-directory token-dir t))
                  (with-temp-file copilot-chat-github-token-file
                    (insert token))
                  (message "GitHub authentication successful")))))

(defun gptel-copilot--renew-token ()
  "Renew session token for Copilot."
  (request "https://api.github.com/copilot_internal/v2/token"
    :type "GET"
    :headers `(("authorization" . ,(concat "token " (copilot-chat-github-token copilot-chat--instance)))
               ("accept" . "application/json")
               ("editor-version" . "Neovim/0.10.0")
               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
               ("user-agent" . "CopilotChat.nvim/2.0.0"))
    :parser 'json-read
    :sync t
    :complete (lambda (&key response &key data &allow-other-keys)
                (unless (= (request-response-status-code response) 200)
                  (error "Authentication error: %s" (request-response-status-code response)))
                (setf (copilot-chat-token copilot-chat--instance) data)
                (let ((cache-dir (file-name-directory copilot-chat-token-cache)))
                  (when (not (file-directory-p cache-dir))
                    (make-directory cache-dir t))
                  (with-temp-file copilot-chat-token-cache
                    (insert (json-encode data)))))))

(defun gptel-copilot--create-request (prompt out-of-context)
  "Create a request payload for Copilot.
PROMPT is the prompt to send.
OUT-OF-CONTEXT is a boolean indicating whether the prompt is out of context."
  (json-encode `(("model" . ,copilot-chat-model)
                 ("prompt" . ,prompt)
                 ("history" . ,(unless out-of-context (copilot-chat-history copilot-chat--instance)))
                 ("contextVisited" . nil)
                 ("outOfContext" . ,out-of-context))))

(defun gptel-copilot--parse-response (response info)
  "Parse Copilot chat RESPONSE and return response text.
INFO is a plist containing additional response information."
  (let* ((choices (alist-get 'choices response))
         (message (and (> (length choices) 0) (alist-get 'message (aref choices 0))))
         (content (and message (alist-get 'content message))))
    (unless (eq content :null)
      content)))

(defun gptel-copilot--parse-stream (info)
  "Parse a streaming Copilot chat response from INFO.
Return the text response."
  (let (content)
    (while (re-search-forward "^data: " nil t)
      (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
             (json (and (not (string= "[DONE]" line)) (json-parse-string line :object-type 'alist)))
             (delta (and json (alist-get 'delta (aref (alist-get 'choices json) 0))))
             (token (and delta (alist-get 'content delta))))
        (when token
          (setq content (concat content token)))))
    content))

;;;###autoload
(cl-defun gptel-make-copilot
    (name &key curl-args stream key request-params
          (header (lambda () `(("Authorization" . ,(concat "Bearer " (copilot-chat-github-token copilot-chat--instance))))))
          (host "api.githubcopilot.com")
          (protocol "https")
          (endpoint "/chat/completions"))
  "Register a Copilot API-compatible backend for gptel with NAME.

Keyword arguments:
CURL-ARGS (optional) is a list of additional Curl arguments.
HOST (optional) is the API host, \"api.githubcopilot.com\" by default.
STREAM is a boolean to toggle streaming responses.
PROTOCOL (optional) specifies the protocol, https by default.
ENDPOINT (optional) is the API endpoint for completions.
HEADER (optional) is for additional headers to send with each request. It should be an alist or a function that returns an alist.
KEY (optional) is a variable whose value is the API key, or function that returns the key.
REQUEST-PARAMS (optional) is a plist of additional HTTP request parameters."
  (declare (indent 1))
  (let ((backend (gptel--make-copilot
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models '(gpt-3.5-turbo gpt-4)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (concat protocol "://" host endpoint))))
    (gptel-copilot--renew-token)
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal) backend))))

(provide 'gptel-copilot)
;;; gptel-copilot.el ends here
