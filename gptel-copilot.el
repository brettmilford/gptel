;;; gptel-copilot.el --- Copilot chat integration for gptel -*- lexical-binding: t; -*-

;; Author: Your Name <you@example.com>
;; Keywords: convenience, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file provides a backend for gptel that integrates with GitHub Copilot chat.

;;; Code:
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))
;(require 'json)
(require 'request)
(require 'gptel)

(defvar json-object-type)

(declare-function prop-match-value "text-property-search")
(declare-function text-property-search-backward "text-property-search")
(declare-function json-read "json" ())
(declare-function gptel-context--wrap "gptel-context")
(declare-function gptel-context--collect-media "gptel-context")

;;; Github Copilot Chat (Messages API)
(cl-defstruct (gptel-copilot (:constructor gptel--make-copilot)
                            (:copier nil)
                            (:include gptel-backend)))

(cl-defmethod gptel--parse-buffer ((backend gptel-copilot) &optional max-entries)
  (let ((prompts) (prev-pt (point))
        (include-media (and gptel-track-media
                            (or (gptel--model-capable-p 'media)
                                (gptel--model-capable-p 'url)))))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min))))
          (pcase (get-char-property (point) 'gptel)
            ('response
             (when-let* ((content (gptel--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "assistant" :content content) prompts)))
            (`(tool . ,id)
             (save-excursion
               (condition-case nil
                   (let* ((tool-call (read (current-buffer)))
                          (name (plist-get tool-call :name))
                          (arguments (gptel--json-encode (plist-get tool-call :args))))
                     (plist-put tool-call :id id)
                     (plist-put tool-call :result
                                (string-trim (buffer-substring-no-properties
                                              (point) prev-pt)))
                     (push (car (gptel--parse-tool-results backend (list tool-call)))
                           prompts)
                     (push (list :role "assistant"
                                 :tool_calls
                                 (vector (list :type "function"
                                               :id (gptel--openai-format-tool-id id)
                                               :function `( :name ,name
                                                            :arguments ,arguments))))
                           prompts))
                 ((end-of-file invalid-read-syntax)
                  (message (format "Could not parse tool-call %s on line %s"
                                   id (line-number-at-pos (point))))))))
            ('ignore)
            ('nil
             (and max-entries (cl-decf max-entries))
             (if include-media
                 (when-let* ((content (gptel--openai-parse-multipart
                                       (gptel--parse-media-links major-mode
                                                                 (point) prev-pt))))
                   (when (> (length content) 0)
                     (push (list :role "user" :content content) prompts)))
               (when-let* ((content (gptel--trim-prefixes (buffer-substring-no-properties
                                                           (point) prev-pt))))
                 (push (list :role "user" :content content) prompts)))))
          (setq prev-pt (point)))
      (let ((content (string-trim (buffer-substring-no-properties
                                    (point-min) (point-max)))))
        (push (list :role "user" :content content) prompts)))
    prompts))

(cl-defmethod gptel--request-data ((backend gptel-copilot) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (when gptel--system-message
    (push (list :role "system"
                :content gptel--system-message)
          prompts))
  (let ((prompts-plist
         `(:model ,(gptel--model-name gptel-model)
           :messages [,@prompts]
           :stream ,(or gptel-stream :json-false)))
        (reasoning-model-p ; TODO: Embed this capability in the model's properties
         (memq gptel-model '(o1 o1-preview o1-mini o3-mini o3))))
    (when (and gptel-temperature (not reasoning-model-p))
      (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-use-tools
      (when (eq gptel-use-tools 'force)
        (plist-put prompts-plist :tool_choice "required"))
      (when gptel-tools
        (plist-put prompts-plist :tools
                   (gptel--parse-tools backend gptel-tools))
        (unless reasoning-model-p
          (plist-put prompts-plist :parallel_tool_calls t))))
    (when gptel-max-tokens
      ;; HACK: The OpenAI API has deprecated max_tokens, but we still need it
      ;; for OpenAI-compatible APIs like GPT4All (#485)
      (plist-put prompts-plist
                 (if reasoning-model-p :max_completion_tokens :max_tokens)
                 gptel-max-tokens))
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

;; NOTE: Github Copilot requires oauth.
;; Github oauth flow
(cl-defstruct (gptel-copilot-chat
               (:copier nil))
  "Struct for Copilot chat state."
  (ready nil :type boolean)
  (github-token nil :type (or null string))
  (token nil)
  (sessionid nil :type (or null string))
  (machineid nil :type (or null string))
  (history nil :type list)
  (buffers nil :type list)
  (models nil :type list)
  (last-models-fetch-time 0 :type number))

(defvar gptel-copilot-chat--instance
  (make-gptel-copilot-chat
   :ready nil
   :github-token nil
   :token nil
   :sessionid nil
   :machineid nil
   :history nil
   :buffers nil
   :models nil
   :last-models-fetch-time 0)
  "Global instance of Copilot chat.")

(defvar gptel-copilot-chat-debug 't)
(defvar gptel-copilot-chat-github-token-file "~/.config/gptel/github-token")

(defun gptel-copilot--request-token (device-code)
  "Request access token using DEVICE-CODE."
  (request
   "https://github.com/login/oauth/access_token"
    :type "POST"
    :data (json-encode `(("client_id" . "Iv1.b507a08c87ecfe98")
                         ("device_code" . ,device-code)
                         ("grant_type" . "urn:ietf:params:oauth:grant-type:device_code")))
    :headers `(("content-type" . "application/json")
               ("accept" . "application/json")
               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
               ("editor-version" . "Neovim/0.10.0")
               ("user-agent" . "CopilotChat.nvim/2.0.0"))
    :parser 'json-read
    :sync t
    :complete (lambda (&key data &allow-other-keys)
                (unless (= (alist-get 'status data) 200)
                  (error "HTTP error: %s" (alist-get 'status data)))
                (let ((token (alist-get 'access_token data))
                       (token-dir (file-name-directory gptel-copilot-chat-github-token-file)))
                  (setf (gptel-copilot-chat-github-token gptel-copilot-chat--instance) token)
                  (unless (file-directory-p token-dir)
                    (make-directory token-dir t))
                  (with-temp-file gptel-copilot-chat-github-token-file
                    (insert token))
                  (message "GitHub authentication successful")))))
(defvar gptel-copilot-chat-github-token-file "~/.config/gptel/github-token")
(cl-defun gptel-copilot-chat--request-token-cb (&key response
                                               &key data
                                               &allow-other-keys)
  "Manage token reception for github auth.
Argument DATA is whatever PARSER function returns, or nil.
Argument RESPONSE is request-response object."
  (unless (= (request-response-status-code response) 200)
    (error "Http error"))
  (let (
        (token (alist-get 'access_token data))
        (token-dir (file-name-directory (expand-file-name gptel-copilot-chat-github-token-file))))
    (setf (gptel-copilot-chat-github-token gptel-copilot-chat--instance) token)
    (when (not (file-directory-p token-dir))
      (make-directory token-dir t))
    (with-temp-file gptel-copilot-chat-github-token-file
      (insert token))))

(cl-defun gptel-copilot-chat--request-code-cb (&key response
                                              &key data
                                              &allow-other-keys)
  "Manage user code reception for github buth.
Argument RESPONSE is request-response object.
Argument DATA is whatever PARSER function returns, or nil."
  (unless (= (request-response-status-code response) 200)
    (error "Http error"))
  (let ((device-code (alist-get 'device_code data))
        (user-code (alist-get 'user_code data))
        (verification-uri (alist-get 'verification_uri data)))
    (gui-set-selection 'CLIPBOARD user-code)
    (read-from-minibuffer
     (format "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
             user-code verification-uri))
    (browse-url verification-uri)
    (read-from-minibuffer "Press ENTER after authorizing.")

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
      :complete #'gptel-copilot-chat--request-token-cb)))

(defun gptel-copilot--request-login ()
  "Manage GitHub login for Copilot."
  (request
   "https://github.com/login/device/code"
    :type "POST"
    :data "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}"
    :sync t
    :headers `(("content-type" . "application/json")
               ("accept" . "application/json")
               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
               ("user-agent" . "CopilotChat.nvim/2.0.0")
               ("editor-version" . "Neovim/0.10.0"))
    :parser 'json-read
    :complete #'gptel-copilot-chat--request-code-cb))

;; Session token
(defvar gptel-copilot-chat-token-cache "~/.cache/gptel/token")
(cl-defun gptel-copilot-chat--request-renew-token-cb(&key response
                                                    &key data
                                                    &allow-other-keys)
  "Renew token callback.
Argument RESPONSE is request-response object.
Argument DATA is whatever PARSER function returns, or nil."
  (unless (= (request-response-status-code response) 200)
    (error "Authentication error"))
  (setf (gptel-copilot-chat-token gptel-copilot-chat--instance) data)
  ;; save token in copilot-chat-token-cache file after creating
  ;; folders if needed
  (let ((cache-dir (file-name-directory (expand-file-name gptel-copilot-chat-token-cache))))
    (when (not (file-directory-p cache-dir))
      (make-directory  cache-dir t))
    (with-temp-file gptel-copilot-chat-token-cache
      (insert (json-encode data)))))

(defun copilot-chat--request-renew-token()
  "Renew session token."
  (request "https://api.github.com/copilot_internal/v2/token"
    :type "GET"
    :headers `(("authorization" . ,(concat "token " (gptel-copilot-chat-github-token gptel-copilot-chat--instance)))
               ("accept" . "application/json")
               ("editor-version" . "Neovim/0.10.0")
               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
               ("user-agent" . "CopilotChat.nvim/2.0.0"))
    :parser 'json-read
    :sync t
    :complete #'gptel-copilot-chat--request-renew-token-cb))


;;(cl-defun copilot-chat--request-renew-token-cb(&key response
;;                                                    &key data
;;                                                    &allow-other-keys)
;;  "Renew token callback.
;;Argument RESPONSE is request-response object.
;;Argument DATA is whatever PARSER function returns, or nil."
;;  (unless (= (request-response-status-code response) 200)
;;    (error "Authentication error"))
;;  (setf (copilot-chat-token copilot-chat--instance) data)
;;  ;; save token in copilot-chat-token-cache file after creating
;;  ;; folders if needed
;;  (let ((cache-dir (file-name-directory (expand-file-name copilot-chat-token-cache))))
;;    (when (not (file-directory-p cache-dir))
;;      (make-directory  cache-dir t))
;;    (with-temp-file copilot-chat-token-cache
;;      (insert (json-encode data)))))
;;
;;
;;(defun gptel-copilot--renew-token ()
;;  "Renew session token for Copilot."
;;  (request
;;   "https://api.github.com/copilot_internal/v2/token"
;;    :type "GET"
;;    :headers `(("authorization" . ,(concat "token " (copilot-chat-github-token copilot-chat--instance)))
;;               ("accept" . "application/json")
;;               ("editor-version" . "Neovim/0.10.0")
;;               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
;;               ("user-agent" . "CopilotChat.nvim/2.0.0"))
;;    :parser 'json-read
;;    :sync t
;;    :complete #'copilot-chat--request-renew-token-cb))

;; (defun get-bearer-token-from-file (file-path)
;;   "Read the token from a file located at FILE-PATH and concatenate 'Bearer ' with the token."
;;   (with-temp-buffer
;;     (insert-file-contents file-path)
;;     (let ((token (string-trim (buffer-string))))
;;       (concat "Bearer " token))))
(defun gptel-copilot-chat--uuid ()
  "Generate a UUID."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (random 16384) 16384)
          (logior (random 4096) 32768)
          (random 65536) (random 65536) (random 65536)))

(defun gptel-copilot-chat--get-headers ()
  "Get headers for Copilot API requests."
  `(("openai-intent" . "conversation-panel")
    ("content-type" . "application/json")
    ("accept" . "application/json")
    ("user-agent" . "CopilotChat.nvim/2.0.0")
    ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
    ("authorization" . ,(concat "Bearer " (alist-get 'token (gptel-copilot-chat-token gptel-copilot-chat--instance))))
    ("x-request-id" . ,(gptel-copilot-chat--uuid))
    ("vscode-sessionid" . ,(gptel-copilot-chat-sessionid gptel-copilot-chat--instance))
    ("vscode-machineid" . ,(gptel-copilot-chat-machineid gptel-copilot-chat--instance))
    ("copilot-integration-id" . "vscode-chat")
    ("openai-organization" . "github-copilot")
    ("editor-version" . "Neovim/0.10.0")))

;; NOTE: Getting and enabling models?
(cl-defun copilot-chat--request-models-cb (&key response
                                                &key data
                                                &allow-other-keys)
  "Handle models response from Copilot API.
Argument DATA is the parsed JSON response.
Argument RESPONSE is request-response object."
  (unless (= (request-response-status-code response) 200)
    (error "Failed to fetch models: %s" (request-response-status-code response)))

  (let* ((models-vector (alist-get 'data data))
         (models (append models-vector nil))  ; Convert vector to list
         (chat-models nil))
    ;; Filter for chat models and extract capabilities
    (dolist (model models)
      (when (and (alist-get 'capabilities model)
                 (equal (alist-get 'type (alist-get 'capabilities model)) "chat"))
        (push model chat-models)))

    (when gptel-copilot-chat-debug
      (message "Fetched %d models" (length chat-models))
      (message "Models: %s" chat-models))

    ;; Store models in instance and return them
    (let ((sorted-models (nreverse chat-models)))
      (setf (gptel-copilot-chat-models gptel-copilot-chat--instance) sorted-models)

      ;; Cache models to disk
      (copilot-chat--save-models-to-cache sorted-models)

      ;; Enable policies for models if needed
      (dolist (model sorted-models)
        (when (and (alist-get 'policy model)
                   (equal (alist-get 'state (alist-get 'policy model)) "unconfigured"))
          (copilot-chat--request-enable-model-policy (alist-get 'id model))))

      ;; Return the models list for immediate use
      sorted-models)))
(defvar gptel-copilot-chat-models-cache-file "~/.cache/gptel/models.json")

(defun copilot-chat--save-models-to-cache (models)
  "Save MODELS to disk cache."
  (when models
    (let ((cache-data `((timestamp . ,(round (float-time)))
                        (models . ,models)))
          (copilot-chat-models-cache-file "~/.cache/gptel/models.json"))
      (with-temp-file copilot-chat-models-cache-file
        (insert (json-encode cache-data)))
      (when gptel-copilot-chat-debug
        (message "Saved %d models to cache %s" (length models) gptel-copilot-chat-models-cache-file)))))

(defun copilot-chat--request-enable-model-policy (model-id)
  "Enable policy for MODEL-ID."
  (let ((url (format "https://api.githubcopilot.com/models/%s/policy" model-id))
        (headers (gptel-copilot-chat--get-headers))
        (data (json-encode '((state . "enabled")))))
    (when gptel-copilot-chat-debug
      (message "Enabling policy for model %s" model-id))
    (request url
      :type "POST"
      :headers headers
      :data data
      :parser 'json-read)))

(defun copilot-chat--request-models (&optional quiet)
  "Fetch available models from Copilot API.
Optional argument QUIET suppresses user messages when non-nil."
  (let ((url "https://api.githubcopilot.com/models")
        (headers (gptel-copilot-chat--get-headers)))
    (when gptel-copilot-chat-debug
      (message "Fetching models from %s" url))
    (unless quiet
      (message "Fetching available Copilot models..."))
    (request url
      :type "GET"
      :headers headers
      :parser 'json-read
      :sync t  ; Use synchronous request when called directly
      :complete #'copilot-chat--request-models-cb)))

;;(copilot-chat--request-models nil)
;;;###autoload
(cl-defun gptel-make-copilot
    (name &key curl-args stream request-params
          (header (gptel-copilot-chat--get-headers))
          (key (gptel-copilot--get-session-token))
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
KEY (optional) is a variable whose value is the session token, or function that returns the session token.
REQUEST-PARAMS (optional) is a plist of additional HTTP request parameters."
  (declare (indent 1))
  (let ((backend (gptel--make-openai
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models '(claude-3.7-sonnet-thought gpt-4)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (concat protocol "://" host endpoint))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal) backend))))

(provide 'gptel-copilot)
;;; gptel-copilot.el ends here
