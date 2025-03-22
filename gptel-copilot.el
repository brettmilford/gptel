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

;; NOTE: Github Copilot requires oauth.
;; Github oauth flow
;; 1. oauth access token -> 2. session token + expires_at -> renew session token
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

(defun gptel-copilot-chat--request-renew-token()
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

;; Helper funcs
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
  `(("authorization" . ,(concat "Bearer " (gptel-copilot--get-session-token)))
    ("content-type" . "application/json")
    ("accept" . "application/json")
    ;;("user-agent" . "CopilotChat.nvim/2.0.0")
    ("editor-version" . "Neovim/0.10.0")
    ;;("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
    ;;("x-request-id" . ,(gptel-copilot-chat--uuid))
    ;;("vscode-sessionid" . ,(gptel-copilot-chat-sessionid gptel-copilot-chat--instance))
    ;;("vscode-machineid" . ,(gptel-copilot-chat-machineid gptel-copilot-chat--instance))
    ("copilot-integration-id" . "vscode-chat")
    ("openai-intent" . "conversation-panel")
    ("openai-organization" . "github-copilot")
    ))

;; NOTE: Getting and enabling models
(defun gptel-copilot-chat--request-enable-model-policy (model-id)
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

(defvar gptel-copilot-chat-models-cache-file "~/.cache/gptel/models.json")

(defun gptel-copilot-chat--save-models-to-cache (models)
  "Save MODELS to disk cache."
  (when models
    (let ((cache-data `((timestamp . ,(round (float-time)))
                        (models . ,models)))
          (copilot-chat-models-cache-file "~/.cache/gptel/models.json"))
      (with-temp-file copilot-chat-models-cache-file
        (insert (json-encode cache-data)))
      (when gptel-copilot-chat-debug
        (message "Saved %d models to cache %s" (length models) gptel-copilot-chat-models-cache-file)))))

(cl-defun gptel-copilot-chat--request-models-cb (&key response
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
      (gptel-copilot-chat--save-models-to-cache sorted-models)

      ;; Enable policies for models if needed
      (dolist (model sorted-models)
        (when (and (alist-get 'policy model)
                   (equal (alist-get 'state (alist-get 'policy model)) "unconfigured"))
          (gptel-copilot-chat--request-enable-model-policy (alist-get 'id model))))

      ;; Return the models list for immediate use
      sorted-models)))

(defun gptel-copilot-chat--request-models (&optional quiet)
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
      :complete #'gptel-copilot-chat--request-models-cb)))

;; Define a helper function to load models from cache
(defun gptel-copilot--get-models-from-cache ()
  "Load Copilot models from cache file."
  (when (file-exists-p gptel-copilot-chat-models-cache-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents gptel-copilot-chat-models-cache-file)
          (let* ((cache-data (json-read-from-string (buffer-substring-no-properties
                                                    (point-min) (point-max))))
                 (timestamp (alist-get 'timestamp cache-data))
                 (models (alist-get 'models cache-data)))
            (when gptel-copilot-chat-debug
              (message "Loaded %d models from cache" (length models)))
            (setf (gptel-copilot-chat-models gptel-copilot-chat--instance) models)
            (setf (gptel-copilot-chat-last-models-fetch-time gptel-copilot-chat--instance) timestamp)
            models))
      (error
       (when gptel-copilot-chat-debug
         (message "Error loading models from cache"))
       nil))))

(defun gptel-copilot-extract-formatted-models ()
  "Extract models from gptel-copilot-chat--instance and format them for gptel."
  ;; Check if we need to refresh models (models empty or cache expired)
  (let ((cache-timeout (* 60 60 24))  ; 24 hour cache expiration
        (current-time (float-time)))

    ;; First, try to load from cache if available and models are empty
    (when (and (null (gptel-copilot-chat-models gptel-copilot-chat--instance))
               (file-exists-p gptel-copilot-chat-models-cache-file))
      (gptel-copilot--get-models-from-cache))

    ;; Check if we need to refresh models
    (when (or (null (gptel-copilot-chat-models gptel-copilot-chat--instance))
              (> current-time
                 (+ (gptel-copilot-chat-last-models-fetch-time gptel-copilot-chat--instance)
                    cache-timeout)))
      (when gptel-copilot-chat-debug
        (message "Models cache expired or empty, fetching new models..."))
      (gptel-copilot-chat--request-models t)))

  (let* ((models-vector (gptel-copilot-chat-models gptel-copilot-chat--instance))
        (models-list (append models-vector nil))  ; Convert vector to list
        (result '())
        ;; Descriptions dictionary (can be expanded with better descriptions)
        (model-descriptions
         '(("gpt-4o" . "Advanced model for complex tasks; cheaper & faster than GPT-Turbo")
           ("gpt-4" . "Powerful model for high-quality text generation")
           ("gpt-3.5-turbo" . "Fast and efficient model for general use")
           ("claude-3.5-sonnet" . "Anthropic's balanced reasoning model")
           ("claude-3.7-sonnet" . "Latest Claude model with enhanced reasoning")
           ("claude-3.7-sonnet-thought" . "Claude model with visible thinking process") ("o1" . "OpenAI's expert reasoning model")
           ("o3-mini" . "Compact reasoning model with strong performance")
           ("gemini-2.0-flash" . "Google's fast and efficient language model"))))

    (dolist (model models-list)
      (let* ((id (alist-get 'id model))
             (name (alist-get 'name model))
             (capabilities (alist-get 'capabilities model))
             (supports (alist-get 'supports capabilities))
             (limits (alist-get 'limits capabilities))
             (vision (alist-get 'vision limits))
             (context-window (/ (or (alist-get 'max_context_window_tokens limits) 0) 1000))
             (family (or (alist-get 'family capabilities) ""))
             (caps '())
             (mime-types nil)
             (description (or (cdr (assoc family model-descriptions))
                              (cdr (assoc id model-descriptions))
                              name)))

        ;; Determine capabilities
        (when (alist-get 'tool_calls supports) (push 'tool-use caps))
        (when (alist-get 'structured_outputs supports) (push 'json caps))
        (when vision
          (push 'media caps)
          (setq mime-types (append (alist-get 'supported_media_types vision) nil)))

        ;; Determine costs based on model family (approximate values)
        (let ((input-cost 1.0)
              (output-cost 2.0)
              (cutoff-date "2023-04"))

          (cond
           ((string-match-p "gpt-4o" id)
            (setq input-cost 2.5 output-cost 10.0 cutoff-date "2023-10"))
           ((string-match-p "gpt-4" id)
            (setq input-cost 3.0 output-cost 12.0 cutoff-date "2023-04"))
           ((string-match-p "claude-3\\.7" id)
            (setq input-cost 3.0 output-cost 15.0 cutoff-date "2024-09"))
           ((string-match-p "claude" id)
            (setq input-cost 2.0 output-cost 8.0 cutoff-date "2024-03"))
           ((string-match-p "gemini" id)
            (setq input-cost 1.5 output-cost 6.0 cutoff-date "2024-05")))

          ;; Create the model entry
          (let ((model-entry
                 `(,(intern id)
                   :description ,description
                   :capabilities ,caps
                   :context-window ,context-window
                   :input-cost ,input-cost
                   :output-cost ,output-cost
                   :cutoff-date ,cutoff-date)))

            ;; Add mime-types if applicable
            (when mime-types
              (setq model-entry (append model-entry `(:mime-types ,mime-types))))

            ;; Add to result
            (push model-entry result)))))

    ;; Return the sorted list of models
    (nreverse result)))

;;(gptel-copilot-chat--request-models nil)

;; Helpers specifically for gptel-make-*
(defun gptel-copilot--get-headers ()
  "Get headers for Copilot API requests."
  (when-let* ((key (gptel--get-api-key)))
  `(("Authorization" . ,(concat "Bearer " key))
    ("openai-intent" . "conversation-panel")
    ("content-type" . "application/json")
    ("accept" . "application/json")
    ;;("user-agent" . "CopilotChat.nvim/2.0.0")
    ;;("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
    ;;("x-request-id" . ,(gptel-copilot-chat--uuid))
    ;;("vscode-sessionid" . ,(gptel-copilot-chat-sessionid gptel-copilot-chat--instance))
    ;;("vscode-machineid" . ,(gptel-copilot-chat-machineid gptel-copilot-chat--instance))
    ("copilot-integration-id" . "vscode-chat")
    ("openai-organization" . "github-copilot")
    ("editor-version" . "Neovim/0.10.0"))))

(defun gptel-copilot--get-access-token ()
  (let ((token-file (expand-file-name gptel-copilot-chat-github-token-file)))
    (if (file-exists-p token-file)
        (progn
          (setf (gptel-copilot-chat-github-token gptel-copilot-chat--instance)
                (with-temp-buffer
                  (insert-file-contents token-file)
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (gptel-copilot--request-login)
      )))

(defun gptel-copilot--get-session-token ()
  (gptel-copilot--get-access-token)
  (when (null (gptel-copilot-chat-token gptel-copilot-chat--instance))
    ;; try to load token from ~/.cache/copilot-chat-token
    (let ((token-file (expand-file-name gptel-copilot-chat-token-cache)))
      (when (file-exists-p token-file)
        (with-temp-buffer
          (insert-file-contents token-file)
          (setf (gptel-copilot-chat-token gptel-copilot-chat--instance) (json-read-from-string (buffer-substring-no-properties (point-min) (point-max))))))))

  (when (or (null (gptel-copilot-chat-token gptel-copilot-chat--instance))
            (> (round (float-time (current-time))) (alist-get 'expires_at (gptel-copilot-chat-token gptel-copilot-chat--instance))))
    (gptel-copilot-chat--request-renew-token))
  (alist-get 'token (gptel-copilot-chat-token gptel-copilot-chat--instance)))

;;;###autoload
(cl-defun gptel-make-copilot
    (name &key curl-args stream request-params
          (header #'gptel-copilot--get-headers)
          (key #'gptel-copilot--get-session-token)
          (host "api.githubcopilot.com")
          (protocol "https")
          (endpoint "/chat/completions")
          (models (gptel-copilot-extract-formatted-models)))
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
                  :models (gptel--process-models models) ;; TODO: get from model-cache
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (concat protocol "://" host endpoint))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal) backend))))

(provide 'gptel-copilot)
;;; gptel-copilot.el ends here
