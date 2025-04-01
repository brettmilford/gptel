
;;; gptel-copilot.el --- GitHub Copilot integration for gptel -*- lexical-binding: t; -*-

;; Keywords: convenience, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides GitHub Copilot Chat integration for gptel.
;; It allows you to use GitHub Copilot's chat models with gptel's interface.
;; Authentication is handled through GitHub's OAuth device flow.

;;; Code:
(require 'cl-lib)
(require 'gptel-openai)
(require 'request)
(require 'xdg)

(declare-function browse-url "browse-url" (url &optional new-window))

;;; Customization options

(defgroup gptel-copilot nil
  "GitHub Copilot integration for gptel."
  :group 'gptel)

(defcustom gptel-copilot-access-token-file
  (expand-file-name "github-access-token" (concat (xdg-config-home) "/gptel"))
  "File to store the GitHub access token for Copilot authentication."
  :type 'file
  :group 'gptel-copilot)

(defcustom gptel-copilot-cache-dir
  (concat (xdg-cache-home) "/gptel")
  "Directory to store Copilot cache files."
  :type 'directory
  :group 'gptel-copilot)

(defcustom gptel-copilot-models-cache-expiry (* 60 60 24)
  "Time in seconds after which the models cache is considered stale (24 hours by default)."
  :type 'natnum
  :group 'gptel-copilot)

(defcustom gptel-copilot-debug nil
  "Whether to enable debug output for gptel-copilot."
  :type 'boolean
  :group 'gptel-copilot)

;;; Internal variables

(defvar gptel-copilot--token-file
  (expand-file-name "copilot-token.json" gptel-copilot-cache-dir)
  "File to store the Copilot session token.")

(defvar gptel-copilot--models-cache-file
  (expand-file-name "copilot-models.json" gptel-copilot-cache-dir)
  "File to store cached Copilot models.")

;;; State management

(cl-defstruct (gptel-copilot-state
               (:constructor gptel-copilot--make-state))
  "Copilot state information."
  (access-token nil :type (or null string))
  (session-token nil :type (or null list))
  (models nil :type list)
  (models-last-fetched 0 :type number))

(defvar gptel-copilot--state (gptel-copilot--make-state)
  "Global Copilot state.")

;;; Utility functions

(defun gptel-copilot--log (format-string &rest args)
  "Log message if debug is enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when gptel-copilot-debug
    (apply #'message (concat "[gptel-copilot] " format-string) args)))

(defun gptel-copilot--ensure-directories ()
  "Ensure that necessary directories exist."
  (unless (file-directory-p gptel-copilot-cache-dir)
    (make-directory gptel-copilot-cache-dir t))
  (unless (file-directory-p (file-name-directory gptel-copilot-access-token-file))
    (make-directory (file-name-directory gptel-copilot-access-token-file) t)))

(defun gptel-copilot--uuid ()
  "Generate a UUID."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (random 16384) 16384)
          (logior (random 4096) 32768)
          (random 65536) (random 65536) (random 65536)))

;;; Authentication

(defun gptel-copilot--get-access-token ()
  "Get GitHub access token for authentication."
  (if (gptel-copilot-state-access-token gptel-copilot--state)
      (gptel-copilot-state-access-token gptel-copilot--state)
    (if (file-exists-p gptel-copilot-access-token-file)
        (let ((token (with-temp-buffer
                       (insert-file-contents gptel-copilot-access-token-file)
                       (string-trim (buffer-string)))))
          (setf (gptel-copilot-state-access-token gptel-copilot--state) token)
          token)
      (gptel-copilot--initiate-login))))

(defun gptel-copilot--initiate-login ()
  "Start the GitHub login process for Copilot."
  (gptel-copilot--log "Starting GitHub login process")

  (request "https://github.com/login/device/code"
    :type "POST"
    :data "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}"
    :headers '(("content-type" . "application/json")
               ("accept" . "application/json")
               ("editor-plugin-version" . "gptel-copilot/1.0.0")
               ("user-agent" . "gptel-copilot/1.0.0")
               ("editor-version" . "Emacs"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((device-code (alist-get 'device_code data))
                      (user-code (alist-get 'user_code data))
                      (verification-uri (alist-get 'verification_uri data)))

                  (gptel-copilot--log "Got device code and user code")

                  ;; Copy to clipboard and prompt user
                  (kill-new user-code)
                  (read-from-minibuffer
                   (format "Your one-time code %s is copied to clipboard. Press ENTER to open GitHub."
                           user-code))

                  (browse-url verification-uri)
                  (read-from-minibuffer "Press ENTER after authorizing in browser.")

                  ;; Exchange the code for a token
                  (gptel-copilot--exchange-code-for-token device-code))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (error "Failed to start GitHub login: %s" error-thrown)))))

(defun gptel-copilot--exchange-code-for-token (device-code)
  "Exchange DEVICE-CODE for a GitHub access token."
  (gptel-copilot--log "Exchanging device code for token")

  (request "https://github.com/login/oauth/access_token"
    :type "POST"
    :headers '(("content-type" . "application/json")
               ("accept" . "application/json")
               ("editor-plugin-version" . "gptel-copilot/1.0.0")
               ("user-agent" . "gptel-copilot/1.0.0"))
    :data (format "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}"
                  device-code)
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((token (alist-get 'access_token data)))
                  (unless token
                    (error "No access token received from GitHub"))

                  (gptel-copilot--ensure-directories)
                  (with-temp-file gptel-copilot-access-token-file
                    (insert token))

                  (setf (gptel-copilot-state-access-token gptel-copilot--state) token))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (error "Failed to get access token: %s" error-thrown)))))

;;; Session token management

(defun gptel-copilot-login()
  (interactive)
  (let ((gptel-copilot-debug 't))
        (gptel-copilot--get-session-token)))

(defun gptel-copilot--get-session-token ()
  "Get or refresh Copilot session token."
  (gptel-copilot--log "Getting session token")

  ;; Ensure we have a GitHub token first
  (gptel-copilot--get-access-token)

  ;; Try to load cached token
  (unless (gptel-copilot-state-session-token gptel-copilot--state)
    (when (file-exists-p gptel-copilot--token-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents gptel-copilot--token-file)
            (setf (gptel-copilot-state-session-token gptel-copilot--state)
                  (json-read-from-string (buffer-string))))
        (error
         (gptel-copilot--log "Error loading token from cache")
         nil))))

  ;; Check if token needs refresh
  (when (or (null (gptel-copilot-state-session-token gptel-copilot--state))
            (> (float-time)
               (alist-get 'expires_at (gptel-copilot-state-session-token gptel-copilot--state))))
    (gptel-copilot--refresh-session-token))

  ;; Return the actual token string
  (alist-get 'token (gptel-copilot-state-session-token gptel-copilot--state)))

(defun gptel-copilot--refresh-session-token ()
  "Refresh the Copilot session token."
  (gptel-copilot--log "Refreshing Copilot session token")

  (request "https://api.github.com/copilot_internal/v2/token"
    :type "GET"
    :headers `(("authorization" . ,(concat "token " (gptel-copilot-state-access-token gptel-copilot--state)))
               ("accept" . "application/json")
               ("editor-version" . "Emacs")
               ("editor-plugin-version" . "gptel-copilot/1.0.0")
               ("user-agent" . "gptel-copilot/1.0.0"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setf (gptel-copilot-state-session-token gptel-copilot--state) data)

                ;; Cache the token
                (gptel-copilot--ensure-directories)
                (with-temp-file gptel-copilot--token-file
                  (insert (json-encode data)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (error "Failed to refresh Copilot token: %s" error-thrown)))))

;;; Model handling

(defun gptel-copilot--get-models (&optional callback)
  "Get available Copilot models.
If CALLBACK is provided, call it with the models when ready."
  (gptel-copilot--log "Getting Copilot models")

  ;; Try to load from cache if models are empty
  (when (and (null (gptel-copilot-state-models gptel-copilot--state))
             (file-exists-p gptel-copilot--models-cache-file))
    (gptel-copilot--load-models-from-cache))

  ;; Check if we need to refresh models
  (let ((current-time (float-time))
        (need-refresh nil))
    (when (or (null (gptel-copilot-state-models gptel-copilot--state))
              (> current-time
                 (+ (gptel-copilot-state-models-last-fetched gptel-copilot--state)
                    gptel-copilot-models-cache-expiry)))
      (setq need-refresh t)
      (gptel-copilot--fetch-models callback))

    ;; If we don't need to refresh and have a callback, call it immediately
    (when (and callback (not need-refresh))
      (funcall callback (gptel-copilot-state-models gptel-copilot--state))))

  ;; Return current models (could be nil if async fetch is in progress)
  (gptel-copilot-state-models gptel-copilot--state))

(defun gptel-copilot--load-models-from-cache ()
  "Load Copilot models from cache."
  (gptel-copilot--log "Loading models from cache")

  (condition-case nil
      (with-temp-buffer
        (insert-file-contents gptel-copilot--models-cache-file)
        (let* ((cache-data (json-read-from-string (buffer-string)))
               (timestamp (alist-get 'timestamp cache-data))
               (models (alist-get 'models cache-data)))
          (setf (gptel-copilot-state-models gptel-copilot--state) models)
          (setf (gptel-copilot-state-models-last-fetched gptel-copilot--state) timestamp)
          (gptel-copilot--log "Loaded %d models from cache" (length models))))
    (error
     (gptel-copilot--log "Error loading models from cache")
     nil)))

(defun gptel-copilot--fetch-models (&optional callback)
  "Fetch available models from Copilot API.
If CALLBACK is provided, call it with the models when ready."
  (gptel-copilot--log "Fetching models from Copilot API")
  (message "Fetching Copilot models...")

  (request "https://api.githubcopilot.com/models"
    :type "GET"
    :headers (gptel-copilot--request-headers)
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((models-vector (alist-get 'data data))
                       (models (append models-vector nil))
                       (chat-models nil))

                  ;; Filter for chat models
                  (dolist (model models)
                    (when (and (alist-get 'capabilities model)
                               (equal (alist-get 'type (alist-get 'capabilities model)) "chat"))
                      (push model chat-models)))

                  (let ((sorted-models (nreverse chat-models)))
                    ;; Store models
                    (setf (gptel-copilot-state-models gptel-copilot--state) sorted-models)
                    (setf (gptel-copilot-state-models-last-fetched gptel-copilot--state) (float-time))

                    ;; Cache models
                    (gptel-copilot--save-models-to-cache sorted-models)

                    ;; Enable policies for models if needed
                    (dolist (model sorted-models)
                      (when (and (alist-get 'policy model)
                                 (equal (alist-get 'state (alist-get 'policy model)) "unconfigured"))
                        (gptel-copilot--enable-model-policy (alist-get 'id model))))

                    (message "Fetched %d Copilot models" (length sorted-models))
                    (when callback
                      (funcall callback sorted-models))))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error fetching models: %S" error-thrown)
              (when callback
                (funcall callback nil))))))

(defun gptel-copilot--save-models-to-cache (models)
  "Save MODELS to disk cache."
  (when models
    (let ((cache-data `((timestamp . ,(float-time))
                        (models . ,models))))

      (gptel-copilot--ensure-directories)
      (with-temp-file gptel-copilot--models-cache-file
        (insert (json-encode cache-data)))

      (gptel-copilot--log "Saved %d models to cache" (length models)))))

(defun gptel-copilot--enable-model-policy (model-id)
  "Enable policy for MODEL-ID."
  (gptel-copilot--log "Enabling policy for model %s" model-id)

  (request (format "https://api.githubcopilot.com/models/%s/policy" model-id)
    :type "POST"
    :headers (gptel-copilot--request-headers)
    :data (json-encode '((state . "enabled")))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (gptel-copilot--log "Successfully enabled policy for %s" model-id)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (gptel-copilot--log "Error enabling policy: %S" error-thrown)))))

(defun gptel-copilot--format-models-for-gptel ()
  "Format Copilot models for gptel backend."
  (let* ((models-vector (gptel-copilot--get-models))
         (models (append models-vector nil))
         (result '())
         ;; Descriptions dictionary
         (model-descriptions
          '(("gpt-4o" . "Advanced model for complex tasks; cheaper & faster than GPT-Turbo")
            ("gpt-4" . "Powerful model for high-quality text generation")
            ("gpt-3.5-turbo" . "Fast and efficient model for general use")
            ("claude-3.5-sonnet" . "Anthropic's balanced reasoning model")
            ("claude-3-opus" . "Anthropic's most powerful model for complex tasks")
            ("claude-3-sonnet" . "Balanced performance and intelligence")
            ("claude-3.7-sonnet" . "Latest Claude model with enhanced reasoning")
            ("claude-3.7-sonnet-thought" . "Claude model with visible thinking process")
            ("o1" . "OpenAI's expert reasoning model")
            ("o3-mini" . "Compact reasoning model with strong performance")
            ("gemini-2.0-flash" . "Google's fast and efficient language model"))))

    (dolist (model models)
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
            (setq input-cost 10.0 output-cost 30.0 cutoff-date "2023-04"))
           ((string-match-p "claude-3\\.7" id)
            (setq input-cost 3.0 output-cost 15.0 cutoff-date "2024-09"))
           ((string-match-p "claude-3-opus" id)
            (setq input-cost 15.0 output-cost 75.0 cutoff-date "2023-12"))
           ((string-match-p "claude" id)
            (setq input-cost 3.0 output-cost 15.0 cutoff-date "2023-12"))
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

;;; Backend integration

(defun gptel-copilot--request-headers ()
  "Get headers for Copilot API requests."
  `(("authorization" . ,(concat "Bearer " (gptel-copilot--get-session-token)))
    ("openai-intent" . "conversation-panel")
    ("content-type" . "application/json")
    ("accept" . "application/json")
    ("user-agent" . "CopilotChat.nvim/2.0.0")
    ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
    ("x-request-id" . ,(gptel-copilot--uuid))
    ("editor-version" . "Neovim/0.10.0")
    ("copilot-integration-id" . "vscode-chat")
    ("openai-organization" . "github-copilot")))

;;;###autoload
(cl-defun gptel-make-copilot
    (name &key curl-args models stream request-params
          (header #'gptel-copilot--request-headers)
          (key #'gptel-copilot--get-session-token)
          (host "api.githubcopilot.com")
          (protocol "https")
          (endpoint "/chat/completions"))
  "Register a GitHub Copilot backend for gptel with NAME.

Keyword arguments:
CURL-ARGS (optional) is a list of additional Curl arguments.
STREAM is a boolean to toggle streaming responses.
HOST (optional) is the API host, defaults to \"api.githubcopilot.com\".
PROTOCOL (optional) specifies the protocol, defaults to https.
ENDPOINT (optional) is the API endpoint for completions.
REQUEST-PARAMS (optional) is a plist of additional HTTP request parameters."
  (declare (indent 1))

  ;; Ensure cache directories exist
  (gptel-copilot--ensure-directories)

  ;; Create and register the backend
  (let ((backend (gptel--make-openai
                  :name name
                  :curl-args curl-args
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (concat protocol "://" host endpoint))))

    ;; Register the backend with gptel
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal) backend))))

(provide 'gptel-copilot)
;;; gptel-copilot.el ends here
