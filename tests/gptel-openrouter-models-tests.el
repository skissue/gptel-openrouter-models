;;; gptel-openrouter-models-tests.el --- Tests for gptel-openrouter-models -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for gptel-openrouter-models.

;;; Code:

(require 'ert)
(require 'gptel-openrouter-models)

(defvar gom-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test file.")

;;; Helpers

(defun gom-test--make-model (&rest args)
  "Build a model hashtable from keyword ARGS.
Accepted keys: :input-modalities :params :id :description
:context-length :prompt-price :completion-price."
  (let ((ht (make-hash-table :test 'equal))
        (arch (make-hash-table :test 'equal))
        (pricing (make-hash-table :test 'equal)))
    (puthash "input_modalities"
             (or (plist-get args :input-modalities) '("text"))
             arch)
    (puthash "architecture" arch ht)
    (puthash "supported_parameters"
             (or (plist-get args :params) '())
             ht)
    (puthash "id" (or (plist-get args :id) "test/model") ht)
    (puthash "description" (or (plist-get args :description) "Desc.") ht)
    (puthash "context_length" (or (plist-get args :context-length) 4000) ht)
    (puthash "prompt" (or (plist-get args :prompt-price) "0") pricing)
    (puthash "completion" (or (plist-get args :completion-price) "0") pricing)
    (puthash "pricing" pricing ht)
    ht))

;;; Capabilities

(ert-deftest gom-test-capabilities-text-only ()
  "Text-only model should have no capabilities."
  (let ((model (gom-test--make-model :input-modalities '("text"))))
    (should (null (gptel-openrouter-models--capabilities model)))))

(ert-deftest gom-test-capabilities-media ()
  "Multimodal inputs should yield the media capability."
  (let ((model (gom-test--make-model :input-modalities '("text" "image"))))
    (should (memq 'media (gptel-openrouter-models--capabilities model)))))

(ert-deftest gom-test-capabilities-tool-use ()
  "Tools parameter should yield tool-use capability."
  (let ((model (gom-test--make-model :params '("tools"))))
    (should (memq 'tool-use (gptel-openrouter-models--capabilities model)))))

(ert-deftest gom-test-capabilities-json-response-format ()
  "response_format parameter should yield json capability."
  (let ((model (gom-test--make-model :params '("response_format"))))
    (should (memq 'json (gptel-openrouter-models--capabilities model)))))

(ert-deftest gom-test-capabilities-json-structured-outputs ()
  "structured_outputs parameter should also yield json capability."
  (let ((model (gom-test--make-model :params '("structured_outputs"))))
    (should (memq 'json (gptel-openrouter-models--capabilities model)))))

(ert-deftest gom-test-capabilities-reasoning ()
  "reasoning parameter should yield reasoning capability."
  (let ((model (gom-test--make-model :params '("reasoning"))))
    (should (memq 'reasoning (gptel-openrouter-models--capabilities model)))))

(ert-deftest gom-test-capabilities-include-reasoning ()
  "include_reasoning parameter should also yield reasoning capability."
  (let ((model (gom-test--make-model :params '("include_reasoning"))))
    (should (memq 'reasoning (gptel-openrouter-models--capabilities model)))))

(ert-deftest gom-test-capabilities-all ()
  "Model with all features should have all capabilities."
  (let* ((model (gom-test--make-model
                 :input-modalities '("text" "image")
                 :params '("tools" "response_format" "reasoning")))
         (caps (gptel-openrouter-models--capabilities model)))
    (should (memq 'media caps))
    (should (memq 'tool-use caps))
    (should (memq 'json caps))
    (should (memq 'reasoning caps))))

;;; MIME types

(ert-deftest gom-test-mime-types-text-only ()
  "Text-only model should have no MIME types."
  (let ((model (gom-test--make-model :input-modalities '("text"))))
    (should (null (gptel-openrouter-models--mime-types model)))))

(ert-deftest gom-test-mime-types-image ()
  "Image modality should produce image MIME types."
  (let ((types (gptel-openrouter-models--mime-types
                (gom-test--make-model :input-modalities '("text" "image")))))
    (should (member "image/jpeg" types))
    (should (member "image/png" types))
    (should (member "image/gif" types))
    (should (member "image/webp" types))))

(ert-deftest gom-test-mime-types-file ()
  "File modality should produce application/pdf."
  (let ((types (gptel-openrouter-models--mime-types
                (gom-test--make-model :input-modalities '("text" "file")))))
    (should (member "application/pdf" types))))

(ert-deftest gom-test-mime-types-audio ()
  "Audio modality should produce audio MIME types."
  (let ((types (gptel-openrouter-models--mime-types
                (gom-test--make-model :input-modalities '("text" "audio")))))
    (should (member "audio/mpeg" types))
    (should (member "audio/wav" types))))

(ert-deftest gom-test-mime-types-video ()
  "Video modality should produce video/mp4."
  (let ((types (gptel-openrouter-models--mime-types
                (gom-test--make-model :input-modalities '("text" "video")))))
    (should (member "video/mp4" types))))

(ert-deftest gom-test-mime-types-all ()
  "All modalities should produce all MIME types."
  (let ((types (gptel-openrouter-models--mime-types
                (gom-test--make-model
                 :input-modalities '("text" "image" "file" "audio" "video")))))
    (should (= (length types) 8))))

;;; Parse model

(ert-deftest gom-test-parse-model-id ()
  "Model id should be interned as a symbol."
  (let* ((model (gom-test--make-model :id "vendor/model-name"))
         (result (gptel-openrouter-models--parse-model model)))
    (should (eq (car result) 'vendor/model-name))))

(ert-deftest gom-test-parse-model-description-truncation ()
  "Description should be truncated at the first newline."
  (let* ((model (gom-test--make-model :description "First line.\nSecond line."))
         (result (gptel-openrouter-models--parse-model model)))
    (should (equal (plist-get (cdr result) :description) "First line."))))

(ert-deftest gom-test-parse-model-context-window ()
  "Context window should be divided by 1000."
  (let* ((model (gom-test--make-model :context-length 128000))
         (result (gptel-openrouter-models--parse-model model)))
    (should (= (plist-get (cdr result) :context-window) 128))))

(ert-deftest gom-test-parse-model-pricing ()
  "Pricing should be converted to $/million tokens."
  (let* ((model (gom-test--make-model :prompt-price "0.000003"
                                      :completion-price "0.000015"))
         (result (gptel-openrouter-models--parse-model model)))
    (should (= (plist-get (cdr result) :input-cost) 3.0))
    (should (= (plist-get (cdr result) :output-cost) 15.0))))

(ert-deftest gom-test-parse-model-no-mime-types-when-text-only ()
  "Text-only models should not have :mime-types in the plist."
  (let* ((model (gom-test--make-model :input-modalities '("text")))
         (result (gptel-openrouter-models--parse-model model)))
    (should-not (plist-member (cdr result) :mime-types))))

(ert-deftest gom-test-parse-model-mime-types-present ()
  "Multimodal models should have :mime-types in the plist."
  (let* ((model (gom-test--make-model :input-modalities '("text" "image")))
         (result (gptel-openrouter-models--parse-model model)))
    (should (plist-member (cdr result) :mime-types))))

;;; Integration: full JSON parse

(ert-deftest gom-test-full-parse-fixture ()
  "Parsing the fixture JSON should return the correct number of models."
  (let ((gptel-openrouter-models--file
         (expand-file-name "test-models.json" gom-test--dir)))
    (let ((models (gptel-openrouter-models)))
      (should (= (length models) 3))
      ;; First model: text-only/basic
      (let ((m (assq 'text-only/basic models)))
        (should m)
        (should (= (plist-get (cdr m) :context-window) 4))
        (should (null (plist-get (cdr m) :capabilities))))
      ;; Second model: multimodal/full
      (let ((m (assq 'multimodal/full models)))
        (should m)
        (should (= (plist-get (cdr m) :context-window) 128))
        (should (memq 'media (plist-get (cdr m) :capabilities)))
        (should (plist-member (cdr m) :mime-types)))
      ;; Third model: vision/tools-only
      (let ((m (assq 'vision/tools-only models)))
        (should m)
        (should (memq 'tool-use (plist-get (cdr m) :capabilities)))
        (should (memq 'json (plist-get (cdr m) :capabilities)))
        (should (memq 'reasoning (plist-get (cdr m) :capabilities)))))))

(provide 'gptel-openrouter-models-tests)

;;; gptel-openrouter-models-tests.el ends here
