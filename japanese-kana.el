;;; japanese-kana.el --- Quail package for typing Japanese using Kana layout -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Kisaragi Hiu
;;
;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Created: 2025-01-20
;; Modified: 2025-11-28
;; Version: 0.2.1
;; Keywords: i18n, input method, Japanese
;; Homepage: https://github.com/kisaragi-hiu/emacs-japanese-kana-input
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; JISかな入力。
;;
;;; Code:

(require 'cl-lib)
(require 'quail)
(require 'kkc)

;; We want the values to be strings even when they can be characters, to make it
;; easy to convert them to katakana.
(defvar japanese-kana-rules
  (append
   '(("4[" "ゔ"))
   (cl-mapcar
    #'cons
    (list "t[" "g[" "h[" "'[" "b["
          "x[" "d[" "r[" "p[" "c["
          "q[" "a[" "z[" "w[" "s["
          "f[" "v[" "2[" "=[" "-["
          "f]" "v]" "2]" "=]" "-]")
    (mapcar #'string
            (string-to-list (concat
                             "がぎぐげご"
                             "ざじずぜぞ"
                             "だぢづでど"
                             "ばびぶべぼ"
                             "ぱぴぷぺぽ"))))
   (cl-mapcar
    #'cons
    (mapcar #'string
            (string-to-list (concat
                             "`1234567890-="
                             "~!@#$%^&*()_+"
                             "qwertyuiop[]\\"
                             "QWERTYUIOP{}"
                             "asdfghjkl;'"
                             "ASDFGHJKL:\""
                             "zxcvbnm,./"
                             "ZXCVBNM<>?")))
    (mapcar #'string
            (string-to-list (concat
                             "ろぬふあうえおやゆよわほへ"
                             "ろぬふぁぅぇぉゃゅょをーへ"
                             "たていすかんなにらせ゛゜む"
                             "たてぃすゕんなにらせ「」"
                             "ちとしはきくまのりれけ"
                             "ちとしはきくまのりれゖ"
                             "つさそひこみもねるめ"
                             "っさそひこみも、。・"))))))

(defun japanese-kana-toggle-kana ()
  "Toggle between Hiragana and Katakana in the translation region.
This is an adapted version of `quail-japanese-toggle-kana'."
  (interactive)
  ;; This part is copied from `quail-japanese-toggle-kana'.
  (setq quail-translating nil)
  (let ((start (overlay-start quail-conv-overlay))
        (end (overlay-end quail-conv-overlay)))
    (save-excursion
      (goto-char start)
      (if (re-search-forward "\\cH" end t)
          (japanese-katakana-region start end)
        (japanese-hiragana-region start end)))
    (setq quail-conversion-str
          (buffer-substring (overlay-start quail-conv-overlay)
                            (overlay-end quail-conv-overlay))))

  ;; Without this, if we type a character that can have dakuten like ち and
  ;; convert it to Katakana then press RET, it will commit both チ and ち.
  (setq quail-current-str nil))

(defun japanese-kana-kanji-kkc ()
  "Convert Hiragana in the current region to Kanji through KKC.
This is an adapted version of `quail-japanese-kanji-kkc'."
  (interactive)
  ;; This is `quail-japanese-kanji-kkc' except the n -> ん handling is removed.
  ;; Even if it is present it doesn't actually have any effect, but we also need
  ;; this function inlined anyways, so we might as well remove it.
  (let* ((from (copy-marker (overlay-start quail-conv-overlay)))
         (len (- (overlay-end quail-conv-overlay) from)))
    (quail-delete-overlays)
    (setq quail-current-str nil)
    (unwind-protect
        (let ((result (kkc-region from (+ from len))))
          (move-overlay quail-conv-overlay from (point))
          (setq quail-conversion-str (buffer-substring from (point)))
          (if (= (+ from result) (point))
              (setq quail-converting nil))
          (setq quail-translating nil))
      (set-marker from nil))))

(defun japanese-kana-translation-ret ()
  "Handle RET during translation."
  (interactive)
  (setq quail-converting nil)
  ;; This is needed to ensure half-done translation is still committed.
  (quail-terminate-translation))

(quail-define-package
 "japanese-kana" "Japanese" "かな" nil
 "Japanese input method with Kana layout.

When this input method is used, there are still roughly two stages: Kana
translation and Kana-Kanji conversion.

Both stages are as similar to the builtin \"japanese\" Romaji input
method as possible. During Kana translation, these commands are also
available, just as they are in the \"japanese\" Romaji input method:

- Press K to toggle between Hiragana and Katakana.
- Press RET to accept the current character sequence.
- Press SPC to proceed to Kana-Kanji conversion.

Kana-Kanji conversion happens the same way as well.

This input method does not provide the temporary ASCII input mode or
hankaku input.

The input method `japanese-kana-katakana' is also provided for typing
just Katakana with the Kana layout."
 nil t t
 nil nil nil nil nil
 ;; The default update-translation-functions works
 nil
 '(("K" . japanese-kana-toggle-kana)
   (" " . japanese-kana-kanji-kkc)
   ;; RET also calls these bindings during the translation stage.
   ("\C-m" . japanese-kana-translation-ret)
   ([return] . japanese-kana-translation-ret)))

(dolist (it japanese-kana-rules)
  (quail-defrule (car it) (cdr it)))

(quail-define-package
 "japanese-kana-katakana" "Japanese" "カナ" nil
 "Japanese input method for typing Katakana with Kana layout."
 nil t t
 nil nil nil nil nil
 nil
 '(("K" . japanese-kana-toggle-kana)
   (" " . japanese-kana-translation-ret)
   ("\C-m" . japanese-kana-translation-ret)
   ([return] . japanese-kana-translation-ret)))

(dolist (it japanese-kana-rules)
  (quail-defrule (car it)
                 (let ((out (cdr it)))
                   (cond ((stringp out)
                          (japanese-katakana out))
                         ((vectorp out)
                          (let ((nv (make-vector (length out) nil)))
                            (dotimes (i (length out))
                              (aset nv i (japanese-katakana (elt out i))))
                            nv))
                         (t out)))))

;;;###autoload
(register-input-method "japanese-kana" "Japanese"
                       (lambda (im)
                         (require 'japanese-kana)
                         (quail-use-package im))
                       "かな" "Japanese input method with Kana layout.")

;;;###autoload
(register-input-method "japanese-kana-katakana" "Japanese"
                       (lambda (im)
                         (require 'japanese-kana)
                         (quail-use-package im))
                       "カナ" "Japanese input method for typing Katakana with Kana layout.")

(provide 'japanese-kana)
;;; japanese-kana.el ends here
