;;; japanese-kana.el --- Quail package for typing Japanese using Kana layout -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Kisaragi Hiu
;;
;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Created: 2025-01-20
;; Modified: 2025-11-28
;; Version: 0.1.0
;; Keywords: i18n, input method, Japanese
;; Homepage: https://github.com/kisaragi-hiu/emacs-japanese-kana
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
(require 'japan-util)
(require 'quail)

;; We want the values to be strings even when they can be characters, to make it
;; easy to convert them to katakana.
(defvar japanese-kana-rules
  (append
   ;; the first two are provided by kkc already
   '(;("4%" ["うぇ" "ゑ"])
     ;("4E" ["うぃ" "ゐ"])
     ("4[" "ゔ"))
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
  "Toggle between Hiragana and Katakana in the translation region."
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

(defun japanese-kana-translation-ret ()
  "Handle RET during translation."
  (interactive)
  (setq quail-converting nil)
  ;; This is needed to ensure half-done translation is still committed.
  (quail-terminate-translation))

(quail-define-package
 "japanese-kana-hiragana" "Japanese" "かな" nil
 "Japanese input method with Kana layout.

The goal is that this skips the Roman-Kana transliteration step in the
builtin \"japanese\" romaji input method with the Kana layout, then does
Kana-Kanji conversion after."
 nil t t
 nil nil nil nil nil
 nil
 '(("K" . japanese-kana-toggle-kana)
   (" " . quail-japanese-kanji-kkc)
   ;; RET also calls these bindings during the translation stage.
   ("\C-m" . japanese-kana-translation-ret)
   ([return] . japanese-kana-translation-ret)))

(dolist (it japanese-kana-rules)
  (quail-defrule (car it) (cdr it)))

(quail-define-package
 "japanese-kana-katakana" "Japanese" "カナ" t
 "Japanese input method for typing Katakana with Kana layout."
 nil t nil)

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

(provide 'japanese-kana)
;;; japanese-kana.el ends here
