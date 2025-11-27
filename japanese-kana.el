;;; japanese-kana.el --- Quail package for typing Japanese using Kana layout -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Kisaragi Hiu
;;
;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Created: 2025-01-20
;; Modified: 2025-11-28
;; Version: 0.1.0
;; Keywords: multilingual, input method, Japanese
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
(require 'quail)

;; We want the values to be strings even when they can be characters, to make it
;; easy to convert them to katakana.
(defvar japanese-kana-rules
  (append
   '(("4%" ["うぇ" "ゑ"])
     ("4E" ["うぃ" "ゐ"])
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

(defun japanese-kana-update-translation (control-flag)
  "Update Quail translation region for Kana input.
See `quail-update-translation' for what CONTROL-FLAG would be."
  (if (null control-flag)
      (setq quail-current-str
            (if (/= (aref quail-current-key 0) ?q)
                (or quail-current-str quail-current-key)
              ""))
    (if (integerp control-flag)
        (let ((keylen (length quail-current-key)))
          (cond ((= control-flag 0)
                 (setq quail-current-str (aref quail-current-key 0)
                       control-flag t))
                ((= (aref quail-current-key 0) ?n)
                 (setq quail-current-str ?ん)
                 (if (and quail-japanese-use-double-n
                          (> keylen 0)
                          (= (aref quail-current-key 1) ?n))
                     (setq control-flag t)))
                ((and (> keylen 1)
                      (= (aref quail-current-key 0) (aref quail-current-key 1)))
                 (setq quail-current-str ?っ))
                (t
                 (setq quail-current-str (aref quail-current-key 0))))
          (if (integerp control-flag)
              (setq unread-command-events
                    (append
                     (substring quail-current-key control-flag)
                     unread-command-events)))))))

(quail-define-package
 "japanese-kana-hiragana" "Japanese" "かな" t
 "Japanese input method with Kana layout.

The goal is that this skips the Roman-Kana transliteration step in the
builtin \"japanese\" romaji input method with the Kana layout, then does
Kana-Kanji conversion after."
 nil t t
 nil nil nil nil nil
 #'japanese-kana-update-translation
 '(("K" . quail-japanese-toggle-kana)
   (" " . quail-no-conversion)
   ("\C-m" . quail-no-conversion)
   ([return] . quail-no-conversion)))

(dolist (it japanese-kana-rules)
  (quail-defrule (car it) (cdr it)))


(quail-define-package
 "japanese-kana-katakana" "Japanese" "カナ" t
 "Japanese input method for typing Katakana with the かな入力 scheme."
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
