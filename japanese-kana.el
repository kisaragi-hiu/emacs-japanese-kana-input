;;; japanese-kana.el --- Quail package for typing Japanese with かな入力 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Kisaragi Hiu
;;
;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Created: 2025-01-20
;; Modified: 2025-11-28
;; Version: 0.0.1
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

(require 'dash)
(require 'quail)
(require 'ucs-normalize)

(defvar japanese-kana-rules
  (append
   '(("4%" ["うぇ" "ゑ"])
     ("4E" ["うぃ" "ゐ"])
     ("4[" "ゔ"))
   (-zip-lists
    (list "t[" "g[" "h[" "'[" "b["
          "x[" "d[" "r[" "p[" "c["
          "q[" "a[" "z[" "w[" "s["
          "f[" "v[" "2[" "=[" "-["
          "f]" "v]" "2]" "=]" "-]")
    (->> (string-to-list (concat
                          "がぎぐげご"
                          "ざじずぜぞ"
                          "だぢづでど"
                          "ばびぶべぼ"
                          "ぱぴぷぺぽ"))
         (-map #'string)))
   (-zip-lists
    (->> (string-to-list (concat
                          "`1234567890-="
                          "~!@#$%^&*()_+"
                          "qwertyuiop[]\\"
                          "QWERTYUIOP{}"
                          "asdfghjkl;'"
                          "ASDFGHJKL:\""
                          "zxcvbnm,./"
                          "ZXCVBNM<>?"))
         (-map #'string))
    (->> (string-to-list (concat
                          "ろぬふあうえおやゆよわほへ"
                          "ろぬふぁぅぇぉゃゅょをーへ"
                          "たていすかんなにらせ゛゜む"
                          "たてぃすゕんなにらせ「」"
                          "ちとしはきくまのりれけ"
                          "ちとしはきくまのりれゖ"
                          "つさそひこみもねるめ"
                          "っさそひこみも、。・"))
         (-map #'string)))))

(quail-define-package
 "japanese-kana-hiragana" "Japanese" "かな" t
 "Japanese input method for typing Hiragana with the かな入力 scheme."
 nil t nil)

(--each japanese-kana-rules
  (quail-defrule (car it) (cadr it)))

(quail-define-package
 "japanese-kana-katakana" "Japanese" "カナ" t
 "Japanese input method for typing Katakana with the かな入力 scheme."
 nil t nil)

(--each japanese-kana-rules
  (quail-defrule (car it)
                 (let ((out (cadr it)))
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
