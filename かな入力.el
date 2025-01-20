;;; かな入力.el --- Implementation for かな入力 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Kisaragi Hiu
;;
;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Maintainer: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Created: 1月 20, 2025
;; Modified: 1月 20, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/kisaragi-hiu/かな入力
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'dash)
(require 'quail)
(require 'ucs-normalize)

(defvar かな入力-rules
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

(--each かな入力-rules
  (quail-defrule (car it) (cadr it)))

(quail-define-package
 "japanese-kana-katakana" "Japanese" "カナ" nil
 "Japanese input method for typing Katakana with the かな入力 scheme."
 nil t nil)

(--each かな入力-rules
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

(provide 'かな入力)
;;; かな入力.el ends here
