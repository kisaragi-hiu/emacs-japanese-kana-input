;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'with-simulated-input)
(require 'japanese-kana)
(require 'buttercup)


;; Basic structure of this was informed by evil-test-buffer. As in, I
;; macroexpanded a call and the simplified it down to arrive at this function
;; body.
(defmacro japanese-kana-simulate-input (input-method input)
  "Return what typing INPUT using INPUT-METHOD would insert."
  (declare (indent 1))
  `(let ((inhibit-message t))
     (with-temp-buffer
       ;; This is necessary for some reason, otherwise the execute-kbd-macro will
       ;; run somewhere else?
       (switch-to-buffer (current-buffer))
       (activate-input-method ,input-method)
       (execute-kbd-macro
        (kbd ,input))
       (buffer-string))))

(describe "japanese-kana"
  (it "can be switched to"
    (expect (with-temp-buffer
              (activate-input-method "japanese-kana")
              current-input-method)
            :to-equal "japanese-kana"))
  (it "types correctly"
    (expect (japanese-kana-simulate-input "japanese-kana"
              "aexu RET")
            :to-equal "ちいさな")
    (expect (japanese-kana-simulate-input "japanese-kana"
              "a[ RET")
            :to-equal "ぢ")
    (expect (japanese-kana-simulate-input "japanese-kana"
              "k RET")
            :to-equal "の"))
  (it "ESC also keeps the text"
    (expect (japanese-kana-simulate-input "japanese-kana"
              "aexu ESC")
            :to-equal "ちいさな")
    (expect (japanese-kana-simulate-input "japanese-kana"
              "kf[o ESC")
            :to-equal "のばら")))
