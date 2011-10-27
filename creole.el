;;; creole.el --- A parser for the Creole Wiki language

;;; Copyright (C) 2011 by Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 27th October 2011
;; Version: 0.1
;; Keywords: lisp, creole, wiki

;;; Commentary:

;; This is a WikiCreole wiki parser. WikiCreole is something like the
;; Wiki language used by OddMuse, which is the EmacsWiki wiki
;; language.

;; WikiCreole is an emerging standard documented at:
;; http://www.wikicreole.org/wiki/Creole1.0

;;; Code:

(defun creole-link-parse (text)
  "Parse TEXT for creole links.

In the future we need to have some sort of resolution system here?

Possibly it would be good to orthongonaly update some list of
links."
  (replace-regexp-in-string 
   "\\[\\[\\(\\([A-Za-z]+:\\)*[^|]+\\)\\(|\\(\\([^]]+\\)\\)\\)*\\]\\]"
   (lambda (m)
     (apply 
      'format 
      (append 
       '("<a href='%s'>%s</a>")
       (cond
        ;; We have both a url and a link
        ((match-string 3 m)
         (list 
          (match-string 1 m)
          (match-string 4 m)))
        ;; We only have a url
        ((match-string 1 m)
         (list 
          (match-string 1 m)
          (match-string 1 m)))))))
   text))

(ert-deftest creole-link-parse ()
  (should (equal "<a href='http://thing'>thing</a>"
                 (creole-link-parse "[[http://thing|thing]]")))
  (should (equal "<a href='thing'>fing!</a>"
                 (creole-link-parse "[[thing|fing!]]")))
  (should (equal "<a href='thing'>thing</a>"
                 (creole-link-parse "[[thing]]")))
  (should (equal "<a href='thing'>thing
broken over lines</a>"
                 (creole-link-parse "[[thing|thing
broken over lines]]"))))


(defun creole-block-parse (text)
  "Parses TEXT as a creole block.

A creole block is a paragraph or list item that can include
links, italic, bold, line break or inline preformatted markup."
  (replace-regexp-in-string 
   "\\*\\*\\(\\(.\\|\n\\)*\\)\\*\\*" 
   "<strong>\\1</strong>"
   (replace-regexp-in-string 
    "//\\(\\(.\\|\n\\)*\\)//" 
    "<em>\\1</em>"
    (replace-regexp-in-string 
     "{{{\\(\\(.\\|\n\\)*\\)}}}" 
     "<code>\\1</code>"
     (replace-regexp-in-string 
      "\\\\" 
      "<br/>"
      (creole-link-parse text))))))


(ert-deftest creole-block-parse ()
  (should (equal "<strong>this is bold</strong>"
                 (creole-block-parse "**this is bold**")))
  (should (equal "<em>this is italic</em>"
                 (creole-block-parse "//this is italic//")))
  (should (equal "<code>this is code</code>"
                 (creole-block-parse "{{{this is code}}}")))
  (should (equal "this has a<br/>line break"
                 (creole-block-parse "this has a\\line break")))
  (should (equal "<em><strong>this is italic bold</strong></em>"
                 (creole-block-parse "//**this is italic bold**//")))
  (should (equal "<strong><em>this is bold italic</em></strong>"
                 (creole-block-parse "**//this is bold italic//**")))
  (should (equal "<a href='http://thing'>thing</a>"
                 (creole-block-parse "[[http://thing|thing]]")))
  (should (equal "<a href='thing'><strong>fing!</strong></a>"
                 (creole-block-parse "[[thing|**fing!**]]")))
  (should (equal "<a href='thing'><code>some code</code></a>"
                 (creole-block-parse "[[thing|{{{some code}}}]]")))
  (should (equal "<a href='thing'>thing</a>"
                 (creole-block-parse "[[thing]]")))
  (should (equal "<a href='thing'>thing
broken over lines</a>"
                 (creole-block-parse "[[thing|thing
broken over lines]]"))))


(defun creole-tokenize (docbuf)
  "Parse DOCBUF which is full of creole wiki text.

See http://www.wikicreole.org/wiki/Creole1.0 for more information
on WikiCreole.

Returns a list of parsed elements."
  (with-current-buffer docbuf
    (goto-char (point-min))
    (let ((res '()))
      (while (not (eobp))
        (cond
         (;; Heading
          (looking-at "^\\(=+\\)[ \t]")
          (let ((level (length (match-string 1))))
            ;; Actually, the end = is optional... not sure if, when
            ;; there is an end = it has to be the same number as the
            ;; first one
            (if (not (re-search-forward "^\\(=+\\)[ \t]+\\(.*\\)[ \t]+\\(=+\\)$" nil 't))
                (error "Creole: badly formatted heading"))
            (when (equal (length (match-string 3))
                         level)
              (setq res (append res
                                (list
                                 (cons
                                  (intern (format "heading%s" level))
                                  ;; The string that is the heading
                                  ;; - any internal rules we should
                                  ;; deal with here
                                  (match-string 2)))))
              (forward-line))))
         (;; Unordered list item
          (looking-at "^\\(\\*+\\)[ \t]\\(.*\\)")
          (let ((level (length (match-string 1))))
            (setq res (append res
                              (list
                               (cons
                                (intern (format "ul%s" level))
                                ;; The string that is the heading
                                ;; - any internal rules we should
                                ;; deal with here
                                (match-string 2)))))
            (forward-line)))
         (;; Ordered list item
          (looking-at "^\\(#+\\)[ \t]\\(.*\\)")
          (let ((level (length (match-string 1))))
            (setq res (append res
                              (list
                               (cons
                                (intern (format "ol%s" level))
                                ;; The string that is the heading
                                ;; - any internal rules we should
                                ;; deal with here
                                (match-string 2)))))
            (forward-line)))
         (;; Horizontal rule
          (looking-at "^[ \t]*----[ \t]*$")
          (setq res (append res
                            (list
                             (cons 'hr ""))))
          (forward-line))
         (;; Pre-formatted block
          (looking-at "^\n{{{$")
          (if (not (re-search-forward "^\n{{{\n\\(\\(.\\|\n\\)*?\\)\n}}}$" nil t))
              (error "Creole: bad preformatted block"))
          (setq res (append res
                              (list
                               (cons 'preformatted (match-string 1)))))
          (forward-line))
         (;; Paragraph line
          (and (looking-at "^[^=*]")
               (not (looking-at "^$")))
          (let* ((start (point))
                 (end 
                  (save-match-data
                    (let* ((matched-end 
                            ;; Find the end - the end is actually BEFORE this
                            (re-search-forward "\\(^$\\)\\|\\(^[=*]\\)" nil 't))
                           (matched (if matched-end (match-string 0))))
                      (cond
                       ((equal matched "") (- matched-end 1))
                       ((equal matched "*") (- matched-end 2))
                       ((equal matched "=") (- matched-end 2))
                       (t
                        (point-max)))))))
            (setq res 
                  (append 
                   res
                   (list
                    (cons 'para (buffer-substring start end)))))
            (goto-char end)))
         ('t
          (forward-line))))
      res)))

(defun creole--test-doc (buffer)
  "Insert a test document of creole text into BUFFER."
  (with-current-buffer buffer
    (insert "= Heading! =\n")
    (insert "\n")
    (insert "== Heading2! ==\n")
    (insert "# an ordered list item\n## a 2nd ordered list item\n")
    (insert "== Heading3 is a multi word heading ==\n")
    (insert "\n{{{\n== this is preformatted ==\n{{\nIt looks great\n}}\n}}}\n")
    (insert "* list item\n** 2nd list item\n*** 3rd list item\n")
    (insert "** another 2nd list item\n*** another 3rd list item\n")
    (insert " ----\n")
    (insert "This is a paragraph\nthat runs over several lines\n* and a list item stops it\n")
    (insert "This is a paragraph {{{with code}}} and [[links]]
and **bold** and //italics//.")))

(ert-deftest creole-tokenize-newline-doc-end ()
  "Specific test for dealing with carriage return as the end."
  (with-temp-buffer
    (insert "= Heading! =\n")
    (insert "This is a paragraph {{{with code}}} and [[links]]\n")
    (should 
     (equal 
      (creole-tokenize (current-buffer))
      '((heading1 . "Heading!")
        (para . "This is a paragraph {{{with code}}} and [[links]]"))))))

(ert-deftest creole-tokenize ()
  (with-temp-buffer
    (creole--test-doc (current-buffer))
    (should 
     (equal 
      (creole-tokenize (current-buffer))
      '((heading1 . "Heading!")
        (heading2 . "Heading2!")
        (ol1 . "an ordered list item")
        (ol2 . "a 2nd ordered list item")
        (heading2 . "Heading3 is a multi word heading")
        (preformatted . "== this is preformatted ==\n{{\nIt looks great\n}}")
        (ul1 . "list item")
        (ul2 . "2nd list item")
        (ul3 . "3rd list item")
        (ul2 . "another 2nd list item")
        (ul3 . "another 3rd list item")
        (hr . "")
        (para . "This is a paragraph\nthat runs over several lines")
        (ul1 . "and a list item stops it")
        (para . "This is a paragraph {{{with code}}} and [[links]]
and **bold** and //italics//."))))))

(defun creole--list-item (list-symbol)
  "Return the type and the level of the LIST-SYMBOL.

For example:

 (creole--list-item 'ol1) 
  => (ordered . 1)

 (creole--list-item 'ul10) 
  => (unordered . 10)"
  (save-match-data
    (let ((s (symbol-name list-symbol)))
      (when (string-match "\\([uo]l\\)\\([0-9]+\\)" s)
        (cons
         (intern (match-string 1 s))
         (string-to-int (match-string 2 s)))))))

(ert-deftest creole--list-item ()
  "Test the little creole list item function."
  (should (equal '(ul . 1) (creole--list-item 'ul1)))
  (should (equal '(ul . 10) (creole--list-item 'ul10)))
  (should (equal '(ul . 7) (creole--list-item 'ul7)))
  (should (equal '(ol . 7) (creole--list-item 'ol7)))
  (should (equal nil (creole--list-item 'h1))))

(defun creole-structure (lst)
  "Make a parsed structure from a list."
  (let* ((docptr lst)
         (state '()) ; used as a stack
         (result '()))
    (while docptr
      (let* ((token (car docptr))
             (lst-item (creole--list-item (car token))))
        (case (if lst-item 'listitem (car token))
          (listitem
           (let* ((last (if (car state) (cdar state)))
                  (last-level (if (car state) (caar state)))
                  (new (list (car lst-item) (cdr token))))
             (cond
              ;; Current level is higher than the last, embed a new list
              ((and last               
                    (> (cdr lst-item) last-level))
               (setcdr last (append (cdr last) (list new)))
               ;; Update the stack
               (push (cons (cdr lst-item) new) state))
              ;; Current level is same as the last, extend the last list
              ((and last
                    (= (cdr lst-item) last-level))
               (setq new (list (cdr token)))
               (setcdr last (append (cdr last) new))
               ;; Reset the top of the stack
               (pop state)
               (push (cons (cdr lst-item) new) state))
              ;; Current level is same as the last, extend the last list
              ((and last               
                    (< (cdr lst-item) last-level))
               (loop for i from 1 to (- last-level (cdr lst-item))
                     do (pop state))
               (let* ((last (if (car state) (cdar state)))
                      (last-level (if (car state) (caar state))))
                 (setq new (list (cdr token)))
                 (setcdr last (append (cdr last) new))))
              ;; The default action when we're dealing with lists
              (t
               (setq result (append result (list new)))
               ;; Update the stack
               (push (cons (cdr lst-item) new) state)))))
          ;; Not a list item - just push it onto the result, always empty the list state
          (t
           (setq state '())
           (setq result (append result (list token))))))
      (setq docptr (cdr docptr)))
    result))

(ert-deftest creole-structure ()
  "Testing tokenize lists to parsed representations.

The test here takes a list that would come from 'creole-tokenize'
and checks it against what should come out of 'creole-structure'.
In other words, a parsed creole document representation."
  (should 
   (equal
    (creole-structure 
     '((h1 . "this is a heading!")
       (ul1 . "this is a first item")
       (ul1 . "this is a 2nd first level item")
       (ul2 . "this is a first 2nd level item")
       (ul2 . "this is a 2nd 2nd level item")
       (ul3 . "this is a first 3rd level item")
       (ul1 . "this is a return to the first level item")
       (ul1 . "this is a 2nd first level item")
       (ul2 . "this is a first 2nd level item")
       (ul2 . "this is a 2nd 2nd level item")
       (ul1 . "this is another return to first level item")))
    '((h1 . "this is a heading!")
      (ul
       "this is a first item"
       "this is a 2nd first level item"
       (ul
        "this is a first 2nd level item"
        "this is a 2nd 2nd level item"
        (ul
         "this is a first 3rd level item"))
       "this is a return to the first level item"
       "this is a 2nd first level item"
       (ul
        "this is a first 2nd level item"
        "this is a 2nd 2nd level item")
       "this is another return to first level item")))))

(ert-deftest creole-structure-end-to-end ()
  "Test the parser directly with the result of the tokenizer.

'creole-tokenize' is called on a buffer and checked against what
should come out."
  (with-temp-buffer
    (insert "= this is a heading! =\n")
    (insert "* this is a first item\n")
    (insert "* this is a 2nd first level item\n")
    (insert "** this is a first 2nd level item\n")
    (insert "** this is a 2nd 2nd level item\n")
    (insert "*** this is a first 3rd level item\n")
    (insert "* this is a return to the first level item\n")
    (insert "* this is a 2nd first level item\n")
    (insert "** this is a first 2nd level item\n")
    (insert "** this is a 2nd 2nd level item\n")
    (insert "* this is another return to first level item\n")
    (insert "This is a paragraph\nthat runs over several lines\n* and a list item stops it\n")
    (insert "This is a paragraph {{{with code}}} and [[links]]\n")
    (should 
     (equal
      (creole-structure (creole-tokenize (current-buffer)))
      '((heading1 . "this is a heading!")
        (ul
         "this is a first item"
         "this is a 2nd first level item"
         (ul
          "this is a first 2nd level item"
          "this is a 2nd 2nd level item"
          (ul
           "this is a first 3rd level item"))
         "this is a return to the first level item"
         "this is a 2nd first level item"
         (ul
          "this is a first 2nd level item"
          "this is a 2nd 2nd level item")
         "this is another return to first level item")
        (para . "This is a paragraph
that runs over several lines")
        (ul "and a list item stops it")
        (para . "This is a paragraph {{{with code}}} and [[links]]"))))))

(defun creole--html-list (type lst)
  "Export the specified LST in HTML.

The exported HTML is written into the current buffer. 

This is NOT intended to be used by anything but 'creole-export-html'."
  (let ((first t))
    (insert "<" (symbol-name type) ">\n")
    (loop for item in lst
          do (progn
               (cond
                ((listp item)
                 (creole--html-list (car item) (cdr item))
                 (setq first nil))
                (t
                 (if (not first)
                     (insert "</li>\n"))
                 (setq first nil)
                 (insert "<li>")
                 (insert item)))))
    (insert "</li>\n")
    (insert "</" (symbol-name type) ">\n")))

(ert-deftest creole--html-list ()
  "Test the list export, which is a little complex."
  (with-temp-buffer
    (creole--html-list 
     'ul 
     '("this is a list" 
       (ul "with a deeper list") 
       "and another item on the end"))
    (should (equal (buffer-substring-no-properties (point-min)(point-max))
                   "<ul>
<li>this is a list<ul>
<li>with a deeper list</li>
</ul>
</li>
<li>and another item on the end</li>
</ul>
"))))

(defun* creole-html (docbuf 
                     &optional html-buffer 
                     &key result-mode
                     (erase-existing t)
                     switch-to)
  "Export DOCBUF as HTML to HTML-BUFFER.

If HTML-BUFFER does not exist then a buffer is created based on
the name of DOCBUF. If DOCBUF doesn't have a name then the
destination buffer is called:

 *creolehtml*

If RESULT-MODE is specified then the HTML-BUFFER is placed in
that mode.

If ERASE-EXISTING is not nil then any existing content in the
HTML-BUFFER is erased before rendering.  By default this is true.

If SWITCH-TO is not nil then the HTML-BUFFER is switched to when
the export is done.

When called interactively RESULT-MODE is set to 'html-mode',
ERASE-EXISTING is set to true and SWITCH-TO is set to true.

Returns the HTML-BUFFER."
  (interactive (list
                (read-buffer "Creole buffer: " (current-buffer))
                nil
                :result-mode 'html-mode
                :switch-to 't))
  (let ((result-buffer ; make up the result buffer
         (or html-buffer 
             (get-buffer-create 
              (replace-regexp-in-string 
               "\\(\\**\\)\\(.*\\)\\(\\**\\)"
               "*creolehtml-\\1*"
               (buffer-name (if (bufferp docbuf) docbuf (get-buffer docbuf))))))))
    (let ((creole (creole-structure (creole-tokenize docbuf))))  ; Get the parsed creole doc
      (with-current-buffer result-buffer
        (if erase-existing (erase-buffer)) ; Erase if we were asked to
        (loop for element in creole
              do 
              (let ((syntax (car element)))
                (case syntax
                  ;; The list elements can follow on from each other and require special handling
                  ((ul ol)
                   ;; FIXME lists don't do block level replacement yet!
                   (creole--html-list syntax (cdr element)))
                  (heading1
                   (insert (format "<h1>%s</h1>\n" (cdr element))))
                  (heading2
                   (insert (format "<h2>%s</h2>\n" (cdr element))))
                  (heading3
                   (insert (format "<h3>%s</h3>\n" (cdr element))))
                  (preformatted
                   (insert (format "<pre>\n%s\n</pre>\n" (cdr element))))
                  (hr
                   (insert "<hr/>\n"))
                  (para
                   (insert (format "<p>%s</p>\n"
                                   (creole-block-parse (cdr element))))))))
        (if result-mode (call-interactively result-mode)))
      (if switch-to (switch-to-buffer result-buffer))
      result-buffer)))

(ert-deftest creole-list-to-html ()
  "Test lists (which are a little complicated) export correctly."
  (with-temp-buffer
    (insert "* list item
** 2nd list item")
    (let ((html (creole-html (current-buffer))))
      (with-current-buffer html
        (goto-char (point-min))
        (should (looking-at "<ul>
<li>list item<ul>
<li>2nd list item</li>
</ul>
</li>
</ul>
"))))))

(ert-deftest creole-html ()
  "Test the HTML export end to end."
  (with-temp-buffer
    (creole--test-doc (current-buffer))
    (let ((html (creole-html (current-buffer))))
      (with-current-buffer html
        (goto-char (point-min))
        (should (looking-at "<h1>Heading!</h1>
<h2>Heading2!</h2>
<ol>
<li>an ordered list item<ol>
<li>a 2nd ordered list item</li>
</ol>
</li>
</ol>
<h2>Heading3 is a multi word heading</h2>
<pre>
== this is preformatted ==
{{
It looks great
}}
</pre>
<ul>
<li>list item<ul>
<li>2nd list item<ul>
<li>3rd list item</li>
</ul>
</li>
<li>another 2nd list item<ul>
<li>another 3rd list item</li>
</ul>
</li>
</ul>
</li>
</ul>
<hr/>
<p>This is a paragraph
that runs over several lines</p>
<ul>
<li>and a list item stops it</li>
</ul>
<p>This is a paragraph <code>with code</code> and <a href='links'>links</a>
and <strong>bold</strong> and <em>italics</em>.</p>
"))))))

(provide 'creole)

;;; creole.el ends here
