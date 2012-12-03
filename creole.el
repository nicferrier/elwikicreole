;;; creole.el --- A parser for the Creole Wiki language

;;; Copyright (C) 2011, 2012 by Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 27th October 2011
;; Version: 0.8.18
;; Keywords: lisp, creole, wiki

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a WikiCreole wiki parser. WikiCreole is something like the
;; Wiki language used by OddMuse, which is the EmacsWiki wiki
;; language.

;; This code was originally written to mark the death of John McCarthy
;; - http://news.ycombinator.com/item?id=3151988

;; WikiCreole is an emerging standard documented at:
;; http://www.wikicreole.org/wiki/Creole1.0

;;; Code:

(require 'ert)
(require 'htmlfontify)
(require 'org-table)
(require 'cl)

(defgroup creole nil
  "A WikiCreole parser and associated tools."
  :group 'hypertext)

(defun creole-link-parse (text)
  "Parse TEXT for creole links.

In the future we need to have some sort of resolution system here?

Possibly it would be good to orthongonaly update some list of
links."
  (replace-regexp-in-string
   "\\[\\[\\(\\([A-Za-z]+:\\)*[^]|]+\\)\\(|\\(\\([^]]+\\)\\)\\)*\\]\\]"
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

(defvar creole-image-class nil
  "A default class to be applied to wiki linked images.")


(defun creole-image-parse (text)
  "Parse TEXT for creole images.

Images should have this format:

{{image.jpg?size=50x100|description}}

where the size and description is optional, and the second
dimension in size can be omitted.

The 'size=' is optional, and I keep there because this way you
could add more parameters to the image if you needed them. By
now, a size is supposed, and the values are assumed to be either
a Width, or a WidthxHeight specification.
"
  (replace-regexp-in-string
   "{{\\([^?|}]+\\)\\(\\?\\([^?|}]+\\)\\)*\\(|\\([^}]+\\)\\)?}}"
   (lambda (m)
     (let (title)
       (apply
        'format
        (append
         '("<img %ssrc='%s' alt='%s' %s%s/>")
         (list
          ;; Whether we have a class to apply or not
          (if creole-image-class (format "class='%s' " creole-image-class) "")
          ;; URL of the image
          (match-string 1 m)
          ;; if we don't have an alternate, use the URL
          (if (match-string 4 m)
              (setq title (match-string 5 m))
              (match-string 1 m))
          ;; title
          (if title (format "title='%s' " title) "")
          ;; Match only the size part for now
          (if (match-string 2 m)
              (let ((options (match-string 3 m)))
                (save-match-data
                  ;; 'size=' is optional and is the only parameter right now
                  (string-match "\\([0-9]+\\)\\(x\\([0-9]+\\)\\)?" options)
                  (when (match-string 1 options)
                    (concat
                     "width='" (match-string 1 options) "' "
                     (when (match-string 2 options)
                       (concat "height='" (match-string 3 options) "' "))))))
              ""))))))
   text))

(ert-deftest creole-image-parse ()
  (should (equal "<img src='image.jpg' alt='whatever I tell you' title='whatever I tell you' width='20' height='1000' />"
                 (creole-image-parse "{{image.jpg?size=20x1000|whatever I tell you}}")))
  (should (equal "<img src='image.jpg' alt='image.jpg' />"
                 (creole-image-parse "{{image.jpg}}")))
  (should (equal "<img src='image.jpg' alt='alternate text' title='alternate text' />"
                 (creole-image-parse "{{image.jpg|alternate text}}")))
  (should (equal "<img src='image.jpg' alt='image.jpg' width='20' />"
                 (creole-image-parse "{{image.jpg?size=20}}")))
  (should (equal "<img src='image.jpg' alt='alternate text' title='alternate text' width='20' />"
                 (creole-image-parse "{{image.jpg?size=20|alternate text}}")))
  (should (equal "<img src='image.jpg' alt='image.jpg' width='20' height='10' />"
                 (creole-image-parse "{{image.jpg?size=20x10}}"))))

(defun creole-block-parse (text)
  "Parses TEXT as a creole block.

A creole block is a paragraph or list item that can include
links, italic, bold, line break or inline preformatted markup.

Returns a copy of TEXT with the WikiCreole replaced with
appropriate HTML."
  (creole-image-parse
   (creole-link-parse
    (replace-regexp-in-string
     "\\*\\*\\(\\(.\\|\n\\)*?\\)\\*\\*"
     "<strong>\\1</strong>"
     (replace-regexp-in-string
      "\\([^:]\\)//\\(\\(.\\|\n\\)*?[^:]\\)//"
      "\\1<em>\\2</em>"
      (replace-regexp-in-string
       "^//\\(\\(.\\|\n\\)*?[^:]\\)//"
       "<em>\\1</em>"
       (replace-regexp-in-string
        "{{{\\(\\(.\\|\n\\)*?\\)}}}"
        "<code>\\1</code>"
        (replace-regexp-in-string
         "\\\\"
         "<br/>"
         text))))))))

(ert-deftest creole-block-parse ()
  "Test the block parsing routines."
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
  (should
   (equal
    (creole-block-parse "**this is bold** this is not **but this is**")
    "<strong>this is bold</strong> this is not <strong>but this is</strong>"))
  (should
   (equal
    (creole-block-parse "{{{this is code}}} and this is //italic//")
    "<code>this is code</code> and this is <em>italic</em>"))
  (should
   (equal
    (creole-block-parse
     "this is //italic// and more //italics//")
    "this is <em>italic</em> and more <em>italics</em>"))
  (should
   (equal
    (creole-block-parse
     "//test// http://server thing is //italic//")
    "<em>test</em> http://server thing is <em>italic</em>"))
  (should
   (equal
    (creole-block-parse
     "http://server //test// http://server thing is //italic//")
    "http://server <em>test</em> http://server thing is <em>italic</em>"))
  (should (equal "<a href='http://thing'>thing</a>"
                 (creole-block-parse "[[http://thing|thing]]")))
  (should (equal "<a href='thing'><strong>fing!</strong></a>"
                 (creole-block-parse "[[thing|**fing!**]]")))
  (should (equal "<a href='thing'><code>some code</code></a>"
                 (creole-block-parse "[[thing|{{{some code}}}]]")))
  (should (equal "<a href='thing'>thing</a>"
                 (creole-block-parse "[[thing]]")))
  (should (equal "<a href='http://thing'>http://thing</a>"
                 (creole-block-parse "[[http://thing]]")))
  (should (equal "<a href='http://thing'>http://thing</a>"
                 (creole-block-parse "[[http://thing]]")))
  (should (equal "<a href='thing'>thing
broken over lines</a>"
                 (creole-block-parse "[[thing|thing
broken over lines]]"))))

(defvar creole-recalculate-org-tables t
  "Indicates that Org tables should be recalculated inplace.

Table calculation is performed calling
`org-table-recalculate'. The default value is to recalculate the
tables. However, this leaves the original buffer modified. If you
don't want the original buffer modified, or you don't have
formulas in your tables (so recalculation is not necessary), you
can change this value to nil.")

(defun creole-tokenize (docbuf)
  "Parse DOCBUF which is full of creole wiki text.

See http://www.wikicreole.org/wiki/Creole1.0 for more information
on WikiCreole.

Returns a list of parsed elements."
  (with-current-buffer docbuf
    (save-excursion
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
               (if (not
                    (re-search-forward
                     "^\\(=+\\)[ \t]+\\(.*\\)[ \t]+\\(=+\\)$" nil 't))
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
            (;; Table
             (looking-at "^|")
             ;; Recalculate tables?
             (when creole-recalculate-org-tables
               ;; Requires that we're back in the table
               (org-table-recalculate t))
             (let* ((tbl (org-table-to-lisp))
                    (pt (org-table-end)))
               (setq res (append
                          res
                          (list
                           (cons 'table tbl))))
               (goto-char pt)
               ;; Skip forward over any org-tbl comments
               (unless (re-search-forward "^[^#]" nil t)
                 (goto-char (point-max)))
               (beginning-of-line)))
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
             (if (not
                  (re-search-forward "^\n{{{\n\\(\\(.\\|\n\\)*?\\)\n}}}$" nil t))
                 (error "Creole: bad preformatted block"))
             (setq res (append res
                               (list
                                (cons 'preformatted (match-string 1)))))
             (forward-line))
            (;; Lisp-plugin
             (looking-at "^\n<<($")
             (if (not
                  (re-search-forward "^\n<<(\n\\(\\(.\\|\n\\)*?\\)\n)>>$" nil t))
                 (error "Creole: bad Lisp plugin block"))
             (let* ((plugin-lisp (match-string 1))
                    (value (eval (car (read-from-string plugin-lisp))))
                    (plugin-fragment (with-temp-buffer
                                       (insert value)
                                       (creole-tokenize (current-buffer)))))
               (setq res (append res plugin-fragment)))
             (forward-line))
            (;; HTML-plugin
             (looking-at "^\n<<html$")
             (if (not
                  (re-search-forward
                   "^\n<<html\n\\(\\(.\\|\n\\)*?\\)\nhtml>>$" nil t))
                 (error "Creole: bad HTML plugin block"))
             (setq res (append res
                               (list
                                (cons 'plugin-html (match-string 1)))))
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
        res))))

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
    (insert "This is a paragraph
that runs over several lines
* and a list item stops it
")
    (insert "This is a paragraph {{{with code}}} and [[links]]
and **bold** and //italics//.")))

(ert-deftest creole-tokenize-with-table ()
  "Test a simple WikiCreole document with a table."
  (with-temp-buffer
    (insert "= Heading! =\n")
    (insert "\n")
    (insert "== Heading2! ==\n")
    (insert "| col1 | col2 |
|   15 |   20 |
|   7  |      |
#+TBLFM: @3$1=@2$1 + 20
")
    (insert "This is a paragraph
that runs over several lines
* and a list item stops it
")
    (insert "This is a paragraph {{{with code}}} and [[links]]
and **bold** and //italics//.")
    (should
     (equal
      (creole-tokenize (current-buffer))
      '((heading1 . "Heading!")
        (heading2 . "Heading2!")
        (table . (("col1" "col2") ("15" "20") ("35" "")))
        (para . "This is a paragraph\nthat runs over several lines")
        (ul1 . "and a list item stops it")
        (para . "This is a paragraph {{{with code}}} and [[links]]
and **bold** and //italics//.")))))
  (with-temp-buffer
    (insert "= Heading! =\n")
    (insert "\n")
    (insert "== Heading2! ==\n")
    (insert "| col1 | col2 |
|   15 |   20 |
|   7  |      |
#+TBLFM: @3$1=@2$1 + 20
")
    (should
     (equal
      (creole-tokenize (current-buffer))
      '((heading1 . "Heading!")
        (heading2 . "Heading2!")
        (table . (("col1" "col2") ("15" "20") ("35" ""))))))))

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

(ert-deftest creole-tokenize-lisp ()
  "Test the new embedded lisp stuff"
  (flet ((my-plugin
          () ; arg list
          "== Plugin Heading! ==
This is a paragraph {{{with code}}} and [[links]]"))
        (with-temp-buffer
          (insert "= Heading! =\n")
          (insert "\n<<(\n(my-plugin)\n)>>\n")
          (should
           (equal
            (creole-tokenize (current-buffer))
            '((heading1 . "Heading!")
              (heading2 . "Plugin Heading!")
              (para . "This is a paragraph {{{with code}}} and [[links]]")))))))

(ert-deftest creole-tokenize-plugin-html ()
  "Test the embedded HTML stuff"
  (with-temp-buffer
    (insert "= Heading! =\n")
    (insert "\n<<html\n<P>A test of HTML code</P>\nhtml>>\n")
    (insert "\n{{{\nsome code\n}}}\n")
    (should
     (equal
      (creole-tokenize (current-buffer))
      '((heading1 . "Heading!")
        (plugin-html . "<P>A test of HTML code</P>")
        (preformatted . "some code"))))))

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
         (string-to-number (match-string 2 s)))))))

(ert-deftest creole--list-item ()
  "Test the little creole list item function."
  (should (equal '(ul . 1) (creole--list-item 'ul1)))
  (should (equal '(ul . 10) (creole--list-item 'ul10)))
  (should (equal '(ul . 7) (creole--list-item 'ul7)))
  (should (equal '(ol . 7) (creole--list-item 'ol7)))
  (should (equal nil (creole--list-item 'h1))))

(defun creole-structure (lst)
  "Make a parsed structure from a list.

This is a parser, of sorts, in that it turns a list of tokens
into more of a tree structure.  In WikiCreole though, the only
thing that really needs a tree representation is ordered and
unordered lists, so all this function does is add structure to a
stream of list tokens.  All other tokens are passed through
directly.

This is not marked private because it does form part of what
might be called the parsing API of this creole library."
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
          ;; Not a list item - just push it onto the result, always
          ;; empty the list state
          (t
           (setq state '())
           (setq result (append result (list token))))))
      (setq docptr (cdr docptr)))
    result))

(ert-deftest creole-structure ()
  "Testing tokenize lists to parsed representations.

The test here takes a list that would come from `creole-tokenize'
and checks it against what should come out of `creole-structure'.
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

`creole-tokenize' is called on a buffer and checked against what
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
    (insert "This is a paragraph
that runs over several lines
* and a list item stops it
")
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

(ert-deftest creole-structure-end-to-end-with-table ()
  "Test the parser directly with the result of the tokenizer.

`creole-tokenize' is called on a buffer and checked against what
should come out."
  (with-temp-buffer
    (insert "= this is a heading! =\n")
    (insert "| col1 | col2 |
|   15 |   20 |
|   7  |      |
#+TBLFM: @3$1=@2$1 + 20
")
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
    (insert "This is a paragraph
that runs over several lines
* and a list item stops it
")
    (insert "This is a paragraph {{{with code}}} and [[links]]\n")
    (should
     (equal
      (creole-structure (creole-tokenize (current-buffer)))
      '((heading1 . "this is a heading!")
        (table . (("col1" "col2") ("15" "20") ("35" "")))
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

;; Exporting functions

(defun creole--html-list (type lst)
  "Export the specified LST in HTML.

The exported HTML is written into the current buffer.

This is NOT intended to be used by anything but
`creole-export-html'."
  (let ((first t))
    (insert "<" (symbol-name type) ">\n")
    (loop for item in lst
          do
          (cond
           ((listp item)
            (creole--html-list (car item) (cdr item))
            (setq first nil))
           (t
            (when (not first)
              (insert "</li>\n"))
            (setq first nil)
            (insert "<li>")
            (insert (creole-block-parse item)))))
    (insert "</li>\n")
    (insert "</" (symbol-name type) ">\n")))

(ert-deftest creole--html-list ()
  "Test the list export, which is a little complex."
  (with-temp-buffer
    (creole--html-list
     'ul
     '("this is a list //with an italicized part//"
       (ul "with a deeper list")
       "and another item on the end with **bold**"))
    (should
     (equal
      (buffer-substring-no-properties
       (point-min)(point-max))
      "<ul>
<li>this is a list <em>with an italicized part</em><ul>
<li>with a deeper list</li>
</ul>
</li>
<li>and another item on the end with <strong>bold</strong></li>
</ul>
"))))

(defun creole--html-table (table-list)
  "Convert the org-table structure TABLE-LIST to HTML.

We use `orgtbl-to-generic' to do this."
  (let ((value
         (orgtbl-to-generic
          table-list
          (list
           :tstart "<table>"
           :tend "</table>\n"
           :hlstart "<thead><tr>\n"
           :hlend "</tr></thead>"
           :hllstart "<thead><tr>\n"
           :hllend "</tr></thead>"
           :hfmt "<th>%s</th>\n"
           :lstart "<tr>\n"
           :lend "</tr>"
           :hline nil
           :fmt (lambda (field)
                  ;; Where we do block formatting
                  (format
                   "<td>%s</td>\n"
                   (creole-block-parse field)))
           ))))
    value))

(ert-deftest creole--html-table ()
  "Test org tables.

org-tables are not quite WikiCreole tables.  Creole table headers
are like this:

 |= header|= header|
 |cell    |cell    |

whereas we do org table headers like this:

 |header| header|
 ----------------
 |cell  |cell   |

Getting `orgtbl-to-generic' to do the WikiCreole style seems quite
difficult."
  (let ((tbl '(("col1" "col2")
               hline
               ("15" "20")
               ("35" "**end**"))))
    (should
     (equal
      (creole--html-table tbl)
      "<table>
<thead><tr>
<th>col1</th>
<th>col2</th>
</tr></thead>
<tr>
<td>15</td>
<td>20</td>
</tr>
<tr>
<td>35</td>
<td><strong>end</strong></td>
</tr>
</table>
"))))

(ert-deftest creole-html-with-links ()
  (with-temp-buffer
    (insert "= Heading! =\n")
    (insert "This is a paragraph.\n\n")
    (insert "This is a paragraph {{{with code}}} and [[links]]
and **bold** and //italics//.\n\n")
    (insert "This is a paragraph with [[http://absolute/links]]
and **bold** and //italics//.\n\n")
    (insert "This is a paragraph with [[http://absolute/links]]\n")
    (let* ((html (creole-html (current-buffer)))
           (htmlstr (with-current-buffer html
                      (buffer-substring (point-min)(point-max)))))
      (should
       (equal
        htmlstr
        "<h1>Heading!</h1>
<p>This is a paragraph.</p>
<p>This is a paragraph <code>with code</code> and <a href='links'>links</a>
and <strong>bold</strong> and <em>italics</em>.</p>
<p>This is a paragraph with <a href='http://absolute/links'>http://absolute/links</a>
and <strong>bold</strong> and <em>italics</em>.</p>
<p>This is a paragraph with <a href='http://absolute/links'>http://absolute/links</a></p>
")))))


(defun creole-htmlize-string (text)
  "Make TEXT syntax coloured HTML using Emacs font-lock.

This uses an indicated Emacs mode at the start of the text:

 ##! C
 int main(char** argv, int argc)
 {
   return 0;
 }

to make a buffer that can be syntax coloured with
`font-lock-fontify-buffer' and then calls `htmlfontify' to
generate the HTML.

A string containing the HTML is returned.

If called interactively the current region is used as the string
and the result buffer is left open and switched to.

A property `:css-list' attached to the string contains the list
of CSS declarations generated by `htmlfontify'.  The list looks
something like this:

 ((default \"default\" . \"{ ... CSS declarations ... }\")
  (font-lock-string-face \"string\" . \"{ ... CSS declarations ... }\")
  (font-lock-type-face \"type\" . \"{ ... CSS declarations ... }\")
  (font-lock-function-name-face \"function-name\" . \"{ ... CSS declarations ... }\")
  (font-lock-keyword-face \"keyword\" . \"{ ... CSS declarations ... }\")
  (font-lock-comment-face \"comment\" . \"{ ... CSS declarations ... }\")
  (whitespace-space \"whitespace-space\" . \"{ ... CSS declarations ... }\")
  (font-lock-comment-delimiter-face \"comment-delimiter\" . \"{ ... CSS declarations ... }\"))

Each element of the list contains the descriptive part of a CSS
class declaration.  To render them to a STYLE tag something like
the following Lisp is necessary:

 (mapcar
   (lambda (style)
     (format
      \"span.%s   %s\nspan.%s a %s\n\"
      (cadr style) (cddr style)
      (cadr style) (hfy-link-style (cddr style))))
   css-list)

This is from `hfy-sprintf-stylesheet' which is part of
`htmlfontify'.

Unfortunately, when run in batch mode Emacs doesn't attach colors
to faces and so we don't get coloured styles.  It should be
possible to use the `cadr' of the style to add colors."
  (interactive
   (list
    (if (mark)
        (buffer-substring
         (region-beginning)
         (region-end))
      (buffer-substring
       (point-min)
       (point-max)))))
  (let (mode-func)
    (save-match-data
      (if (when (string-match "^##! \\(.*\\)\n" text)
            (setq mode-func
                  (intern
                   (concat
                    (or
                     (match-string 1 text)
                     (downcase mode-name))
                    "-mode")))
            (functionp mode-func))
          (with-temp-buffer
            ;; Get font-lock?
            (insert text "\n")
            ;; Kill the mode variable line
            (goto-char (point-min))
            (kill-line)
            ;; Now switch that mode into the new mode
            (funcall mode-func)
            (whitespace-mode -1)
            (font-lock-fontify-buffer)
            ;; Do some dynamic binding magic to alter htmlfontify
            ;; behaviour - no header, no footer and the styles list is
            ;; captured rather than written out.
            (let (css-list)
              (flet ((hfy-sprintf-stylesheet
                      (css file)
                      (setq css-list css)
                      ""))
                (let ((hfy-display-class '((type x-toolkit)))
                      (hfy-page-footer
                       (lambda (&optional file-name)
                         "")))
                  (let (result
                        (htmlbuf
                         (flet
                             ;; htmlfontify has annoying messages in it.
                             ((message (format-str &rest args) t))
                           (htmlfontify-buffer))))
                    (with-current-buffer htmlbuf
                      ;; FIXME we should add another property
                      ;; detailing which mode we're dealing with-
                      ;;
                      ;; We MAY want to disambiguate styles, like
                      ;; "keyword" into "pre.emacs-lisp span.keyword"
                      (put-text-property
                       (point-min) (point-max)
                       :css-list css-list)
                      (setq
                       result
                        (buffer-substring
                         (point-min)
                         (point-max))))
                    (if (called-interactively-p 'interactive)
                        (switch-to-buffer htmlbuf)
                      (with-current-buffer htmlbuf
                        (set-buffer-modified-p nil))
                      (kill-buffer htmlbuf))
                    result)))))
        (concat "<pre>\n" text "\n</pre>")))))

(ert-deftest creole-htmlize-string-null ()
  "Test that a string with no markup does NOT get fontified."
  (should
   (equal
    (creole-htmlize-string "<<(
 (mapconcat
   (lambda (s)
     (format \"== %s ==\" s))
   '(\"rationale\" \"compliance\" \"tests\")
   \"\n\")
)>>")
    "<pre>
<<(
 (mapconcat
   (lambda (s)
     (format \"== %s ==\" s))
   '(\"rationale\" \"compliance\" \"tests\")
   \"\n\")
)>>
</pre>")))

(ert-deftest creole-htmlize-string ()
  "Test that we can capture 'htmlfontify' into strings."
  (let ((css-decl-list ; this is a list of the CSS declarations there should be
         '(default
            font-lock-keyword-face
            font-lock-variable-name-face
            font-lock-function-name-face
            font-lock-type-face))
        ;; The actual program text we'll fontify
        (fontified (creole-htmlize-string "##! c
int main(char **argv, int argc)
{
  return 1;
}
")))
    ;; Now the assertions
    (should
     (equal
      (substring-no-properties fontified 0 (length fontified))
      "
<pre>
<span class=\"type\">in</span><span class=\"type\">t</span> <span class=\"function-name\">main</span>(<span class=\"type\">char</span> **<span class=\"variable-name\">argv</span>, <span class=\"type\">int</span> <span class=\"variable-name\">argc</span>)
{
  <span class=\"keyword\">return</span> 1;
}

</pre>
"))
    (should
     (equal
      (loop for style-decl in (get-text-property 0 :css-list fontified)
            collect (car style-decl))
      css-decl-list))))

(defvar creole-structured '()
  "Contains the parsed creole.")

(defun* creole-html (docbuf
                     &optional html-buffer
                     &key result-mode
                     (erase-existing t)
                     do-font-lock
                     switch-to)
  "Export DOCBUF as HTML to HTML-BUFFER.

If HTML-BUFFER does not exist then a buffer is created based on
the name of DOCBUF. If DOCBUF doesn't have a name then the
destination buffer is called:

 *creolehtml.html

If RESULT-MODE is specified then the HTML-BUFFER is placed in
that mode.

If ERASE-EXISTING is not nil then any existing content in the
HTML-BUFFER is erased before rendering.  By default this is true.

If DO-FONT-LOCK is not nil then any pre-formatted areas are
examined for Emacs mode comments in the first line.  If present
then `htmlify' is run on the pre-formatted area using the Emacs
mode specified.  For example:

 {{{
 ##! emacs-lisp
 (let ((x 10))
  x)
 }}}

will `htmlify' the code excerpt as Emacs Lisp.  Note that an
HTML-PRE tag is not used for the text in that case.

If SWITCH-TO is not nil then the HTML-BUFFER is switched to when
the export is done.

When called interactively RESULT-MODE is set to 'html-mode',
ERASE-EXISTING is set to true and SWITCH-TO is set to true.

The buffer local variable `creole-structured' is set on the
HTML-BUFFER with the parsed creole in it.  See `creole-structure'
for the details of that data structure.

Returns the HTML-BUFFER."
  (interactive
   (list
    (read-buffer "Creole buffer: " (current-buffer))
    nil
    :result-mode 'html-mode
    :switch-to 't))
  (let ((result-buffer ; make up the result buffer
         (or html-buffer
             (get-buffer-create
              (replace-regexp-in-string
               "\\(\\**\\)\\(.*\\)\\(\\**\\)"
               "*creolehtml\\2.html"
               (buffer-name
                (if (bufferp docbuf)
                    docbuf
                  (get-buffer docbuf))))))))
    (make-local-variable 'creole-structured)
    (let ((creole
           (creole-structure
            (creole-tokenize docbuf))))  ; Get the parsed creole doc
      (with-current-buffer result-buffer
        (if erase-existing (erase-buffer)) ; Erase if we were asked to
        (loop for element in creole
              do
              (let ((syntax (car element)))
                (case syntax
                  ;; The list elements can follow on from each other
                  ;; and require special handling
                  ((ul ol)
                   ;; FIXME lists don't do block level replacement yet!
                   (creole--html-list syntax (cdr element)))
                  ;; Headings - FIXME - we need to change these
                  ;; obviously to something that can cope with any
                  ;; level of heading
                  (heading1
                   (insert (format "<h1>%s</h1>\n" (cdr element))))
                  (heading2
                   (insert (format "<h2>%s</h2>\n" (cdr element))))
                  (heading3
                   (insert (format "<h3>%s</h3>\n" (cdr element))))
                  (heading4
                   (insert (format "<h4>%s</h4>\n" (cdr element))))
                  (heading5
                   (insert (format "<h5>%s</h5>\n" (cdr element))))
                  ;; Tables
                  (table
                   (insert (creole--html-table (cdr element))))
                  ;; We support htmfontify for PRE blocks
                  (preformatted
                   (let ((styled (and do-font-lock
                                      (creole-htmlize-string (cdr element)))))
                     (if (not styled)
                         (insert
                          (format
                           "<pre>\n%s\n</pre>\n"
                           (cdr element)))
                       (insert styled))))
                  ;; Just embed any HTML
                  (plugin-html
                   (insert (cdr element)))
                  (hr
                   (insert "<hr/>\n"))
                  (para
                   (insert (format
                            "<p>%s</p>\n"
                            (creole-block-parse (cdr element))))))))
        (if result-mode (call-interactively result-mode))
        (setq creole-structured creole))
      (if switch-to (switch-to-buffer result-buffer))
      result-buffer)))

(ert-deftest creole-plugin-html-to-html ()
  "Test raw HTML exports correctly."
  (with-temp-buffer
    (insert "= Heading =

<<html
<P>This is a paragraph of text</P>
html>>
")
    (let ((html (creole-html (current-buffer))))
      (with-current-buffer html
        (goto-char (point-min))
        (should (looking-at "<h1>Heading</h1>
<P>This is a paragraph of text</P>"))))))

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

(ert-deftest creole-table-to-html ()
  "Test tables (which are a little complicated) export correctly."
  (with-temp-buffer
    (insert "| col1 | col2 |
|------|------|
|   15 |   20 |
|   7  |**end**|
#+TBLFM: @3$1=@2$1 + 20
")
    (let ((html (creole-html (current-buffer))))
      (with-current-buffer html
        (goto-char (point-min))
        (should (looking-at "<table>
<thead><tr>
<th>col1</th>
<th>col2</th>
</tr></thead>
<tr>
<td>15</td>
<td>20</td>
</tr>
<tr>
<td>35</td>
<td><strong>end</strong></td>
</tr>
</table>
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

(defun creole--file-under-root-p (file-name root)
  "Is FILE-NAME under the directory ROOT?

Return nil if there is no match or the part of the file-name
which was not under the docroot."
  (and root
       (file-directory-p root)
       (let* ((true-name
               (file-truename
                (expand-file-name file-name)))
              (root-dir
               (directory-file-name
                (expand-file-name root))))
         (let ((docroot-match-index
                (compare-strings
                 root-dir 0 (length root-dir)
                 true-name 0 (length true-name))))
           ;; If the compare-value is less than 0 we matched
           ;; and we have extra characters in the
           ;; true-name...  we *should* have extra
           ;; characters because otherwise we'd be referring
           ;; to the docroot.
           (when (< docroot-match-index 0)
             (substring
              true-name
              ;; -2 here because of index 0 *and* needing the
              ;; -leading slash
              (- (abs docroot-match-index) 1)
              (length true-name)))))))

(defun creole--get-file (filename)
  "An exception based FILENAME lookup.

Either loads the FILENAME in a buffer (but does not select it) or
errors 'file-error.

The FILENAME is expanded and `file-truename'd first."
  (let ((file-path
         (ignore-errors
           (file-truename (expand-file-name filename)))))
    (if (not (file-exists-p file-path))
        (signal 'file-error (format "No such file %s" file-path))
      (find-file-noselect file-path))))

(defun creole--expand-item-value (item &optional docroot)
  "Expand ITEM to be a value.

If ITEM begins with a file-name identifying character then try
and resolve the ITEM as a file-name, optionally under the
DOCROOT.

Return a cons cell with the `car' identifying the type, one of:

 :link     to indicate a linkable file-name
 :string   to indicate the raw data

and the `cdr' being the expanded string."
  (save-match-data
    (if (string-match "^\\(\\./\\|/\\|~\\).*" item)
        ;; file-name templating has been requested
        ;; Check if we have a docroot that works
        (let* ((path-info (creole--file-under-root-p item docroot)))
          (if path-info
              ;; The file is linkable so return the template with the
              ;; docroot-ed true-name
              (cons :link path-info)
            ;; No workable docroot so return either the text of the
            ;; file (if it exists) or just the filename
            (condition-case err
                (with-current-buffer (creole--get-file item)
                  (cons :string
                        (buffer-substring
                         (point-min)
                         (point-max))))
              ;; FIXME - I'd like this to be file-error - why doesn't
              ;; that work???
              (error (cons :link item)))))
      ;; The item was not a file-name so just return it
      (cons :string item))))

(ert-deftest creole--expand-item-value-mocked-file ()
  "Test that we can mock the file loading."
  (with-temp-buffer
    (insert "= A very small creole file =\n")
    (let ((file-buffer (current-buffer)))
      ;; Mock both these functions to cause the buffer 'file-buffer'
      ;; to be returned from creole--get-file
      (flet ((creole--file-under-root-p
              (file-name root)
              nil)
             (creole--get-file
              (filename)
              file-buffer))
        (should (equal
                 (cons :string "= A very small creole file =\n")
                 (creole--expand-item-value
                  "~/elwikicreole/README.creole"
                  "~/elwikicreole/")))))))

(ert-deftest creole--expand-item-value-plain-string ()
  ;; Should just be the value of the string
  (should (equal
           (cons :string "just a string")
           (creole--expand-item-value
            "just a string"
            "~/elwikicreole/"))))

(ert-deftest creole--expand-item-value-safe-file ()
  "Test that a file under the docroot is returned as just a file."
  (flet ((creole--file-under-root-p
          (file-name root)
          ;; TODO - implementation
          "/README.creole"))
        (should (equal
                 (cons :link "/README.creole")
                 (creole--expand-item-value
                  "~/elwikicreole/README.creole"
                  "~/elwikicreole/")))))

(ert-deftest creole--expand-item-value-null-file ()
  ;; Should be an empty :link
  (flet ((creole--file-under-root-p
          (file-name root)
          ;; TODO - implementation
          nil))
        (should (equal
                 (cons :link "/__not__there__.creole")
                 (creole--expand-item-value
                  "/__not__there__.creole"
                  "~/elwikicreole/")))))

(ert-deftest creole--expand-item-value-unsafe-file ()
  ;; Supply a filename but get back the expanded string
  ;; because the filename is not under the docroot
  (let ((TEST-FILE-NAME "/home/nferrier/wiki/small.creole")
        (TMPBUF
         (get-buffer-create
          (generate-new-buffer-name
           " *creole-expand-item-value-unsafe-file*"))))
    (with-current-buffer TMPBUF
      (insert "= A very small Creole document =\n"))
    (flet
        ((creole--file-under-root-p
          (file-name root)
          nil)
         (file-truename
          (file-name)
          TEST-FILE-NAME)
         (file-exists-p
          (file-name)
          (equal file-name TEST-FILE-NAME))
         (find-file-noselect
          (file-name)
          TMPBUF))
      (should
       (equal
        (cons :string "= A very small Creole document =\n")
        (creole--expand-item-value
         "~/elwikiengine/wiki/small.creole"
         "~/elwikicreole/"))))
    (kill-buffer TMPBUF)))

(defun creole--wrap-buffer-text (start end &optional buffer)
  "Simply wrap the text of BUFFER (or the current buffer).

START is placed at the start of the BUFFER and END is placed at
the end of the BUFFER."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (insert start)
        (goto-char (point-max))
        (insert end)))))

(defun creole--insert-template (key
                                position
                                docroot
                                link-template
                                embed-template
                                &optional docroot-alias)
  "Insert either the LINK-TEMPLATE or the EMBED-TEMPLATE.

KEY specifies a value that is expanded with
`creole--expand-item-value', possibly with DOCROOT.

Whether we're a :link or a :string will cause either the
LINK-TEMPLATE or the EMBED-TEMPLATE to be inserted at the marker
POSITION.

If DOCROOT-ALIAS is specified and the :link template is used then
the filename is concatenated with that."
  (save-excursion
    (when key
      (goto-char position)
      (let ((value (creole--expand-item-value key docroot)))
        (case (car value)
          (:link
           (insert
            (format
             link-template
             (if docroot-alias
                 (concat docroot-alias (cdr value))
                 (cdr value)))))
          (:string
           (insert
            (format embed-template (cdr value)))))))))

(defcustom creole-css-color-type "#000000"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-default "#000000"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-whitespace-empty "#b22222"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-regexp-grouping-construct "#000000"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-builtin "#483d8b"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-function-name "#0000ff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-doc "#8b2252"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-string "#8b2252"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-variable-name "#a0522d"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-constant "#008b8b"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-keyword "#a020f0"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-comment "#b22222"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-whitespace-space "#d3d3d3"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-color-comment-delimiter "#b22222"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-default "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-whitespace-empty "#ffff00"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-regexp-grouping-construct "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-regexp-grouping-backslash "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-builtin "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-function-name "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-doc "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-string "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-variable-name "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-constant "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-keyword "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-comment "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-whitespace-space "#ffffe0"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))

(defcustom creole-css-background-comment-delimiter "#ffffff"
  "A custom color to be used for CSS style rendering."
  :group 'creole
  :type '(string))


(defun creole--css-list-to-style-decl (css-list)
  "Make the CSS-LIST into an HTML STYLE decl."
  (mapconcat
   (lambda (style)
     (format
      "span.%s   %s\nspan.%s a %s\n%s\n"
      (cadr style) (cddr style)
      (cadr style) (hfy-link-style (cddr style))
      ;; Add in our own colors - just add nothing
      ;; if we don't have customization for it
      (condition-case err
          (let ((css-value
                 (symbol-value
                  (intern
                   (concat
                    "creole-css-color-"
                    (cadr style))))))
            (if css-value
                (format
                 "span.%s { color: %s; }\n"
                 (cadr style)
                 css-value)))
        (void-variable ""))))
   css-list
   "\n"))

(ert-deftest creole-htmlize-css-lists ()
  "Test that we can capture 'htmlfontify' css lists."
  (let* ((css-decl-list ; this is a list of the CSS declarations there should be
          '(default
            font-lock-keyword-face
            font-lock-variable-name-face
            font-lock-function-name-face
            font-lock-type-face))
         ;; The actual program text we'll fontify
         (fontified (creole-htmlize-string "##! c
int main(char **argv, int argc)
{
  return 1;
}
"))
         (style-decl
          (creole--css-list-to-style-decl
           (get-text-property 0 :css-list fontified))))
    (should (string-match "^span.keyword" style-decl))
    (should (string-match "^span.default" style-decl))
    (should (string-match "^span.variable-name" style-decl))
    (should (string-match "^span.function-name" style-decl))
    (should (string-match "^span.type" style-decl))))

(defun creole-moustache (template variables)
  "Moustache replace in TEMPLATE with VARIABLES.

Eg:

  (creole-moustache
    \"<textarea>{{text}}</textarea>\"
    '((text . \"this is my text\")))

  =>  \"<textarea>this is my text</textarea>\""
  (replace-regexp-in-string
   "{{\\([A-Za-z0-9_-]+\\)}}"
   (lambda (m)
     (let* ((expansion (match-string 1 m))
            (var (intern expansion))
            (pair (assoc var variables)))
       (if pair
           (cdr pair)
         (concat "{{" expansion "}}"))))
   template
   nil
   t))

(ert-deftest creole-moustache ()
  "Test the moustache templating."
  (should
   (equal
    "<<this is a test>>"
    (creole-moustache
     "<<{{text}}>>"
     '((text . "this is a test")))))
  (should
   (equal
    "<<this is a test>>[[{{with-a-spare-var}}]]"
    (creole-moustache
     "<<{{text}}>>[[{{with-a-spare-var}}]]"
     '((text . "this is a test")))))
  (should
   (equal
    "<<this is a test>>[[another test]]"
    (creole-moustache
     "<<{{text}}>>[[{{working-var}}]]"
     '((text . "this is a test")
       (working-var . "another test"))))))


(defun creole-list-text-properties (buffer property predicate)
  "List all the values for PROPERTY in BUFFER.

PREDICATE is used to merge the properties."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((lst (list))
             (p (next-single-property-change
                 (point-min)
                 :css-list
                 (current-buffer)
                 (point-max))))
        (while (not (equal p (point-max)))
          (let ((prop (get-text-property p property)))
            (when prop
              (setq lst
                    (merge
                     'list
                     lst prop
                     predicate))))
            (goto-char (+ 1 p))
            (setq p (next-single-property-change
                     (point)
                     property
                     (current-buffer)
                     (point-max))))
          lst))))

;;;###autoload
(defun* creole-wiki (source
                     &key
                     destination
                     (htmlfontify t)
                     (htmlfontify-style t)
                     body-header
                     body-footer
                     variables
                     docroot
                     docroot-alias
                     css
                     javascript
                     meta
                     other-link)
  "Export WikiCreole SOURCE into HTML.

Returns the buffer where the HTML was exported. This could be a
user supplied buffer (see DESTINATION) or a buffer created based
on the filename of the source (or just automatically created).

SOURCE can be a buffer or plain text or something we might
recognize as a file.  A file-name is detected by a leading
'~' (meaning expand from the user root) or '/' (meaning rooted)
or './' (meaning expand from the root of the source creole file).

If SOURCE is a filename it is loaded with `creole--get-file'.


Keyword arguments are supported to change the way the HTML is
produced.

DESTINATION can be a buffer or a buffer name to write the HTML
into or it can be 't' to indicate the default output stream.  In
the latter case an automatic buffer is still created and the HTML
is sent to the default output stream when the export is done.

The DESTINATION buffer is always returned.


HTMLFONTIFY - use 'htmlfontify' to fontify any code blocks; this
is true by default.

Code blocks are marked up like pre-formatted areas but must begin
with a line stating the Emacs mode to fontify the text as; for
example:

 {{{
 ##! emacs-lisp
 (let ((x 1)) x)
 }}}

would cause Emacs Lisp to be fontified.

HTMLFONTIFY-STYLE - add an HTML-STYLE block for 'htmlfontify'
code blocks. If this is nil an HTML-STYLE block is NOT added.

BODY-HEADER - a string or a file-name with HTML code to be
inserted in the BODY of the HTML document before the Creole
markup export.  A file-name is detected in the same way as for
SOURCE.

BODY-FOOTER - a string or a file-name with HTML code to be
inserted in the BODY of the HTML document after the Creole markup
export.  A file-name is detected in the same way as for SOURCE.

The BODY-HEADER and the BODY-FOOTER are treated as moustache
templates and expanded before being inserted.  See
'creole-moustache' for a description.  Variables passed to
'creole-moustache' with the template are:

  text - the creole source text of the page

or any variable in VARIABLES, which is an alist of
symbols -> values.

DOCROOT - base any files to be served.  Any file-name reference
for CSS or JavaScript, if residing under this docroot, will be
linked to the document rather than embedded.

DOCROOT-ALIAS - is the docroot path to use in any links as an
alias for the docroot.

CSS - a list of cascading style sheets, each entry can either be
a file-name (a file-name is detected in the same way as
for SOURCE) or a string with W3C-CSS statements in it.

If a DOCROOT is specified then any cascading style sheets
file-name is LINKed into the resulting document, if not then the
statements are embedded directly.

JAVASCRIPT - a list of JavaScript, as for CSS, each entry can
be either a string of the JavaScript to be directly embedded or a
file-name reference (as in SOURCE).  As for :CSS if
a :DOCROOT is specified then the scripts will be loaded as links
but otherwise will be embedded.

META - a list of strings specifying resulting HTML-META elements.
For example:

 :meta '(\"name='description'
          content='Free Web tutorials on HTML, CSS, XML'\")

:OTHER-LINK - a list of string specifying resulting HTML-LINK
elements, for example:

 :other-link '(\"rel='alternate' href='/my-feed.rss'\")

All, any or none of these keys may be specified.
"
  (interactive "fCreole file: ")
  (let* (file-opened ;; a flag to indicate whether we opened a file or not
         (source-buffer
          ;; Detect what sort of source we have
          (cond
           ((bufferp source)
            source)
           ((string-match "^\\(./\\|/\\|~\\).*" source)
            (creole--get-file source))
           (t
            (with-current-buffer (generate-new-buffer "* creole-source *")
              (insert source)
              (current-buffer)))))
         (html-buffer
          (cond
           ((bufferp destination)
            destination)
           ((stringp destination)
            (get-buffer-create destination))
           (t
            (get-buffer-create "*creole-html*")))))

    ;; Export the creole to the result buffer
    (creole-html source-buffer html-buffer :do-font-lock htmlfontify)

    ;; Now a bunch of other transformations on the result buffer
    (with-current-buffer html-buffer
      (let* ((creole-text
              (with-current-buffer source-buffer
                (buffer-substring (point-min)(point-max))))
             ;; We should let users specify more variables in the
             ;; call to creole-wiki?
             (vars (append `((text . ,creole-text)) variables)))

        ;; Insert the BODY header and footer
        (when body-header
          (let ((hdr (creole--expand-item-value body-header)))
            (when (eq (car hdr) :string)
              (goto-char (point-min))
              (insert
               (creole-moustache
                (cdr hdr)
                vars)))))

        (when body-footer
          (let ((ftr (creole--expand-item-value body-footer)))
            (when (eq (car ftr) :string)
               (goto-char (point-max))
               (insert
                (creole-moustache
                 (cdr ftr)
                 vars)))))

        ;; Now wrap everything we have so far with the BODY tag
        (creole--wrap-buffer-text "<body>\n" "</body>\n")

        ;; Now stuff that should go in a header
        (when (or css javascript meta other-link
                  (and htmlfontify
                       htmlfontify-style
                       (next-single-property-change
                        (point-min)
                        :css-list
                        (current-buffer)
                        (point-max))))
          (let (head-marker)
            (goto-char (point-min))
            (insert "<head>\n")
            (let ((creole-doc-title (assoc 'heading1 creole-structured)))
              (when creole-doc-title
                (insert (format "<title>%s</title>\n" (cdr creole-doc-title)))))
            (setq head-marker (point-marker))
            (insert "</head>\n")
            ;; First the CSS
            (loop for ss in css
               do (creole--insert-template
                   ss
                   head-marker
                   docroot
                   "<link rel='stylesheet' href='%s' type='text/css'/>\n"
                   "<style>\n%s\n</style>\n"
                   docroot-alias))
            ;; Now the JS
            (loop for js in javascript
               do (creole--insert-template
                   javascript
                   head-marker
                   docroot
                   "<script src='%s' language='Javascript'></script>\n"
                   "<script>
//<!--
%s
//-->
</script>
"
                   docroot-alias))
            ;; Now meta
            (creole--insert-template
             meta
             head-marker
             docroot
             "<meta %s/>\n"
             "<meta %s/>\n")
            (creole--insert-template
             other-link
             head-marker
             docroot
             "<link %s/>\n"
             "<link %s/>\n")

            ;; Find any styles that are embedded
            (if (and htmlfontify htmlfontify-style)
                (let ((css (remove-duplicates
                            (creole-list-text-properties
                             (current-buffer)
                             :css-list
                             (lambda (a b) (string< (cadr a) (cadr b))))
                            :test (lambda (a b) (string= (cadr a) (cadr b))))))
                    (save-excursion
                      (goto-char head-marker)
                      (insert
                       "<style>\n"
                       (creole--css-list-to-style-decl css)
                       "\n</style>\n"))))))

        ;; Wrap the whole thing in the HTML tag
        (creole--wrap-buffer-text "<html>\n" "</html>\n")))

    ;; Should we output the whole thing to the default output stream?
    (when (eq destination t)
      (with-current-buffer html-buffer
        (princ (buffer-substring (point-min)(point-max)))))

    (when (called-interactively-p 'any)
      (switch-to-buffer html-buffer))

    (when file-opened
      (kill-buffer source-buffer))

    ;; Return the destination buffer
    html-buffer))

(ert-deftest creole-wiki-file ()
  "Test that a mocked file can be loaded as a Wiki file."
  (with-temp-buffer
    (insert "= A Creole Wiki file =

This is a nice simple Creole Wiki file.
")
    (let ((creole-file-buffer (current-buffer)))
      (flet ((creole--get-file
              (filename)
              creole-file-buffer))
        (with-temp-buffer
          (creole-wiki
           "~/anyfilename"
           :destination (current-buffer)
           :htmlfontify-style nil)
          (should
           (equal
            (buffer-substring-no-properties (point-min) (point-max))
            "<html>
<body>
<h1>A Creole Wiki file</h1>
<p>This is a nice simple Creole Wiki file.</p>
</body>
</html>
")))))))

(defmacro creole--wiki-test (creoletext htmltext &rest extras)
  "A helper macro for testing full HTML conversion.

CREOLETEXT is the creoletext to test.

HTMLTEXT is what it should produce.

EXTRAS are any extra controls to be added to the 'creole-to-html'
call.

HTMLFONTIFY-STYLE is turned off for all these tests because it's
too difficult to test strings of 'htmlfontify' styles.  They are
too dependent on the particular environment (fonts etc...)."
  (declare (indent defun))
  `(with-temp-buffer
     (let ((html-buffer (current-buffer)))
       (with-temp-buffer
         ;; Setup the buffer
         (insert ,creoletext)
         ;; Now convert
         (creole-wiki
          (current-buffer)
          :destination html-buffer
          ;; Turn off style header
          :htmlfontify-style nil
          ;; Include any further parameters
          ,@extras)
         ;; Now test the resulting HTML
         (should
          (equal
           (with-current-buffer html-buffer
             (buffer-substring-no-properties (point-min)(point-max)))
           ,htmltext))))))

(ert-deftest creole-wiki-fontify ()
  ;; Font lock testing
  (creole--wiki-test
    "= A Creole Document =

{{{
##! c
int main(char **argv, int argc)
{
  return 1;
}
}}}

A preformatted area with styling.
" "<html>
<body>
<h1>A Creole Document</h1>

<pre>
<span class=\"type\">in</span><span class=\"type\">t</span> <span class=\"function-name\">main</span>(<span class=\"type\">char</span> **<span class=\"variable-name\">argv</span>, <span class=\"type\">int</span> <span class=\"variable-name\">argc</span>)
{
  <span class=\"keyword\">return</span> 1;
}
</pre>
<p>A preformatted area with styling.</p>
</body>
</html>
" :htmlfontify t))


(ert-deftest creole-wiki-base ()
  "Test the comprehensive HTML production."
  (creole--wiki-test
    "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<body>
<h1>A Creole Document</h1>
<p>This is a Creole document with some stuff in it.</p>
</body>
</html>
"))

(ert-deftest creole-wiki-headers-footers ()
  "Test that specified headers and footers come out correctly."
  (creole--wiki-test
    "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<body>
<div id='header'>the header</div>
<h1>A Creole Document</h1>
<p>This is a Creole document with some stuff in it.</p>
<div id='footer'>the footer</div>
</body>
</html>
"
    :body-header "<div id='header'>the header</div>\n"
    :body-footer "<div id='footer'>the footer</div>\n"))

(ert-deftest creole-wiki-css-link ()
  "Test that a CSS under the docroot is linked not embedded."
  (flet ((creole--file-under-root-p
          (file-name root)
          "/styles.css"))
    (creole--wiki-test
      "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<head>
<title>A Creole Document</title>
<link rel='stylesheet' href='/styles.css' type='text/css'/>
</head>
<body>
<h1>A Creole Document</h1>
<p>This is a Creole document with some stuff in it.</p>
</body>
</html>
"
    :docroot "~/elwikicreole/"
    :css "~/elwikicreole/styles.css")))

(ert-deftest creole-wiki-css-embed ()
  "Test that strings are embedded for CSS when necessary."
  (flet ((creole--file-under-root-p
          (file-name root)
          "/styles.css"))
  ;; Test a string specified as the CSS is embedded
  (creole--wiki-test
    "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<head>
<title>A Creole Document</title>
<style>
p {
font-size: 8pt;
}
</style>
</head>
<body>
<h1>A Creole Document</h1>
<p>This is a Creole document with some stuff in it.</p>
</body>
</html>
"
    ;; Here's the docroot
    :docroot "~/elwikicreole/"
    ;; Here's a string specifying some CSS, clearly not a file
    :css "p {
font-size: 8pt;
}"))

  ;; This version tests the full mocking causing embedding
  (with-temp-buffer
    (insert "P { background: blue; }")
    (let ((css-file-buffer (current-buffer)))
      ;; Mock the 2 functions so that the file is not considered under
      ;; the docroot and so that it's contents if the CSS fragment
      ;; above
      (flet ((creole--file-under-root-p
              (file-name root)
              nil)
             (creole--get-file
              (filename)
              css-file-buffer))
        (creole--wiki-test
          "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<head>
<title>A Creole Document</title>
<style>
P { background: blue; }
</style>
</head>
<body>
<h1>A Creole Document</h1>
<p>This is a Creole document with some stuff in it.</p>
</body>
</html>
"
    ;; Here's the docroot...
    :docroot "~/elwikicreole/"
    ;; ... here's a file clearly not under the docroot, so it should
    ;; be embedded.
    :css "~/someplace/styles.css")))))

(ert-deftest creole-wiki-js ()
  (flet ((creole--file-under-root-p
          (file-name root)
          "/scripts.js"))
    (creole--wiki-test
      "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<head>
<title>A Creole Document</title>
<script src='/scripts.js' language='Javascript'></script>
</head>
<body>
<h1>A Creole Document</h1>
<p>This is a Creole document with some stuff in it.</p>
</body>
</html>
"
    :docroot "~/elwikicreole/"
    :javascript "~/elwikicreole/scripts.js"))
  (creole--wiki-test
    "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<head>
<title>A Creole Document</title>
<script>
//<!--
$(document).ready(function () {
doSomething();
});

//-->
</script>
</head>
<body>
<h1>A Creole Document</h1>
<p>This is a Creole document with some stuff in it.</p>
</body>
</html>
"
    :docroot "~/elwikicreole/"
    :javascript "$(document).ready(function () {
doSomething();
});
"))


;; Useful functions

(defun creole-directory-list (directory-name &optional make-links)
  "WikiCreole format a table of files in DIRECTORY-NAME.

MAKE-LINKS causes the files to be WikiCreole links."
  (loop for filename in (directory-files directory-name)
        if (not (or (equal filename ".")
                    (equal filename "..")))
        concat
        (let* ((fq (expand-file-name filename directory-name))
               (fa (file-attributes fq))
               (timestr
                (apply 'format
                       "%04d-%02d-%02d %02d:%02d"
                       (let ((dt (decode-time (elt fa 5))))
                         (list (elt dt 5)
                               (elt dt 4)
                               (elt dt 3)
                               (elt dt 2)
                               (elt dt 1))))))
          (format
           "|%s|%s|%s|\n"
           (if make-links
               (format "[[%s]]" filename)
             filename)
           timestr
           (elt fa 7)))))

(ert-deftest creole-directory-list ()
  (flet ((directory-files
          (directory-name)
          '("." ".." "file1.html" "file2.html"))
         (file-attributes
          (file-name)
          (let ((t1 (encode-time 0 32 9 1 12 2011))
                (t2 (encode-time 0 22 9 1 12 2011)))
            (cond
             ((string-match ".*/file1.html" file-name)
              `(nil 1 "uid" "grp"
                    ,t1 ,t1 ,t1
                    200 "-rwxrwxrwx-" t
                    1333331114234 0))
             ((string-match ".*/file2.html" file-name)
              `(nil 1 "uid" "grp"
                    ,t2 ,t2 ,t2
                    157 "-rwxrwxrwx-" t
                    1333331114234 0))))))
    (should
     (equal
      (creole-directory-list "~/mydirectory")
      "|file1.html|2011-12-01 09:32|200|
|file2.html|2011-12-01 09:22|157|
"))
    (should
     (equal
      (creole-directory-list "~/mydirectory" t)
      "|[[file1.html]]|2011-12-01 09:32|200|
|[[file2.html]]|2011-12-01 09:22|157|
"))))


(provide 'creole)

;;; creole.el ends here
