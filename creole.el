;;; creole.el --- A parser for the Creole Wiki language

;;; Copyright (C) 2011 by Nic Ferrier

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


(defun creole-parse (docbuf)
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
          (let ((start (point))
                (end 
                 (save-match-data
                   (let ((matched-end (re-search-forward "^\\(\\($\\)\\|\\([=*]\\)\\)" nil 't)))
                     (if matched-end
                         (- matched-end 2)
                       (point-max))))))
            (setq res (append res
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


(ert-deftest creole-parse ()
  (with-temp-buffer
    (creole--test-doc (current-buffer))
    (should 
     (equal 
      (creole-parse (current-buffer))
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


(defun creole-export-html (docbuf)
  "Export DOCBUF as HTML."
  (let ((creole (creole-parse docbuf)))
    (with-current-buffer (get-buffer-create "*creolehtml*")
      (erase-buffer)
      (loop for element in creole
            do 
            (case (car element)
              ('heading1
               (insert (format "<h1>%s</h1>\n" (cdr element))))
              ('heading2
               (insert (format "<h2>%s</h2>\n" (cdr element))))
              ('heading3
               (insert (format "<h3>%s</h3>\n" (cdr element))))
              ('ul1
               (insert (format "<ul>\n<li>%s</li>\n</ul>\n" 
                               (creole-block-parse (cdr element)))))
              ('ul2
               (insert (format "<ul>\n<li>%s</li>\n</ul>\n" 
                               (creole-block-parse (cdr element)))))
              ('ul3
               (insert (format "<ul>\n<li>%s</li>\n</ul>\n"
                               (creole-block-parse (cdr element)))))
              ('ol1
               (insert (format "<ol>\n<li>%s</li>\n</ol>\n" 
                               (creole-block-parse (cdr element)))))
              ('ol2
               (insert (format "<ol>\n<li>%s</li>\n</ol>\n"
                               (creole-block-parse (cdr element)))))
              ('ol3
               (insert (format "<ol>\n<li>%s</li>\n</ol>\n"
                               (creole-block-parse (cdr element)))))
              ('preformatted
               (insert (format "<pre>\n%s\n</pre>\n" (cdr element))))
              ('hr
               (insert "<hr/>\n"))
              ('para
               (insert (format "<p>%s</p>\n"
                               (creole-block-parse (cdr element)))))))
      (current-buffer))))


(defun creole-to-html (docbuf)
  "Export the specified DOCBUF as HTML.

DOCBUF can be specified."
  (interactive "bBuffer: ")
  (switch-to-buffer (creole-export-html docbuf)))

(ert-deftest creole-export ()
  (with-temp-buffer
    (creole--test-doc (current-buffer))
    (let ((html (creole-export-html (current-buffer))))
      (with-current-buffer html
        (goto-char (point-min))
        (should (looking-at "<h1>Heading!</h1>
<h2>Heading2!</h2>
<ol>
<li>an ordered list item</li>
</ol>
<ol>
<li>a 2nd ordered list item</li>
</ol>
<h2>Heading3 is a multi word heading</h2>
<pre>
== this is preformatted ==
{{
It looks great
}}
</pre>
<ul>
<li>list item</li>
</ul>
<ul>
<li>2nd list item</li>
</ul>
<ul>
<li>3rd list item</li>
</ul>
<ul>
<li>another 2nd list item</li>
</ul>
<ul>
<li>another 3rd list item</li>
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
