;; test creole stuff

(require 'creole)
(require 'ert)
(require 'cl)

(ert-deftest creole/link-resolve ()
  "Test simple resolution."
  (should
   (equal
    "test.creole"
    (flet ((directory-files (dir &optional full match nosort)
             (list "test.creole")))
      (creole/link-resolve "test"))))
  (should
   (equal
    "test"
    (flet ((directory-files (dir &optional full match nosort)
             (list)))
      (creole/link-resolve "test")))))

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

(ert-deftest creole-link-parse-camel ()
  (let (creole-link-resolver-fn)
    (should (equal (creole-link-parse "ThisThing")
                   "ThisThing")))
  (let ((creole-link-resolver-fn 'creole/link-resolve)
        (link-thing "ThisThing.creole"))
    (flet ((directory-files (dir &optional full match nosort)
             (list link-thing)))
      (should (equal (creole-link-parse "ThisThing")
                     "<a href='ThisThing.creole'>ThisThing</a>"))
      ;; make sure we do NOT escape if creole-oddmuse-on is nil
      (should
       (equal
          (creole-link-parse "!OtherLongerThing")
          "<a href='ThisThing.creole'>!OtherLongerThing</a>"))
      (let ((link-thing "OtherLongerThing.creole"))
        (should
         (equal
          (creole-link-parse "OtherLongerThing")
          "<a href='OtherLongerThing.creole'>OtherLongerThing</a>"))
        ;; Without oddmuse we don't match []
        (should
         (equal
          (creole-link-parse "[OtherLongerThing]")
          "[OtherLongerThing]"))
        (let ((creole-oddmuse-on t))
          (should
           (equal
            (creole-link-parse "[OtherLongerThing]")
            "<a href='OtherLongerThing.creole'>OtherLongerThing</a>"))
          (should
           (equal
            (creole-link-parse "!UnescapedThing")
            "UnescapedThing")))))))

(ert-deftest creole-link-parse-resolver ()
  (let ((creole-link-resolver-fn 'creole/link-resolve))
    (flet ((directory-files (dir &optional full match nosort)
             (list "thing.creole")))
      (should
       (equal "<a href='thing.creole'>thing</a>"
              (creole-link-parse "[[thing]]"))))
    (flet ((directory-files (dir &optional full match nosort)
             (list "ThisThing.creole")))
      (should
       (equal "<a href='ThisThing.creole'>ThisThing</a>"
              (creole-link-parse "ThisThing"))))
    (flet ((directory-files (dir &optional full match nosort)
             (list "ThisThing.creole")))
      (should
       (equal "<a href='http://blah/blah'>blah</a>"
              (creole-link-parse "[[http://blah/blah|blah]]"))))
    (flet ((directory-files (dir &optional full match nosort)
             (list "ThisThing.creole")))
      (should
       (equal "<a href='ThisThing.creole'>blah</a>"
              (creole-link-parse "[[ThisThing|blah]]"))))))


(ert-deftest creole-link-parse-oddmuse ()
  (let ((creole-oddmuse-on t))
    (should (equal "<a href='http://thing'>thing</a>"
                   (creole-link-parse "[[http://thing|thing]]")))
    (should (equal "<a href='http://thing'>thing</a>"
                   (creole-link-parse "[http://thing|thing]")))
    (should (equal "<a href='http://thing'>blah</a>"
                   (creole-link-parse "[http://thing blah]")))
    (should (equal "<a href='thing'>fing!</a>"
                   (creole-link-parse "[[thing|fing!]]")))
    (should (equal "<a href='thing'>thing</a>"
                   (creole-link-parse "[[thing]]")))
    (should (equal "<a href='thing'>thing
broken over lines</a>"
                   (creole-link-parse "[[thing|thing
broken over lines]]")))))

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
                 (creole-image-parse "{{image.jpg?size=20x10}}")))
  (let ((creole-link-resolver-fn
         (lambda (name)
           (concat name ".jpg"))))
    (flet ((directory-files (dir &optional full match nosort)
             (list "thing.jpg")))
      (should
       (equal (creole-image-parse "{{thing}}")
              "<img src='thing.jpg' alt='thing' />")))))

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
    (creole/test-doc (current-buffer))
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

(ert-deftest creole/list-item ()
  "Test the little creole list item function."
  (should (equal '(ul . 1) (creole/list-item 'ul1)))
  (should (equal '(ul . 10) (creole/list-item 'ul10)))
  (should (equal '(ul . 7) (creole/list-item 'ul7)))
  (should (equal '(ol . 7) (creole/list-item 'ol7)))
  (should (equal nil (creole/list-item 'h1))))

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

(ert-deftest creole/html-list ()
  "Test the list export, which is a little complex."
  (with-temp-buffer
    (creole/html-list
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

(ert-deftest creole/html-table ()
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
      (creole/html-table tbl)
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

(ert-deftest creole/heading-test>id ()
  "Test that we convert heading text to HTML IDs."
  (should (equal
           "this-is-it-then"
           (creole/heading-text->id "this is it then")))
  (should (equal
           "This-is-it-then"
           (creole/heading-text->id "This is it then"))))

(ert-deftest creole/heading->html ()
  "Test the heading production."
  (should
   (equal
    (let (creole-oddmuse-on creole-do-anchor-headings)
      (creole/heading->html
       '(heading1 "this is a test")))
    "<h1>this is a test</h1>\n"))
  ;; Now test with automatic anchor links
  (let ((creole-oddmuse-on t))
    (should
     (equal
      (creole/heading->html
       '(heading1 "this is a test"))
      "<a id='this-is-a-test'></a>\n<h1>this is a test</h1>\n")))
  (let ((creole-oddmuse-on nil)
        (creole-do-anchor-headings t))
    (should
     (equal
      (creole/heading->html
       '(heading1 "this is a test"))
      "<a id='this-is-a-test'></a>\n<h1>this is a test</h1>\n"))))

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
        "<a id='Heading!'></a>
<h1>Heading!</h1>
<p>This is a paragraph.</p>
<p>This is a paragraph <code>with code</code> and <a href='links'>links</a>
and <strong>bold</strong> and <em>italics</em>.</p>
<p>This is a paragraph with <a href='http://absolute/links'>http://absolute/links</a>
and <strong>bold</strong> and <em>italics</em>.</p>
<p>This is a paragraph with <a href='http://absolute/links'>http://absolute/links</a></p>
")))))

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
        (should (looking-at "<a id='Heading'></a>
<h1>Heading</h1>
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

(ert-deftest creole/structure-pipeline ()
  "Test the pipelining."
  (should ;; test with empty pipeline
   (equal
    '((heading1 "hello")
      (ul "a tokenizer"
       (ul "Inline markup inside a paragraph is NOT converted.")
       "a //parser//"))
    (creole/structure-pipeline
     (list)
     '((heading1 "hello")
       (ul "a tokenizer"
        (ul "Inline markup inside a paragraph is NOT converted.")
        "a //parser//")))))
  (should ;; test with a single stage in the pipeline
   (equal
    '((heading1 "hello")
      (ul "a tokenizer"
       (ul "Inline markup inside a paragraph is NOT converted.")
       "a //parser//")
      (para "end of the document"))
    (creole/structure-pipeline
     (list (lambda (structure)
             (append
              structure
              (list (list 'para "end of the document")))))
     '((heading1 "hello")
       (ul "a tokenizer"
        (ul "Inline markup inside a paragraph is NOT converted.")
        "a //parser//"))))))

(ert-deftest creole-html ()
  "Test the HTML export end to end."
  (with-temp-buffer
    (creole/test-doc (current-buffer))
    (let ((html (creole-html (current-buffer))))
      (should 
       (equal
        (with-current-buffer html (buffer-string))
        "<a id='Heading!'></a>
<h1>Heading!</h1>
<a id='Heading2!'></a>
<h2>Heading2!</h2>
<ol>
<li>an ordered list item<ol>
<li>a 2nd ordered list item</li>
</ol>
</li>
</ol>
<a id='Heading3-is-a-multi-word-heading'></a>
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
")))))


(ert-deftest creole/expand-item-value-mocked-file ()
  "Test that we can mock the file loading."
  (with-temp-buffer
    (insert "= A very small creole file =\n")
    (let ((file-buffer (current-buffer)))
      ;; Mock both these functions to cause the buffer 'file-buffer'
      ;; to be returned from creole/get-file
      (flet ((creole/file-under-root-p
                 (file-name root)
               nil)
             (creole/get-file
                 (filename)
               file-buffer))
        (should (equal
                 (cons :string "= A very small creole file =\n")
                 (creole/expand-item-value
                  "~/elwikicreole/README.creole"
                  "~/elwikicreole/")))))))

(ert-deftest creole/expand-item-value-plain-string ()
  ;; Should just be the value of the string
  (should (equal
           (cons :string "just a string")
           (creole/expand-item-value
            "just a string"
            "~/elwikicreole/"))))

(ert-deftest creole/expand-item-value-safe-file ()
  "Test that a file under the docroot is returned as just a file."
  (flet ((creole/file-under-root-p
          (file-name root)
          ;; TODO - implementation
          "/README.creole"))
        (should (equal
                 (cons :link "/README.creole")
                 (creole/expand-item-value
                  "~/elwikicreole/README.creole"
                  "~/elwikicreole/")))))

(ert-deftest creole/expand-item-value-null-file ()
  ;; Should be an empty :link
  (flet ((creole/file-under-root-p
          (file-name root)
          ;; TODO - implementation
          nil))
        (should (equal
                 (cons :link "/__not__there__.creole")
                 (creole/expand-item-value
                  "/__not__there__.creole"
                  "~/elwikicreole/")))))

(ert-deftest creole/expand-item-value-unsafe-file ()
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
        ((creole/file-under-root-p
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
        (creole/expand-item-value
         "~/elwikiengine/wiki/small.creole"
         "~/elwikicreole/"))))
    (kill-buffer TMPBUF)))

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
          (creole-css-list-to-style-decl
           (get-text-property 0 :css-list fontified))))
    (should (string-match-p "^span.keyword" style-decl))
    (should (string-match-p "^span.default" style-decl))
    (should (string-match-p "^span.variable-name" style-decl))
    (should (string-match-p "^span.function-name" style-decl))
    (should (string-match-p "^span.type" style-decl))))


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


(ert-deftest creole-wiki-file ()
  "Test that a mocked file can be loaded as a Wiki file."
  (with-temp-buffer
    (insert "= A Creole Wiki file =

This is a nice simple Creole Wiki file.
")
    (let ((creole-file-buffer (current-buffer)))
      (flet ((creole/get-file
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
<a id='A-Creole-Wiki-file'></a>
<h1>A Creole Wiki file</h1>
<p>This is a nice simple Creole Wiki file.</p>
</body>
</html>
")))))))

(defmacro creole/wiki-test (creoletext htmltext &rest extras)
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
  (let (creole-do-anchor-headings)
    (creole/wiki-test
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
" :htmlfontify t)))


(ert-deftest creole-wiki-base ()
  "Test the comprehensive HTML production."
  (creole/wiki-test
    "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<body>
<a id='A-Creole-Document'></a>
<h1>A Creole Document</h1>
<p>This is a Creole document with some stuff in it.</p>
</body>
</html>
"))

(ert-deftest creole-wiki-headers-footers ()
  "Test that specified headers and footers come out correctly."
  (creole/wiki-test
    "= A Creole Document =

This is a Creole document with some stuff in it.
"
    "<html>
<body>
<div id='header'>the header</div>
<a id='A-Creole-Document'></a>
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
  (flet ((creole/file-under-root-p
          (file-name root)
          "/styles.css"))
    (let (creole-do-anchor-headings)
      (creole/wiki-test
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
        :css '("~/elwikicreole/styles.css")))))

(ert-deftest creole-wiki-css-embed ()
  "Test that strings are embedded for CSS when necessary."
  (flet ((creole/file-under-root-p
          (file-name root)
          "/styles.css"))
    (let (creole-do-anchor-headings)
      ;; Test a string specified as the CSS is embedded
      (creole/wiki-test
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
        :css '("p {
font-size: 8pt;
}"))))

  ;; This version tests the full mocking causing embedding
  (with-temp-buffer
    (insert "P { background: blue; }")
    (let (creole-do-anchor-headings
          (css-file-buffer (current-buffer)))
      ;; Mock the 2 functions so that the file is not considered under
      ;; the docroot and so that it's contents if the CSS fragment
      ;; above
      (flet ((creole/file-under-root-p
              (file-name root)
              nil)
             (creole/get-file
              (filename)
              css-file-buffer))
        (creole/wiki-test
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
    :css '("~/someplace/styles.css"))))))

(ert-deftest creole-wiki-js ()
  (flet ((creole/file-under-root-p
             (file-name root)
           "/scripts.js"))
    (let (creole-do-anchor-headings)
      (creole/wiki-test
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
        :javascript '("~/elwikicreole/scripts.js"))
      (creole/wiki-test
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
        :javascript '("$(document).ready(function () {
doSomething();
});
")))))

(ert-deftest creole-directory-list ()
  (flet ((directory-files
          (directory-name)
          '("." ".." "file1.html" "file2.html"))
         (file-attributes
          (file-name)
          (let ((t1 (encode-time 0 32 9 1 12 2011))
                (t2 (encode-time 0 22 9 1 12 2011)))
            (cond
             ((string-match-p ".*/file1.html" file-name)
              `(nil 1 "uid" "grp"
                    ,t1 ,t1 ,t1
                    200 "-rwxrwxrwx-" t
                    1333331114234 0))
             ((string-match-p ".*/file2.html" file-name)
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

;; creole-tests.el ends here
