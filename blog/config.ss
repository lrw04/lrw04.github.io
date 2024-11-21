(define-config-field title use-child)
(define-config-field date use-child)
(define-config-field page use-child) ; not a post (not included in rss feed)
(define-config-field lang use-child)
(add-local-config '((title . "40whirl's lil silly blog")
                    (lang . "en")))

;;; html templates
(define html-defaults
  (lambda (head body)
    `(html ((lang . "en"))
           ((head ()
                  ((meta ((charset . "utf-8")))
                   (meta ((name . "viewport")
                          (content . "width=device-width, initial-scale=1")))
                   (link ((rel . "stylesheet")
                          (href . "style.css")))
                   (link ((rel . "stylesheet")
                          (href . "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.10.0/build/styles/default.min.css")))
                   (link ((rel . "stylesheet")
                          (href . "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css")))
                   ,@head))
            (body ()
                  ,body)))))

(define file-date
  (lambda (f)
    (eval '(config 'date) (cadr f))))
(define file-page
  (lambda (f)
    (eval '(config 'page) (cadr f))))
(define file-title
  (lambda (f)
    (eval '(config 'title) (cadr f))))

(define nav
  `(nav ()
        ((a ((href . "index.html"))
            (,(config 'title)))
         (a ((href . "about.html"))
            ("about")))))

(define footer
  `(footer ()
           ((p ()
               ((a ((href . "feed.xml"))
                   ((img ((src . "blobcat-rss.png")
                          (style . "height: 2em")
                          (alt . "RSS icon but blobcat")))))))
            (p ()
               ("this is a "
                (a ((href . "https://github.com/lrw04/bwog/"))
                   ("bwog"))
                " website!!"))
            (div ((class . "trans-pride")) ()))))

(define index-template
  (lambda (repo path)
    (let ((files (caddr repo))
          (title (eval '(config 'title) (cadr repo))))
      (html-defaults `((title () (,title)))
                     `(,nav
                       (main ()
                             (,@(if (null? path)
                                    '((p ()
                                         ("hello! i revamped this website in oct 2024. "
                                          "crazy times, eh? "
                                          "thought it would be nice if i wrote english "
                                          "versions for some posts, so i did! for some. "))
                                      (p ()
                                         ("old ones are still kept here. "
                                          "do note that i sounded very much like a "
                                          "closeted trans lesbian in those posts. (duh) "
                                          "so, not hehe, not much self-esteem, "
                                          "generally having a bad time. "))
                                      (p ()
                                         ("go to the "
                                          (a ((href . "about.html"))
                                             ("about"))
                                          " page for bits about myself."))
                                      (p ()
                                         ("there is also a dedicated page for random facts,"
                                          "at "
                                          (a ((href . "facts.html"))
                                             ("facts.html.")))))
                                    '())
                              (table ()
                                     ((tbody ()
                                             ,(map (lambda (file)
                                                     (let* ((filename (car file))
                                                            (node (cdr file))
                                                            (url (string-append filename
                                                                                ".html"))
                                                            (title (file-title node))
                                                            (date (file-date node)))
                                                       `(tr ()
                                                            ((td ()
                                                                 ((a ((href . ,url))
                                                                     (,title))))
                                                             (td ()
                                                                 (,date))))))
                                                   (list-sort
                                                    (lambda (a b)
                                                      (string>? (file-date (cdr a))
                                                                (file-date (cdr b))))
                                                    (remp (lambda (file)
                                                            (file-page (cdr file)))
                                                          files))))))))
                       ,footer)))))

(define file-template
  (lambda (repo path node)
    (let ((title (file-title node))
          (html (cadddr node)))
      (html-defaults `((title () (,title)))
                     `(,nav
                       (main ((lang . ,(eval '(config 'lang) (cadr node))))
                             (,html))
                       ,footer)))))

(add-local-config `((index-template . ,index-template)
                    (file-template . ,file-template)))

;;; math/code/latex markup processors
(define kill-process
  (case (machine-type)
    ((i3nt ti3nt a6nt ta6nt arm64nt tarm64nt)
     (lambda (pid)
       (system (string-append "taskkill /f /pid " (number->string pid)))))
    (else
     (lambda (pid)
       (system (string-append "kill -s KILL " (number->string pid)))))))

(define mkdir-p
  (lambda (p)
    (if (file-exists? p)
        #f
        (begin (mkdir-p (path-parent p))
               (mkdir p)))))

(define cache-root
  (case (machine-type)
    ((i3nt ti3nt a6nt ta6nt arm64nt tarm64nt)
     "%USERPROFILE%\\.cache\\bwog")
    (else
     "~/.cache/bwog")))
(mkdir-p cache-root)

(library (sha)
  (export sha-224 sha-256)
  (import (chezscheme))
  ;; sha2.scm -- SHA2 digest algorithms
  ;; Copyright (c) 2014 Alex Shinn.  All rights reserved.
  ;; BSD-style license: http://synthcode.com/license.txt

  ;; http://csrc.nist.gov/groups/STM/cavp/documents/shs/sha256-384-512.pdf
  ;; http://tools.ietf.org/html/rfc6234

  ;; Note 1: All variables are 32 bit unsigned integers and addition is
  ;;   calculated modulo 32
  ;; Note 2: For each round, there is one round constant k[i] and one entry
  ;;   in the message schedule array w[i], 0 ≤ i ≤ 63
  ;; Note 3: The compression function uses 8 working variables, a through h
  ;; Note 4: Big-endian convention is used when expressing the constants in
  ;;   this pseudocode, and when parsing message block data from bytes to
  ;;   words, for example, the first word of the input message "abc" after
  ;;   padding is #x61626380

  ;; On a 32-bit machine, these will involve bignum computations
  ;; resulting in poor performance.  Breaking this down into separate
  ;; 16-bit computations may help.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Utilities.

  ;; We fake 32-bit arithmetic by ANDing out the low 32 bits.
  (define (u32 n)
    (bitwise-and n #xFFFFFFFF))

  ;; 32-bit addition.
  (define (u32+ a b)
    (u32 (+ a b)))

  ;; Extract bytes 0..3 of a big-endian 32-bit value.
  (define (extract-byte n i)
    (bitwise-and #xFF (bitwise-arithmetic-shift n (* i -8))))

  ;; Rotate right in 32 bits.
  (define (bitwise-rot-u32 n k)
    (bitwise-ior
     (u32 (bitwise-arithmetic-shift n (- 32 k)))
     (bitwise-arithmetic-shift n (- k))))

  (define (hex32 num)
    (let* ((res (number->string num 16))
           (len (string-length res)))
      (if (>= len 8)
          res
          (string-append (make-string (- 8 len) #\0) res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; The first 32 bits of the fractional parts of the square roots of
  ;; the first 8 primes 2..19:

  (define sha-224-inits
    '#(#xc1059ed8 #x367cd507 #x3070dd17 #xf70e5939
       #xffc00b31 #x68581511 #x64f98fa7 #xbefa4fa4))

  ;; The second 32 bits of the fractional parts of the square roots of
  ;; the 9th through 16th primes 23..53.

  (define sha-256-inits
    '#(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
       #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

  ;; First 32 bits of the fractional parts of the cube roots of the
  ;; first 64 primes 2..311:

  (define k
    '#(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
       #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
       #xd807aa98 #x12835b01 #x243185be #x550c7dc3
       #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
       #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
       #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
       #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
       #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
       #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
       #x650a7354 #x766a0abb #x81c2c92e #x92722c85
       #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
       #xd192e819 #xd6990624 #xf40e3585 #x106aa070
       #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
       #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
       #x748f82ee #x78a5636f #x84c87814 #x8cc70208
       #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

  (define (sha-224-256 src inits full?)
    (let ((in (cond ((string? src) (open-bytevector-input-port (string->utf8 src)))
                    ((bytevector? src) (open-bytevector-input-port src))
                    ((input-port? src) src)
                    (else (error "unknown digest source: " src))))
          (buf (make-bytevector 64 0))
          (w (make-vector 64 0)))
      (let chunk ((i 0)
                  (pad #x80)
                  (h0 (vector-ref inits 0))
                  (h1 (vector-ref inits 1))
                  (h2 (vector-ref inits 2))
                  (h3 (vector-ref inits 3))
                  (h4 (vector-ref inits 4))
                  (h5 (vector-ref inits 5))
                  (h6 (vector-ref inits 6))
                  (h7 (vector-ref inits 7)))
        (let* ((n (get-bytevector-n! in buf 0 64))
               (n (if (eof-object? n) 0 n)))
          ;; Maybe pad.
          (cond
           ((< n 64)
            (let ((len (* 8 (+ i n))))
              (bytevector-u8-set! buf n pad)
              (do ((j (+ n 1) (+ j 1))) ((>= j 64))
                (bytevector-u8-set! buf j 0))
              (cond
               ((< n 56)
                (bytevector-u8-set! buf 63 (extract-byte len 0))
                (bytevector-u8-set! buf 62 (extract-byte len 1))
                (bytevector-u8-set! buf 61 (extract-byte len 2))
                (bytevector-u8-set! buf 60 (extract-byte len 3))
                (bytevector-u8-set! buf 59 (extract-byte len 4))
                (bytevector-u8-set! buf 58 (extract-byte len 5))
                (bytevector-u8-set! buf 57 (extract-byte len 6))
                (bytevector-u8-set! buf 56 (extract-byte len 7)))))))
          ;; Copy block i into the buffer.
          (do ((j 0 (+ j 1)))
              ((= j 16))
            (vector-set! w j (bytevector-u32-ref buf (* j 4) 'big)))
          ;; Extend the first 16 words into the remaining 48 words
          ;; w[16..63] of the message schedule array:
          (do ((j 16 (+ j 1)))
              ((= j 64))
            (let* ((w15 (vector-ref w (- j 15)))
                   (w2 (vector-ref w (- j 2)))
                   (s0 (bitwise-xor (bitwise-rot-u32 w15 7)
                                    (bitwise-rot-u32 w15 18)
                                    (bitwise-arithmetic-shift w15 -3)))
                   (s1 (bitwise-xor (bitwise-rot-u32 w2 17)
                                    (bitwise-rot-u32 w2 19)
                                    (bitwise-arithmetic-shift w2 -10))))
              (vector-set! w j (u32 (+ (vector-ref w (- j 16))
                                       s0
                                       (vector-ref w (- j 7))
                                       s1)))))
          ;; Compression function main loop:
          (let lp ((j 0)
                   (a h0) (b h1)
                   (c h2) (d h3)
                   (e h4) (f h5)
                   (g h6) (h h7))
            (cond
             ((= j 64)
              (let ((a (u32+ h0 a)) (b (u32+ h1 b))
                    (c (u32+ h2 c)) (d (u32+ h3 d))
                    (e (u32+ h4 e)) (f (u32+ h5 f))
                    (g (u32+ h6 g)) (h (u32+ h7 h)))
                (cond
                 ((< n 64)
                  (if (>= n 56)
                      (chunk (+ i n) 0 a b c d e f g h)
                      (string-append
                       (hex32 a) (hex32 b) (hex32 c) (hex32 d)
                       (hex32 e) (hex32 f) (hex32 g) (if full? (hex32 h) ""))))
                 (else
                  (chunk (+ i 64) pad a b c d e f g h)))))
             (else
              ;; Step - compute the two sigmas and recurse on the new a-h.
              (let* ((s1 (bitwise-xor (bitwise-rot-u32 e 6)
                                      (bitwise-rot-u32 e 11)
                                      (bitwise-rot-u32 e 25)))
                     (ch (bitwise-xor (bitwise-and e f)
                                      (bitwise-and (bitwise-not e) g)))
                     (temp1 (u32 (+ h s1 ch (vector-ref k j) (vector-ref w j))))
                     (s0 (bitwise-xor (bitwise-rot-u32 a 2)
                                      (bitwise-rot-u32 a 13)
                                      (bitwise-rot-u32 a 22)))
                     (maj (bitwise-xor (bitwise-and a b)
                                       (bitwise-and a c)
                                       (bitwise-and b c)))
                     (temp2 (u32+ s0 maj)))
                (lp (+ j 1)
                    (u32+ temp1 temp2) a b c
                    (u32+ d temp1) e f g)))))))))

  (define (sha-224 src)
    (sha-224-256 src sha-224-inits #f))

  (define (sha-256 src)
    (sha-224-256 src sha-256-inits #t)))
(import (sha))

(define run-process
  (lambda (cmdline input cache)
    (mkdir-p (path-build cache-root cache))
    (let ((cf (path-build (path-build cache-root cache)
                          (sha-256 input))))
      (if (file-exists? cf)
          (let* ((port (open-file-input cf))
                 (out (get-string-all port)))
            (close-input-port port)
            out)
          (let-values (((stdin stdout stderr pid)
                        (open-process-ports cmdline
                                            'block
                                            (make-transcoder (utf-8-codec)))))
            (put-string stdin input)
            (close-output-port stdin)
            (let ((out (get-string-all stdout)))
              (kill-process pid)
              (let ((port (open-file-output cf)))
                (put-string port out)
                (close-output-port port))
              out))))))

(add-markup-processor
 (make-tag-predicate 'code)
 (lambda (t)
   (let* ((lang-absent (null? (cddr t)))
          (lang (if lang-absent "txt" (symbol->string (cadr t))))
          (code ((if lang-absent cadr caddr) t))
          (js (string-append "import hljs from 'npm:highlight.js@11.10.0';
Deno.stdout.writeSync(new TextEncoder().encode(hljs.highlight(\""
                             (string-escape code)
                             "\", {language: \""
                             (string-escape lang)
                             "\"}).value));")))
     `(pre ()
           ((code ((class . ,(string-append "language-" lang)))
                  ((raw ,(run-process "deno run -" js "deno")))))))))

(define latex-macros
  '(("\\Gal" 0 "\\operatorname{Gal}")
    ("\\tr" 0 "\\operatorname{tr}")
    ("\\GL" 0 "\\operatorname{GL}")
    ("\\SL" 0 "\\operatorname{SL}")
    ("\\PSL" 0 "\\operatorname{PSL}")
    ("\\SO" 0 "\\operatorname{SO}")
    ("\\SU" 0 "\\operatorname{SU}")
    ("\\im" 0 "\\operatorname{im}")
    ("\\cof" 0 "\\operatorname{cof}")
    ("\\End" 0 "\\operatorname{End}")
    ("\\Tor" 0 "\\operatorname{Tor}")
    ("\\rk" 0 "\\operatorname{rk}")
    ("\\Hom" 0 "\\operatorname{Hom}")
    ("\\diag" 0 "\\operatorname{diag}")
    ("\\vspan" 0 "\\operatorname{span}")
    ("\\lcm" 0 "\\operatorname{lcm}")
    ("\\id" 0 "\\operatorname{id}")
    ("\\Ab" 0 "\\textsf{Ab}")
    ("\\Fld" 0 "\\textsf{Fld}")
    ("\\Mod" 1 "#1\\textsf{-Mod}")
    ("\\Grp" 0 "\\textsf{Grp}")
    ("\\dSet" 1 "#1\\textsf{-Set}")
    ("\\Set" 0 "\\textsf{Set}")
    ("\\SetStar" 0 "\\textsf{Set*}")
    ("\\Vect" 1 "#1\\textsf{-Vect}")
    ("\\Alg" 1 "#1\\textsf{-Alg}")
    ("\\Ring" 0 "\\textsf{Ring}")
    ("\\R" 0 "\\mathbb{R}")
    ("\\C" 0 "\\mathbb{C}")
    ("\\N" 0 "\\mathbb{N}")
    ("\\Z" 0 "\\mathbb{Z}")
    ("\\Q" 0 "\\mathbb{Q}")
    ("\\F" 0 "\\mathbb{F}")
    ("\\sfC" 0 "\\mathsf{C}")
    ("\\vphi" 0 "\\varphi")))
(define latex-macro-decls
  (apply string-append
         (map (lambda (macro)
                (string-append "\\providecommand"
                               (car macro)
                               "{}\\renewcommand"
                               (car macro)
                               "["
                               (number->string (cadr macro))
                               "]{"
                               (caddr macro)
                               "}"))
              latex-macros)))

(add-markup-processor
 (make-tag-predicate 'math)
 (lambda (t)
   (let* ((latex (cadr t))
          (str (string-append latex-macro-decls latex))
          (js (string-append "import katex from 'npm:katex@0.16.11';
Deno.writeAllSync(Deno.stdout, new TextEncoder().encode(katex.renderToString(\""
                             (string-escape str)
                             "\")));")))
     `(raw ,(run-process "deno run -" js "deno")))))
(add-markup-processor
 (make-tag-predicate 'dmath)
 (lambda (t)
   (let* ((latex (cadr t))
          (str (string-append latex-macro-decls latex))
          (js (string-append "import katex from 'npm:katex@0.16.11';
Deno.writeAllSync(Deno.stdout, new TextEncoder().encode(katex.renderToString(\""
                             (string-escape str)
                             "\", {displayMode: true})));")))
     `(raw ,(run-process "deno run -" js "deno")))))

(define latex-preamble
  "\\usepackage{amsmath, amssymb, amsthm, latexsym, mathrsfs, eucal, ctex}
\\usepackage[dvipsnames]{xcolor}
\\usepackage{tabularx, tikz-cd, tikz, bm}")

(define latex->svg
  (lambda (latex)
    (if (file-exists? "tmp.tex") (error 'latex->svg "tmp.tex exists"))
    (mkdir-p (path-build cache-root "latex"))
    (let ((cf (path-build (path-build cache-root "latex")
                          (sha-256 latex))))
      (if (file-exists? cf)
          (let* ((port (open-file-input cf))
                 (out (get-string-all port)))
            (close-input-port port)
            out)
          (let ((port (open-file-output "tmp.tex")))
            (put-string port latex)
            (close-output-port port)
            (system "tectonic tmp.tex")
            (system "dvisvgm --pdf -f woff2 tmp.pdf")
            (let* ((port (open-file-input "tmp.svg"))
                   (svg (get-string-all port)))
              (close-input-port port)
              (delete-file "tmp.tex")
              (delete-file "tmp.pdf")
              (delete-file "tmp.svg")
              (let ((port (open-file-output cf)))
                (put-string port svg)
                (close-output-port port)
                svg)))))))

(add-markup-processor
 (make-tag-predicate 'latex)
 (lambda (t)
   (let* ((text (cadr t))
          (latex (string-append "\\documentclass{standalone}"
                                latex-preamble
                                latex-macro-decls
                                "\\begin{document}"
                                text
                                "\\end{document}"))
          (svg (latex->svg latex)))
     `(raw ,svg))))

(add-markup-processor
 (make-tag-predicate 'div)
 (lambda (t)
   `(div ((class . ,(symbol->string (cadr t))))
         ,(process-subtree (caddr t)))))

;;; rss
(define link "https://blog.lrw04.online/")
(define files->rss
  (lambda (files)
    `(rss ((version . "2.0")
           (xmlns:atom . "http://www.w3.org/2005/Atom"))
          ((channel ()
                    ((title ()
                            (,(config 'title)))
                     (link ()
                           (,link))
                     (description ()
                                  ("Documentation for bwog"))
                     (atom:link ((href . ,(string-append link "feed.xml"))
                                 (rel . "self")
                                 (type . "application/rss+xml"))
                                ())
                     ,@(map (lambda (file)
                              (let* ((path (car file))
                                     (file (cdr file))
                                     (env (cadr file))
                                     (url (string-append link
                                                         (join "/" path)
                                                         ".html")))
                                `(item ()
                                       ((title ()
                                               (,(eval '(config 'title) env)))
                                        (link ()
                                              (,url))
                                        (description ()
                                                     ;; use the title again
                                                     (,(eval '(config 'title) env)))
                                        (guid ()
                                              (,url))))))
                            files)))))))

(define-config-field rss-enable use-child)
(add-local-config '((rss-enable . #t)))
(add-after-hook (lambda (repo root path)
                  (if (null? path)
                      (letrec ((all-files
                                (lambda (repo path)
                                  (let ((files (caddr repo))
                                        (subdirs (cadddr repo)))
                                    (apply append
                                           (map (lambda (file)
                                                  (let ((name (car file))
                                                        (node (cdr file)))
                                                    (cons (append path (list name))
                                                          node)))
                                                files)
                                           (map (lambda (subdir)
                                                  (all-files (cdr subdir)
                                                             (append path
                                                                     (list (car subdir)))))
                                                subdirs))))))
                        (let* ((files (all-files repo '()))
                               (files (filter (lambda (file)
                                                (let ((env (caddr file)))
                                                  (not (eval '(config 'page) env))))
                                              files))
                               (xml (files->rss files))
                               (port (open-file-output (path-build root "feed.xml"))))
                          (put-string port "<?xml version=\"1.0\"?>")
                          (xml>> xml port)
                          (close-output-port port))))))

;;; solid period for chinese posts
(add-markup-processor char?
                      (lambda (t)
                        (escape-string (string t)
                                       '((#\。 . "．")))))
