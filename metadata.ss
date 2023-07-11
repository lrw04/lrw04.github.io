`((propagation-rules . ((nav . ,use-child)
                        (footer . ,use-child)
                        (page . ,use-child)))
  (config . ((title . "wlr 的博客")
             (link . "https://blog.lrw04.online/")
             (index-template . ,(lambda (config files)
                                  (default-base
                                    `((link ((rel . "stylesheet")
                                             (href . "style.css")))
                                      (title () (,(config-title config))))
                                    `(,(read-config config 'nav)
                                      (main ()
                                            ((table ()
                                                    ((tbody ()
                                                            ,(map (lambda (f)
                                                                    `(tr ()
                                                                         ((td ()
                                                                              ((a ((href . ,(string-append (file-filename f) ".html")))
                                                                                  (,(file-title f)))))
                                                                          (td ()
                                                                              (,(file-date f))))))
                                                                  (list-sort
                                                                   (lambda (a b)
                                                                     (string>? (file-date a) (file-date b)))
                                                                   (remp (lambda (file) (read-config (file-metadata file) 'page)) files))))))))
                                      ,(read-config config 'footer)))))
             (template . ,(lambda (config document)
                            (default-base
                              `((link ((rel . "stylesheet")
                                       (href . "style.css")))
                                (link ((rel . "stylesheet")
                                       (href . "https://cdn.jsdelivr.net/npm/prismjs@1/themes/prism.min.css")))
                                (link ((rel . "stylesheet")
                                       (href . "https://cdn.jsdelivr.net/npm/katex@0/dist/katex.min.css")))
                                (title () (,(cdr (assoc 'title config)))))
                              `(,(read-config config 'nav)
                                (main ()
                                      (,document))
                                ,(read-config config 'footer)
                                (script ((src . "https://cdn.jsdelivr.net/npm/prismjs@1/components/prism-core.min.js")) ())
                                (script ((src . "https://cdn.jsdelivr.net/npm/prismjs@1/plugins/autoloader/prism-autoloader.min.js")) ())
                                (script ((src . "https://cdn.jsdelivr.net/npm/katex@0/dist/katex.min.js")) ())
                                (script () ((raw ,(string-append "const macros = " (make-katex-macros (cdr (assoc 'latex-macros config)))))))
                                (script ((src . "katex-loader.js")) ())
                                (script ((src . "svg-resize.js")) ())))))
             (processors . ((,(make-tag-pred 'latex) . ,(lambda (config t)
                                                          (if (file-exists? "tmp.tex") (error 'latex-proc "tmp.tex exists"))
                                                          (let* ((text (cadr t))
                                                                 (latex-source (string-append "\\documentclass{standalone}"
                                                                                              (read-config config 'preamble)
                                                                                              (make-latex-macros (read-config config 'latex-macros))
                                                                                              "\\begin{document}"
                                                                                              text
                                                                                              "\\end{document}"))
                                                                 (port (open-file-output-port "tmp.tex"
                                                                                              (file-options no-fail)
                                                                                              'block
                                                                                              (make-transcoder (utf-8-codec)))))
                                                            (display latex-source port)
                                                            (close-output-port port)
                                                            (system "tectonic tmp.tex")
                                                            (system "dvisvgm --pdf -f ttf tmp.pdf")
                                                            ;; get svg
                                                            (delete-file "tmp.tex")
                                                            (delete-file "tmp.pdf")
                                                            (let* ((port (open-file-input-port "tmp.svg"
                                                                                               (file-options)
                                                                                               'block
                                                                                               (make-transcoder (utf-8-codec))))
                                                                   (svg (get-string-all port)))
                                                              (close-input-port port)
                                                              (delete-file "tmp.svg")
                                                              `(raw ,svg)))))))
             (desc . "Documentation for bwog, a static blog generator written in Scheme.")
             (latex-macros . ())
             (nav . (header ()
                            ((nav ()
                                  ((a ((href . "index.html"))
                                      ((img ((src . "avatar.png")))
                                       "wlr 的博客"))
                                   " "
                                   (a ((href . "about.html"))
                                      ("关于我")))))))
             (footer . (footer ()
                               ((p ()
                                   ((a ((href . "feed.xml"))
                                       ("RSS Feed"))))
                                (p ()
                                   ("Generated with bwog"))
                                (p ()
                                   ((div ((class . "pride")) ()))))))
             (preamble . "\\usepackage{amsmath, amssymb, amsthm, latexsym, mathrsfs, eucal, ctex}
\\usepackage[dvipsnames]{xcolor}
\\usepackage{tabularx, tikz-cd, tikz, bm}")))
  (files . ("about"
            ;"after-comp"
            ;"bwog"
            ;"callcc-impl"
            ;"cube-orientation"
            ;"det-equality"
            ;"heptadecagon"
            ;"homebrew-exam"
            ;"lisp-impl"
            ;"oj-sandbox"
            ;"poorman-mlp"
            ;"quartic"
            ;"ranobe-typeset"
            ;"taca2021-commentary"
            )))
