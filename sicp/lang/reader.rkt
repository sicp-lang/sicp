#lang s-exp syntax/module-reader
#:language 'sicp
#:wrapper1 (lambda (proc)
             (parameterize ((print-as-expression      #false)
                            (print-pair-curly-braces  #true)
                            (print-mpair-curly-braces #false))
               (proc)))
