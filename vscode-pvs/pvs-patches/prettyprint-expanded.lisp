(in-package :pvs-jsonrpc)

(defrequest prettyprint-expanded (theory-ref)
  "Returns the pretty-print expanded form of the given theory"
  (let ((theory (pvs::get-typechecked-theory theory-ref))
        (pvs::*no-comments* nil)
        (pvs::*unparse-expanded* t)
        (pvs::*xt-periods-allowed* t))
    (let ((thstring (pvs::unparse theory
                        :string t
                        :char-width sb-runtime::*default-char-width*)))
                    thstring)))