
dataTableToInnerLatex <- function(dt,
                                  file = './Downloads/test.tex',
                                  digits = 0,
                                  include.rownames = FALSE,
                                  include.colnames = FALSE){
    out <- capture.output({
        print(xtable:::xtable(dt,
                              digits = digits
                              ),
              only.contents  = T,
              hline = NULL,
              include.rownames = include.rownames,
              include.colnames = include.colnames,
              sanitize.text.function = LaTeXTableGems:::.sanitize)
    })
    
    LaTeXTableGems:::.innerTEX(out,
                               file = file)

    return(out)
}
