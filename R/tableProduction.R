##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Convert a data.table into inner part of latex table (table
##' environment, column names, rules to be produced manually)
##' @param dt data.table to be converted into latex table 
##' @param outfile path of the destination file
##' @param digits digit formating. Useful only if table consists only of numbers
##' with the same desired formating. If this is not the case, it is easier to
##' arrange the formating manually on the original data.table.
##' @param include.rownames Include rownames? Default is FALSE.
##' @param include.colnames Include colnames? Default is FALSE
##' @return Character vector, where each entry corresponds to the row in the
##' output data table. Main use of this function
##' is its side product consisting of the 'outfile' latex table.
##' @author Janko Cizel
dataTableToInnerLatex <- function(dt,
                                  outfile = '~/Downloads/test.tex',
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
    
    LaTeXTableGems:::innerTEX(out,
                              file = outfile)

    return(out)
}
