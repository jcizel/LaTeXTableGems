## ##' .. content for \description{} (no empty lines) ..
## ##'
## ##' .. content for \details{} ..
## ##' @title Convert a data.table into inner part of latex table (table
## ##' environment, column names, rules to be produced manually)
## ##' @param dt data.table to be converted into latex table 
## ##' @param outfile path of the destination file
## ##' @param digits digit formating. Useful only if table consists only of numbers
## ##' with the same desired formating. If this is not the case, it is easier to
## ##' arrange the formating manually on the original data.table.
## ##' @param include.rownames Include rownames? Default is FALSE.
## ##' @param include.colnames Include colnames? Default is FALSE
## ##' @return Character vector, where each entry corresponds to the row in the
## ##' output data table. Main use of this function
## ##' is its side product consisting of the 'outfile' latex table.
## ##' @author Janko Cizel
## dataTableToInnerLatex <- function(dt,
##                                   outfile = '~/Downloads/test.tex',
##                                   digits = 0,
##                                   include.rownames = FALSE,
##                                   include.colnames = FALSE){
##     out <- capture.output({
##         print(xtable:::xtable(dt,
##                               digits = digits
##                               ),
##               only.contents  = T,
##               hline = NULL,
##               include.rownames = include.rownames,
##               include.colnames = include.colnames,
##               sanitize.text.function = LaTeXTableGems:::.sanitize)
##     })
    
##     LaTeXTableGems:::innerTEX(out,
##                               file = outfile)

##     return(out)
## }

## createLatexTableHeader <- function(
##     table.parameters =  paste0('{P{1cm}*{',length(unlist(definition))-1,'}{R{2.2cm}}}'),
##     definition =
##         list(list(list('')),
##              'Non-Financial Score'=
##                  list('Rating',
##                       'Bond Spread',
##                       'CDS Spread'),
##              'Banking Score (Closure Model)' =
##                  list('Rating',
##                       'Bond Spread',
##                       'CDS Spread'),
##              'Banking Score (Closure Model)' =
##                  list('Rating',
##                       'Bond Spread',
##                       'CDS Spread')),
##     outfile = '~/Downloads/test.tex'
## ){
##     r1 <- names(definition)
##     l1 <- sapply(definition, length)
##     o1 <- paste0(paste(paste0('\\multicolumn{',l1,'}{c}{',r1,'}'), collapse = '\n & '),'\\\\')

##     i <- 1
##     .m1 <- list()
##     for (x in 1:length(l1)) {
##         if (names(l1[x])=='') {
##             i <- i + 1
##             next
##         }
##         else {
##             .m1[[length(.m1)+1]] <- paste0('\\cmidrule(lr){',i,'-',i + l1[[x]]-1,'}')
##             i <- i + l1[[x]]
##         }
##     }
    
##     m1 <- paste(.m1,collapse = '')

##     r2 <-
##         foreach(x = 1:length(definition),
##                 .combine = c) %do% {
##                     unlist(definition[[x]])
##                 }

##     o2 <- paste0(paste(paste0('\\multicolumn{1}{c}{',r2,'}'), collapse = '\n & '),'\\\\')

##     m2 <- '\\midrule'
        
##     out <- c(table.parameters,
##              o1,
##              m1,
##              o2,
##              m2)
    
##     sink(outfile)
##     cat(paste(out, collapse = "\n"))
##     sink()
##     cat(paste0("The table header was written to the file '", outfile, "'.\n"))
##     return(out)
## }
