#' @import data.table pipeR rlist dplyr rprintf xtable stringr htmltools
NULL

star_patterns = list(
    html = "%s<sup>%s</sup>",
    latex = "%s$^{%s}$"    
)

star_signif = c(
    `10%` = "*",
    `5%` = "**",
    `1%` = "***"
)

## sprintf(star_patterns[['html']], 10, star_signif)






##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title To be completed
##' @param obj_list 
##' @param type 
##' @param ... 
##' @return NULL 
##' @author Janko Cizel
rtable <- function(
    obj_list,
    type = 'latex',
    ...
){
    o <- switch(
        type,
        'latex' = latextable(obj_list = obj_list, ...),
        'html' = htmltable(obj_list = obj_list, ...)        
    )

    return(o)
}
