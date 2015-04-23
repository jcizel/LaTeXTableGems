##' Make labels from a character vector of variable names
##'
##' 
##' @param vars 
##' @param lookup_table 
##' @param name_col 
##' @param label_col 
##' @param label_pattern
##' @return character vector of labels, corresponding to the variable names. If
##' variable name is not available in the lookup table, the returned vector
##' contains the variable name itself.
##'
##' 
##' @author Janko Cizel
##'
##' @export
make_labels <- function(
    vars = NULL,
    lookup_table = NULL,
    name_col= NULL,
    label_col= NULL,
    label_pattern = "[%s]",
    split_char = ":"
){
    if (is.null(vars) | is.null(lookup_table) | is.null(name_col) | is.null(label_col))
        stop('`vars`,`lookup_table`,`name_col`, and `label_col` are the required arguments')

    vars %>>%
    str_split(pattern = split_char) %>>%
    list.map({
        l <- (data.table(lookup_table, key = name_col) %>>% unique)[.] %>>% copy
        l[is.na(get(label_col)), (label_col) := get(name_col)]
        l[[label_col]] %>>%
        sprintf(
            fmt = label_pattern
        ) %>>%
        paste(collapse = " * ")
    }) %>>%
    unlist ->
        out
    
    return(out %>>% as.character)
}



sanitizer <- function(str, type = 'latex'){
    if (type == 'latex'){
        str %>>%
        ## gsub(pattern = "\\\\", replacement = "SANITIZE.BACKSLASH") %>>%
        ## gsub(pattern = "$", replacement = "\\$", fixed = TRUE) %>>%
        gsub(pattern = ">", replacement = "$>$", fixed = TRUE) %>>%
        gsub(pattern = "<", replacement = "$<$", fixed = TRUE) %>>%
        gsub(pattern = "|", replacement = "$|$", fixed = TRUE) %>>%
        ## gsub(pattern = "{", replacement = "\\{", fixed = TRUE) %>>%
        ## gsub(pattern = "}", replacement = "\\}", fixed = TRUE) %>>%
        gsub(pattern = "%", replacement = "\\%", fixed = TRUE) %>>%
        gsub(pattern = "&", replacement = "\\&", fixed = TRUE) %>>%
        gsub(pattern = "_", replacement = "\\_", fixed = TRUE) %>>%
        gsub(pattern = "#", replacement = "\\#", fixed = TRUE) %>>%
        ## gsub(pattern = "^", replacement = "\\verb|^|", fixed = TRUE) %>>%
        gsub(pattern = "~", replacement = "\\~{}", fixed = TRUE) %>>%
        gsub(pattern = "SANITIZE.BACKSLASH", replacement = "$\\backslash$", fixed = TRUE) ->
            result
    } else if (type == 'html') {
        str %>>%
        gsub(pattern = "&", replacement = "&amp;", fixed = TRUE)
        gsub(pattern = ">", replacement = "&gt;", fixed = TRUE)
        gsub(pattern = "<", replacement = "&lt;", fixed = TRUE) ->
            result
    } else {
        stop('Type must be either `latex` or `html`')
    }
    
    return(result)
}



stringify <- function(x = NULL, digits = 2){
    if (is.null(x))
        stop("Specify input vector x!")
    
    switch(
        typeof(x),
        "double" = {
            x %>>%
            formatC(digits = digits, format = 'f')
        },
        "integer" = {
            x %>>%
            formatC(format = 'f', digits = 0, big.mark = ',')
        },
        "character" = {
            x %>>%
            sprintf(fmt = "%s")
        }            
    )
}


computeColWidth = function(
    data,
    digits = 2,
    widthScale = function(nchar) nchar
){
    data %>>%
    list.map(
        . %>>% stringify %>>% nchar %>>% max
    ) %>>%
    unlist %>>%
    widthScale %>>%
    ({
        . / sum(.)
    }) %>>%
    as.list
}




