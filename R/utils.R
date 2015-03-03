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
    label_pattern = "%s"
){
    if (is.null(vars) | is.null(lookup_table) | is.null(name_col) | is.null(label_col))
        stop('`vars`,`lookup_table`,`name_col`, and `label_col` are the required arguments')

    l <- (data.table(lookup_table, key = name_col) %>>% unique)[vars] %>>% copy
    l[is.na(get(label_col)), (label_col) := get(name_col)]

    l[[label_col]] %>>%
    sprintf(
        fmt = label_pattern
    ) ->
        out
    
    return(out %>>% as.character)
}


