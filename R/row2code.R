veclist2html <- function(x){
    if (!inherits(x,'list'))
        stop("`x' must be a list of vectors, each of which represents the lines to be added at the bottom of the table")
    x %>>%
    list.map({
        . %>>%
        list.map({
            e = .
            tags$td(e %>>% HTML)
        }) ->
            o
        
        names(o) = NULL

        tags$tr(
            c(
                list(tags$td(.name %>>% HTML,
                             style = 'text-align:left')),
                o
            )
        )
    }) ->
        out

    names(out) = NULL

    return(out)    
}


veclist2latex <- function(x){
    if (!inherits(x,'list'))
        stop("`x' must be a list of vectors, each of which represents the lines to be added at the bottom of the table")
    x %>>%
    list.map({
        c(.name,.) %>>%
        stringify %>>%
        paste(collapse = " & ") ->
            o        

        o
    }) ->
        out

    names(out) <- NULL

    out %>>%
    do.call(what = 'c') %>>%
    paste(collapse = '\\\\ \n') %>>%
    sprintf(fmt = '%s\\\\') ->
        out
    
    return(out)    
}



## x <- list('Additional row' = c('x','y','z'),
##           'second row' = c('a', 2, 'xxx'))
## veclist2html(x)
