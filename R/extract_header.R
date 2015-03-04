##' Given a list of objects with regression results, return a list of strings
##' that serve as header decorations in a latex table
##'
##' 
##' @param obj_list 
##' @return NULL
##' @author Janko Cizel
##'
##' @export
extract_header_info <- function(obj,...){
    UseMethod('extract_header_info')
}

extract_header_info.felm <- function(obj){
    obj %>>%
    attr("class") ->
        model_name

    obj %>>%
    attr("call") %>>%
    all.vars %>>%
    (.[[1L]]) ->
        model_depvar

    data.table(
        model_name = model_name,
        model_depvar = model_depvar
    )
}

extract_header_info.lm <- function(obj){
    obj %>>%
    attr("class") ->
        model_name

    obj %>>%
    (call) %>>%
    all.vars %>>%
    (.[[1L]]) ->
        model_depvar

    data.table(
        model_name = model_name,
        model_depvar = model_depvar
    )
}
