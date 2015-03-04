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


##' Given a list of objects with regression results, return a data.table with
##' publication-ready formatted results.
##'
##
##' @param obj_list a list of objects with regression results
##' @param digits  
##' @param stars 
##' @return NULL
##' @author Janko Cizel
##'
##' @export
parse_result_list_coef<- function(
    obj_list = NULL,
    digits = 3,
    stars = TRUE,
    type = 'html',
    drop_coef = NULL,
    label_coef = FALSE,
    lookup_table_coef = NULL,
    lookup_table_coef_name_col = NULL,
    lookup_table_coef_label_col = NULL
){
    ## Number each model
    names(obj_list) <- 1:length(obj_list) %>>% as.character

    select = c('beta','se','pval')
    obj_list %>>%
    list.map(
        . %>>%
        extract_selected(select = select,
                         stars = stars,
                         digits = digits,
                         type = type) %>>%
        setnames(
            old = 'value',
            new = sprintf("(%s)", .name)
        )
    ) %>>%
    Reduce(
        f = function(...){
            merge(..., by = c('id','variable'), all = TRUE)
        }
    ) %>>%
    subset(
        variable %in% c('beta.star','se.fmt')
    ) ->
        o

    if (!is.null(drop_coef)){
        o[!id %>>%
          grepl(
              pattern = drop_coef %>>%
              paste(collapse = "|"),
              ignore.case = TRUE
          )] ->
              o
    }

    ## ROW NAMES
    o[, group := rleid(id)]
    o[, group.row := 1:length(id), by = group]
    o[group.row == 1,
      group.name := id]

    group_name <- o$group.name

    if (label_coef == TRUE){
        if (is.null(lookup_table_coef) |
            is.null(lookup_table_coef_name_col) |
            is.null(lookup_table_coef_label_col))
            stop('`lookup_table_coef` is the obligatory argument if `label_vars==TRUE`')
        
        group_name %>>%
        make_labels(lookup_table = lookup_table_coef,
                    name_col = lookup_table_coef_name_col,
                    label_col = lookup_table_coef_label_col) ->
                        group_name
    }

    o %>>%
    select(
        -id,
        -variable,
        -group,
        -group.row,
        -group.name
    ) %>>%
    ({
        cbind(
            " " = group_name,
            .
        )
    }) -> o

    o[, lapply(.SD, function(c){
        c[is.na(c)] <- ""
        c
    })] ->
        o

    return(o)    
}




##' Given a list of objects with regression results, return a data.table that
##' contains `non-coefficient` estimation results
##' 
##'
##' @param obj_list  
##' @param digits 
##' @return NULL
##' @author Janko Cizel
##'
##' @export
parse_result_list_static <- function(
    obj_list = NULL,
    digits = 3,
    stars = NULL,
    type = 'html',
    drop_stats = NULL,
    label_stats = FALSE,
    lookup_table_stats = NULL,
    lookup_table_stats_name_col = NULL,
    lookup_table_stats_label_col = NULL
){
    names(obj_list) <- 1:length(obj_list)
    
    obj_list %>>%
    list.map(
        . %>>%
        extract_stats(digits = digits) %>>%
        setnames(
            old = 'value',
            new = sprintf("(%s)", .name)
        )
    ) %>>%
    Reduce(
        f = function(...){
            merge(..., by = c('id'), all = TRUE)
        }
    ) ->
        o


    if (!is.null(drop_stats)){
        o[!id %>>%
          grepl(
              pattern = drop_stats %>>%
              paste(collapse = "|"),
              ignore.case = TRUE
          )] ->
              o
    }

    o[, lapply(.SD, function(c){
        c[is.na(c)] <- ""
        c
    })] ->
        o

    if (label_stats == TRUE){
        if (is.null(lookup_table_stats) |
            is.null(lookup_table_stats_name_col) |
            is.null(lookup_table_stats_label_col))
            stop('`lookup_table_stats` is the obligatory argument if `label_vars==TRUE`')
        
        o$id %>>%
        make_labels(lookup_table = lookup_table_stats,
                    name_col = lookup_table_stats_name_col,
                    label_col = lookup_table_stats_label_col) ->
                        o$id
    }    
    
    return(o)
}


header_preparation <- function(
    obj_list = NULL
){
    obj_list %>>%
    list.map({
        . %>>% extract_header_info
    }) %>>%
    rbindlist ->
        o

    o[, row := 1:.N + 1]

    o[, group.model := rleid(model_name)]
    o[, group.model.row := 1:.N, by = group.model]
    o[, group.model.nobs := .N, by = group.model]
    o[, group.model.min := min(row), by = group.model]
    o[, group.model.max := max(row), by = group.model]                

    o[, group := rleid(model_name, model_depvar)]
    o[, group.row := 1:.N, by = group]
    o[, group.nobs := .N, by = group]
    o[, group.min := min(row), by = group]
    o[, group.max := max(row), by = group]

    o %>>%
    split(factor(.$model_name, levels = .$model_name %>>% unique)) %>>%
    list.map(
        list(
            name = .name,
            rows = c(.$group.model.min %>>% unique,
                .$group.model.max %>>% unique),
            length = .$group.model.nobs %>>% unique        
        )
    ) ->
        r1

    o %>>%
    split(interaction(.$model_name %>>% (factor(., levels = . %>>% unique)),
                      .$model_depvar %>>% (factor(., levels = . %>>% unique)))) %>>%
    list.map(
        list(
            name = .$model_depvar %>>% unique,
            rows = c(.$group.min %>>% unique,
                .$group.max %>>% unique),
            length = .$group.nobs %>>% unique        
        )
    ) ->
        r2

    list(
        r1,
        r2
    ) ->
        out
    
    return(out)
}

header_latex <- function(
    obj_list = NULL,
    label_dep = FALSE,
    lookup_table_dep = NULL,
    lookup_table_dep_name_col = NULL,
    lookup_table_dep_label_col = NULL    
){
    obj_list %>>%
    header_preparation ->
        header

    if (label_dep == TRUE){
        if (is.null(lookup_table_dep) |
            is.null(lookup_table_dep_name_col) |
            is.null(lookup_table_dep_label_col))
            stop('`lookup_table_dep` is the obligatory argument if `label_dep==TRUE`')
        
        header %>>% 
        list.map(
            . %>>%
            list.update(
                name = {
                    name %>>%
                    make_labels(lookup_table = lookup_table_dep,
                                name_col = lookup_table_dep_name_col,
                                label_col = lookup_table_dep_label_col)
                }
            )
        ) ->
            header
        
    }

    header %>>%
    list.map({
        . %>>%
        list.map({
            sprintf(
                fmt = "\\multicolumn{%s}{c}{%s}",
                .$length,
                .$name
            )      
        }) ->
            text

        . %>>%
        list.map({
            sprintf(
                fmt = "\\cmidrule(lr){%s-%s}",
                .$rows[[1L]],
                .$rows[[2L]]
            )      
        }) ->
            line

        list(
            text = text %>>% unlist %>>% paste(collapse = ' & ') %>>% sprintf(fmt = "& %s \\\\"),
            line = line %>>% unlist %>>% paste(collapse = ' ')
        )
    })    
}


header_html <- function(
    obj_list = NULL,
    label_dep = FALSE,
    lookup_table_dep = NULL,
    lookup_table_dep_name_col = NULL,
    lookup_table_dep_label_col = NULL    
){
    obj_list %>>%
    header_preparation ->
        header

    if (label_dep == TRUE){
        if (is.null(lookup_table_dep) |
            is.null(lookup_table_dep_name_col) |
            is.null(lookup_table_dep_label_col))
            stop('`lookup_table_dep` is the obligatory argument if `label_dep==TRUE`')
        
        header %>>% 
        list.map(
            . %>>%
            list.update(
                name = {
                    name %>>%
                    make_labels(lookup_table = lookup_table_dep,
                                name_col = lookup_table_dep_name_col,
                                label_col = lookup_table_dep_label_col)
                }
            )
        ) ->
            header
        
    }    

    header %>>%
    list.map({
        row <- .

        row %>>%
        list.map(
            tags$td(
                .$name,
                colspan = .$length
            )
        ) %>>%
        (tags$tr(tags$td(),
                 .)) ->            
            text

        row %>>%
        list.map(
            tags$td(
                colspan = .$length,
                style = "border-bottom: 1px solid black"
            )
        ) %>>%
        (tags$tr(tags$td(),
                 .)) ->
            line

        list(
            text = text,
            line = line
        )
    })
}


header <- function(
    obj_list = NULL,
    type = 'html',
    label_dep = FALSE,
    lookup_table_dep = NULL,
    lookup_table_dep_name_col = NULL,
    lookup_table_dep_label_col = NULL
){
    switch(
        type,
        'html' = header_html(
            obj_list,
            label_dep = label_dep,
            lookup_table_dep = lookup_table_dep,
            lookup_table_dep_name_col = lookup_table_dep_name_col,
            lookup_table_dep_label_col = lookup_table_dep_label_col
        ),
        'latex' = header_latex(
            obj_list,
            label_dep = label_dep,
            lookup_table_dep = lookup_table_dep,
            lookup_table_dep_name_col = lookup_table_dep_name_col,
            lookup_table_dep_label_col = lookup_table_dep_label_col            
        )
    ) ->
        o

    return(o)
}



