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
    coef.presentation = 'T1',
    drop_coef = NULL,
    keep_coef = NULL,
    order_coef = NULL,
    label_coef = FALSE,
    lookup_table_coef = NULL,
    lookup_table_coef_name_col = NULL,
    lookup_table_coef_label_col = NULL,
    label_pattern = "[%s]"
){
    if (!is.null(drop_coef) & !is.null(keep_coef))
        stop("Cannot specify `drop_coef' and `keep_coef' simultaniously!")
    
    ## Number each model
    names(obj_list) <- 1:length(obj_list) %>>% as.character

    obj_list %>>%
    list.map(
        . %>>%
        extract_selected(
            stars = stars,
            digits = digits,
            type = type
        ) %>>%
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
    ({
        if (coef.presentation == 'T1'){
            . %>>%
            subset(
                variable %in% c('beta.star','se.fmt')
            )
        } else if ((coef.presentation == 'T2')){
            . %>>%
            subset(
                variable %in% c('T2')
            )
        }
    })  ->
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

    if (!is.null(keep_coef)){
        o[id %>>%
          grepl(
              pattern = keep_coef %>>%
              paste(collapse = "|"),
              ignore.case = TRUE
          )] ->
              o
    }    

    if (!is.null(order_coef)){
        o %>>%
        mutate(
            id = id %>>%
            as.character %>>%
            factor(levels = order_coef)
        ) %>>%
        arrange(
            id
        ) %>>%
        subset(
            !is.na(id)
        ) ->
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
        
        group_name %>>% as.character %>>%
        make_labels(lookup_table = lookup_table_coef,
                    name_col = lookup_table_coef_name_col,
                    label_col = lookup_table_coef_label_col,
                    label_pattern = label_pattern) ->
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
        c %>>% as.character ->
            c
        
        c[is.na(c) |
              (c %>>% sprintf(fmt = label_pattern)) == ('NA' %>>% sprintf(fmt = label_pattern))] <- ""
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
    keep_stats = NULL,
    order_stats = NULL,    
    label_stats = FALSE,
    lookup_table_stats = NULL,
    lookup_table_stats_name_col = NULL,
    lookup_table_stats_label_col = NULL,
    label_pattern = "[%s]"
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

    if (!is.null(keep_stats)){
        o[id %>>%
          grepl(
              pattern = keep_stats %>>%
              paste(collapse = "|"),
              ignore.case = TRUE
          )] ->
              o
    }    

    if (!is.null(order_stats)){
        o %>>%
        mutate(
            id = id %>>%
            as.character %>>%
            factor(levels = order_stats)
        ) %>>%
        arrange(
            id
        ) %>>%
        subset(
            !is.na(id)
        ) ->
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
        
        o$id %>>% as.character %>>%
        make_labels(lookup_table = lookup_table_stats,
                    name_col = lookup_table_stats_name_col,
                    label_col = lookup_table_stats_label_col,
                    label_pattern = label_pattern) ->
                        o$id
    }    
    
    return(o)
}


header_preparation <- function(
    obj_list = NULL
){
    N = length(obj_list) + 1
    
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
    lookup_table_dep_label_col = NULL,
    label_pattern = "[%s]",
    table.width = 14,
    first.col.width = 3
){
    N = length(obj_list) + 1
    col.width = (table.width - first.col.width) / (N-1)
    
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
                    name %>>% as.character %>>%
                    make_labels(lookup_table = lookup_table_dep,
                                name_col = lookup_table_dep_name_col,
                                label_col = lookup_table_dep_label_col,
                                label_pattern = label_pattern)
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
                fmt = "\\multicolumn{%s}{C{%scm}}{%s}",
                .$length,
                .$length * col.width,
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
            text = text %>>% sanitizer %>>% unlist %>>% paste(collapse = ' & ') %>>% sprintf(fmt = "& %s \\\\"),
            line = line %>>% unlist %>>% paste(collapse = ' ')
        )
    })    
}


header_html <- function(
    obj_list = NULL,
    label_dep = FALSE,
    lookup_table_dep = NULL,
    lookup_table_dep_name_col = NULL,
    lookup_table_dep_label_col = NULL,
    label_pattern = "[%s]",
    table.width = 14
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
                                label_col = lookup_table_dep_label_col,
                                label_pattern = label_pattern)
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
    lookup_table_dep_label_col = NULL,
    label_pattern = "[%s]",
    table.width = 14,
    first.col.width = 3
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
            lookup_table_dep_label_col = lookup_table_dep_label_col,
            label_pattern = label_pattern,
            table.width = table.width,
            first.col.width = first.col.width
        )
    ) ->
        o

    return(o)
}



