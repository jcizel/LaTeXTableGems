sanitizer <- function(str, type = 'latex'){
    if (type == 'latex'){
        str %>>%
        gsub("\\\\", "SANITIZE.BACKSLASH", result)
        gsub("$", "\\$", fixed = TRUE) %>>%
        gsub(">", "$>$", fixed = TRUE) %>>%
        gsub("<", "$<$", fixed = TRUE) %>>%
        gsub("|", "$|$", fixed = TRUE) %>>%
        gsub("{", "\\{", fixed = TRUE) %>>%
        gsub("}", "\\}", fixed = TRUE) %>>%
        gsub("%", "\\%", fixed = TRUE) %>>%
        gsub("&", "\\&", fixed = TRUE) %>>%
        gsub("_", "\\_", fixed = TRUE) %>>%
        gsub("#", "\\#", fixed = TRUE) %>>%
        gsub("^", "\\verb|^|", fixed = TRUE) %>>%
        gsub("~", "\\~{}", fixed = TRUE) %>>%
        gsub("SANITIZE.BACKSLASH", "$\\backslash$", fixed = TRUE) ->
            result
    } else if (type == 'html') {
        str %>>%
        gsub("&", "&amp;", fixed = TRUE)
        gsub(">", "&gt;", fixed = TRUE)
        gsub("<", "&lt;", fixed = TRUE) ->
            result
    } else {
        stop('Type must be either `latex` or `html`')
    }
    
    return(result)
}

##' Extract desired stats from R estimation object and return result in data.table
##'
##
##' @param obj 
##' @param select 
##' @param digits 
##' @param stars 
##' @return data.table
##' @author Janko Cizel
##'
##' @export
##' @import data.table pipeR rlist dplyr rprintf
extract_selected <- function(
    obj,
    select = c('beta','se','pval'),
    digits = 3,
    stars = FALSE
){
    obj[select] %>>%
    list.map({
        o = .
        name = .name
        
        o  %>>%
        (
            data.table(
                id = names(.),
                value = . %>>% as.numeric,
                key = 'id'
            )
        ) %>>%
        mutate(
            value = value %>>% round(digits = digits)
        ) %>>%
        setnames(
            old = 'value',
            new = name
        )
    }) %>>%
    Reduce(
        f = function(...){
            merge(..., by = 'id', all = TRUE)
        }
    ) -> o

    ## Stars
    if (stars == TRUE){
        if (!'pval' %in% names(o))
            stop('`pval` must be included in `select` list in order to produce significance stars')

        star_char = c(
            "$^{***}$",
            "$^{**}$",
            "$^{*}$"                        
        )
        
        o %>>%
        apply(1, function(r){
            if (r[['pval']] <= 0.01)
                sprintf("%s%s",
                        r[['beta']],
                        star_char[1])
            else if ((r[['pval']] > 0.01) & (r[['pval']] <= 0.05))
                sprintf("%s%s",
                        r[['beta']],
                        star_char[2])
            else if ((r[['pval']] > 0.05) & (r[['pval']] <= 0.10))
                sprintf("%s%s",
                        r[['beta']],
                        star_char[3])
            else
                sprintf("%s",
                        r[['beta']])
        }) ->
            beta_char

        o[, beta.star := beta_char]

    }

    ## Format standard errors
    o %>>%
    apply(1, function(r){
        sprintf(
            "(%s)",
            r[['se']]            
        )
    }) ->
        se_char

    o[, se.fmt := se_char]


    ## Final steps
    select <- c('beta.star','se.fmt',select)
    
    o %>>%
    melt.data.table(id.vars = 'id') %>>%
    mutate(
        variable = factor(variable, levels = select)
    ) %>>% 
    arrange(
        id,
        variable
    )    
}

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
##' @import data.table pipeR rlist dplyr
parse_result_list_coef<- function(
    obj_list = NULL,
    digits = 3,
    stars = TRUE
){
    ## Number each model
    names(obj_list) <- 1:length(obj_list) %>>% as.character

    select = c('beta','se','pval')
    obj_list %>>%
    list.map(
        . %>>%
        extract_selected(select = select, stars = stars, digits = digits) %>>%
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

    ## ROW NAMES
    o[, group := rleid(id)]
    o[, group.row := 1:length(id), by = group]
    o[group.row == 1,
      group.name := id]

    group_name <- o$group.name

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



extract_static_elements <- function(
    obj = NULL,
    digits =3
){
    obj %>>%
    summary %>>%
    list.filter(
        length(.) == 1 &
            !is.list(.)
    ) %>>%
    list.map({
        x = .

        if (!is.integer(x) & !is.logical(x) & is.numeric(x))
            x %>>% round(digits)
        else
            x
    }) %>>%
    ({
        data.table(
            id = names(.),
            value = sprintf("%s", .)
        )
    })
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
    digits = 3
){
    names(obj_list) <- 1:length(obj_list)
    
    obj_list %>>%
    list.map(
        . %>>%
        extract_static_elements(digits = digits) %>>%
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

    o[, lapply(.SD, function(c){
        c[is.na(c)] <- ""
        c
    })] ->
        o
    
    return(o)
}





##' Given a list of objects with regression results, return a list of strings
##' that serve as header decorations in a latex table
##'
##' 
##' @param obj_list 
##' @return NULL
##' @author Janko Cizel
##'
##' @export
parse_result_list_model_names<- function(
    obj_list = NULL
){
    obj_list %>>%
    list.map({
        . %>>%
        attr("class") ->
            model_name

        . %>>%
        attr("call") %>>%
        all.vars %>>%
        (.[[1L]]) ->
            model_depvar

        data.table(
            model_name = model_name,
            model_depvar = model_depvar
        )
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


    o[group.model.row == 1,
      group.model.name := sprintf(
                           fmt = "\\multicolumn{%s}{c}{%s}",
                           group.model.nobs,
                           model_name)]

    o[group.model.row == 1,
      group.model.line := sprintf(
                           fmt = "\\cmidrule(lr){%s-%s}",
                           group.model.min,
                           group.model.max)]    

    o[group.row == 1,
      group.name := sprintf(
                           fmt = "\\multicolumn{%s}{c}{%s}",
                           group.nobs,
                           model_depvar)]

    o[group.row == 1,
      group.line := sprintf(
                           fmt = "\\cmidrule(lr){%s-%s}",
                           group.min,
                           group.max)]


    ## Output text 
    o$group.model.name %>>%
    (
        .[!is.na(.)]
    ) %>>%
    paste(collapse = " & ") %>>%
    sprintf(
        fmt = "& %s\\\\ \n"
    ) ->
        group_model_name

    o$group.model.line %>>%
    (
        .[!is.na(.)]
    ) %>>%
    paste(collapse = " ") %>>%
    sprintf(
        fmt = "%s\\\\ \n"
    ) ->
        group_model_line

    o$group.name %>>%
    (
        .[!is.na(.)]
    ) %>>%
    paste(collapse = " & ") %>>%
    sprintf(
        fmt = "& %s\\\\ \n"
    ) ->
        group_name

    o$group.line %>>%
    (
        .[!is.na(.)]
    ) %>>%
    paste(collapse = " ") %>>%
    sprintf(
        fmt = "%s\\\\ \n"
    ) ->
        group_line
    
    return(
        list(
            group_model_name = group_model_name,
            group_model_line = group_model_line,
            group_name = group_name,
            group_line = group_line
        )
    )
}




