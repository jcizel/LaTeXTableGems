htmltable_inner <- function(data){
    data %>>%
    apply(1,
          function(r){
              r %>>%
              list.map({
                  e = .
                  
                  if (.i == 1)
                      tags$td(e %>>% HTML, style = 'text-align:left')
                  else 
                      tags$td(e %>>% HTML)                     
              }) ->
                  o

              names(o) <- NULL

              return(tags$tr(o))
          }) ->
              out
    return(out)
}



##' Produce a HTML table from a list of regression objects
##' 
##' @param obj_list a list of regression objects
##' @param digits number of digits to present in result table
##' @param stars 
##' @param print render the resulting table?
##' @return html table
##' @author Janko Cizel
##' 
##' @export
htmltable <- function(
    obj_list = NULL,
    digits = 3,
    head = TRUE,
    model_num = TRUE,
    stars = TRUE,
    print = TRUE,
    drop_coef = NULL,
    drop_stats = NULL,
    label_coef = FALSE,
    lookup_table_coef = NULL,
    lookup_table_coef_name_col = NULL,
    lookup_table_coef_label_col = NULL,
    label_stats = FALSE,
    lookup_table_stats = NULL,
    lookup_table_stats_name_col = NULL,
    lookup_table_stats_label_col = NULL,
    label_dep = label_coef,
    lookup_table_dep = lookup_table_coef,
    lookup_table_dep_name_col = lookup_table_coef_name_col,
    lookup_table_dep_label_col = lookup_table_coef_label_col,
    cellpadding = 1,
    cellspacing = 1
){   
    N = length(obj_list) + 1

    if (head == TRUE){
        h = header(
            obj_list,
            type = 'html',
            label_dep = label_dep,
            lookup_table_dep = lookup_table_dep,
            lookup_table_dep_name_col = lookup_table_dep_name_col,
            lookup_table_dep_label_col = lookup_table_dep_label_col            
        )
        
        list(h[[1L]]$text,
             h[[1L]]$line,
             h[[2L]]$text,
             h[[2L]]$line) ->
                 h
    } else {
        h = NULL
    }

    if (model_num == TRUE){
        1:(N-1) %>>%
        sprintf(fmt = '(%s)') %>>%
        list.map(tags$td(.)) ->
            m

        names(m) <- NULL
        
        tags$tr(tags$td(""),m) ->
            m        
    } else {
        m = NULL
    }
    
   c(
       style = 'text-align:center',
       cellpadding = cellpadding,
       cellspacing = cellspacing,
       list(tags$tr(tags$td(colspan = N, style = 'border-bottom: 1px solid black'))),
       h,
       list(m),
       list(tags$tr(tags$td(colspan = N, style = 'border-bottom: 1px solid black'))),       
       parse_result_list_coef(obj_list,
                              digits = digits,
                              stars = stars,
                              type = 'html',
                              label_coef = label_coef,
                              drop_coef = drop_coef,
                              lookup_table_coef = lookup_table_coef,
                              lookup_table_coef_name_col = lookup_table_coef_name_col,
                              lookup_table_coef_label_col = lookup_table_coef_label_col
                              ) %>>% htmltable_inner,
       list(tags$tr(tags$td(colspan = N, style = 'border-bottom: 1px solid black'))),
       parse_result_list_static(obj_list,
                                digits = digits,
                                stars = stars,
                                type = 'html',
                                drop_stats = drop_stats,
                                label_stats = label_stats,
                                lookup_table_stats = lookup_table_stats,
                                lookup_table_stats_name_col = lookup_table_stats_name_col,
                                lookup_table_stats_label_col = lookup_table_stats_label_col
                                ) %>>% htmltable_inner,
       list(tags$tr(tags$td(colspan = N, style = 'border-bottom: 1px solid black')))
    ) ->
        o

    do.call(
        tags$table,
        o
    ) ->
        tab
    
    if (print == TRUE){
        html_print(tab)
    }
    
    return(tab)
}

##' Scaffold LaTeX table from a data.frame
##'
##' 
##' @param data 
##' @param header 
##' @param table.width 
##' @param position 
##' @param caption 
##' @param label 
##' @param notes 
##' @param outfile 
##' @return NULL
##' @author Janko Cizel
##'
##' @export
##' @import data.table pipeR rlist dplyr
scaffoldTable <- function(
    data = NULL,
    type = 'latex',
    header = NULL,
    table.width = 14,                    #cm
    digits = 2,
    position = "C",
    caption = "Test table",
    label = "test.table",
    notes = "Test note.",
    outfile = NULL
){

    NCOL = ncol(data)

    ## ---------------------------------------------------------------------- ##
    ## Define widths                                                          ##
    ## ---------------------------------------------------------------------- ##
    computeColWidth(
        data = data,
        digits = digits
    ) %>>%
    list.map(
        sprintf(
            fmt = "%s{%scm}",
            position,
            table.width * . 
        )
    ) %>>%
    paste(collapse = "") %>>%
    sprintf(fmt = "%s") ->
        table.widths   

    ## ---------------------------------------------------------------------- ##
    ## Table header                                                           ##
    ## ---------------------------------------------------------------------- ##
    if (is.null(header)){
        data %>>%
        names %>>%
        paste(collapse = " & ") %>>%
        sprintf(fmt = "%s\\\\\n\\midrule") ->
            table.header
    } else {
        c(
            header,
            data %>>%
            names %>>%
            paste(collapse = " & ") %>>%
            sprintf(fmt = "%s\\\\\n\\midrule")
        ) %>>%
        paste(collapse = "\n")->
            table.header
    }

    ## ---------------------------------------------------------------------- ##
    ## OUTPUT FILE                                                            ##
    ## ---------------------------------------------------------------------- ##
    rprintf(x = "
\\begin{table}

\\centering

\\begin{threeparttable}

\\caption{$caption}

\\scriptsize

\\begin{tabular}{$widths}

\\toprule

$header

$content

\\addlinespace[.75ex]
\\bottomrule

\\end{tabular}

\\label{tab:$label}

\\begin{tablenotes}
\\scriptsize
\\item Notes:
$notes
\\end{tablenotes}

\\end{threeparttable}

\\end{table}
",
            caption = caption,
            widths = table.widths,
            header = table.header,
            content = latextable_inner(data),
            label = label,
            notes = notes) ->
                out

    if (!is.null(outfile)){
        sink(outfile)
        cat(out)
        sink()

        cat(sprintf("The table header was written to the file '%s'.\n", outfile))
    }
    
    return(out)
}

##' Scaffold boilerplate for LaTeX figure 
##'
##' 
##' @param figList 
##' @param fig.dir 
##' @param caption 
##' @param label 
##' @return NULL
##' @author Janko Cizel
##'
##' @export
scaffoldFigure <- function(
    figList = NULL,
    fig.dir = './FIG',
    caption = 'Test',
    label = 'Test'
){
    if (length(figList) > 1){
        figList %>>%
        list.map({
            rprintf(
                "\\begin{subfigure}[t]{$ratio\\linewidth}
\\includegraphics[width=\\linewidth]{$path}
\\caption{$caption}
\\label{fig:$label}
\\end{subfigure}
",
                caption = .[['caption']],
                path = sprintf("%s/%s",fig.dir,.[['path']]),
                label = .[['path']],
                ratio = .[['ratio']]
            )

        }) %>>%
        unlist %>>%
        paste(collapse = "\n") ->
            inner
    } else {
        figList %>>%
        list.map({
            rprintf(
                "\\includegraphics[width=$ratio\\linewidth]{$path}",
                path = sprintf("%s/%s",fig.dir,.[['path']]),
                ratio = .[['ratio']]
            )
        }) %>>%
        unlist %>>%
        paste(collapse = "\n") ->
            inner
    }

    rprintf("
\\begin{figure*}[t!]
\\centering
\\caption{$caption}
$inner
\\label{$label}
\\end{figure*}
",
            inner = inner,
            caption = caption,
            label = label
            ) ->
                out

    return(out)
}

latextable_inner <- function(
    data = NULL,
    digits = 2
){
    data %>>%
    list.map({
        . %>>% stringify
    }) %>>%
    as.data.frame %>>%
    xtable %>>%
    ({
        capture.output({
            print(
                x = .,
                only.contents = TRUE,
                include.rownames = FALSE,
                include.colnames = FALSE,
                hline = NULL,
                sanitize.text.function = sanitizer
            )
        })
    }) %>>%
    (.[-c(1:2)]) %>>%
    paste(collapse = "\n")   
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






latextable <- function(
    obj_list = NULL,
    digits = 3,
    head = TRUE,
    table.width = 14,                    #cm
    position = "C",    
    model_num = TRUE,
    stars = TRUE,
    print = TRUE,
    caption = "Test table",
    label = "test.table",
    notes = "Test note.",
    textsize = 'scriptsize',
    outfile = NULL,    
    drop_coef = NULL,
    drop_stats = NULL,
    label_coef = FALSE,
    lookup_table_coef = NULL,
    lookup_table_coef_name_col = NULL,
    lookup_table_coef_label_col = NULL,
    label_stats = FALSE,
    lookup_table_stats = NULL,
    lookup_table_stats_name_col = NULL,
    lookup_table_stats_label_col = NULL,
    label_dep = label_coef,
    lookup_table_dep = lookup_table_coef,
    lookup_table_dep_name_col = lookup_table_coef_name_col,
    lookup_table_dep_label_col = lookup_table_coef_label_col
){   
    N = length(obj_list) + 1

    parse_result_list_coef(obj_list,
                           digits = digits,
                           stars = stars,
                           type = 'latex',
                           label_coef = label_coef,
                           drop_coef = drop_coef,
                           lookup_table_coef = lookup_table_coef,
                           lookup_table_coef_name_col = lookup_table_coef_name_col,
                           lookup_table_coef_label_col = lookup_table_coef_label_col
                           ) ->
                               coefs

    parse_result_list_static(obj_list,
                             digits = digits,
                             stars = stars,
                             type = 'latex',
                             drop_stats = drop_stats,
                             label_stats = label_stats,
                             lookup_table_stats = lookup_table_stats,
                             lookup_table_stats_name_col = lookup_table_stats_name_col,
                             lookup_table_stats_label_col = lookup_table_stats_label_col
                             ) ->
                                 stats
    
    ## ---------------------------------------------------------------------- ##
    ## Define widths                                                          ##
    ## ---------------------------------------------------------------------- ##
    computeColWidth(
        data = list(coefs,stats) %>>% rbindlist,
        digits = digits
    ) %>>%
    list.map({
        if (.i == 1){
            sprintf(
                fmt = "P{%scm}",
                table.width * . 
            )
        } else {
            sprintf(
                fmt = "%s{%scm}",
                position,
                table.width * . 
            )
        }    
    }) %>>%
    paste(collapse = "") %>>%
    sprintf(fmt = "%s") ->
        table.widths


    ## ---------------------------------------------------------------------- ##
    ## Define head                                                            ##
    ## ---------------------------------------------------------------------- ##    
    if (head == TRUE){
        h = header(
            obj_list,
            type = 'latex',
            label_dep = label_dep,
            lookup_table_dep = lookup_table_dep,
            lookup_table_dep_name_col = lookup_table_dep_name_col,
            lookup_table_dep_label_col = lookup_table_dep_label_col            
        )
        
        list(h[[1L]]$text,
             h[[1L]]$line,
             h[[2L]]$text,
             h[[2L]]$line) ->
                 h
    } else {
        h = NULL
    }

    if (model_num == TRUE){
        1:(N-1) %>>%
        sprintf(fmt = '(%s)') %>>%
        paste(collapse = " & ") %>>%
        sprintf(fmt = "& %s \\\\") ->
            m
    } else {
        m = NULL
    }
    
   c(
       h %>>% unlist,
       m,
       "\\midrule",
       coefs %>>% latextable_inner,
      "\\midrule",
       stats %>>% latextable_inner
    ) ->
        o

    o %>>%
    paste(collapse = "\n") ->
        table_content


    ## ---------------------------------------------------------------------- ##
    ## OUTPUT FILE                                                            ##
    ## ---------------------------------------------------------------------- ##
    rprintf(x = "
\\newcolumntype{P}[1]{>{\\raggedright\\arraybackslash}p{#1}}
\\newcolumntype{C}[1]{>{\\centering\\arraybackslash}p{#1}}
\\newcolumntype{R}[1]{>{\\raggedleft\\arraybackslash}p{#1}}

\\begin{table}

\\centering

\\begin{threeparttable}

\\caption{$caption}

\\$textsize

\\begin{tabular}{$widths}

\\toprule

$content

\\addlinespace[.75ex]
\\bottomrule

\\end{tabular}

\\label{tab:$label}

\\begin{tablenotes}
\\scriptsize
\\item Notes:
$notes
\\end{tablenotes}

\\end{threeparttable}

\\end{table}
",
            caption = caption,
            textsize = textsize,
            widths = table.widths,
            content = table_content,
            label = label,
            notes = notes) ->
                out

    if (!is.null(outfile)){
        sink(outfile)
        cat(out)
        sink()

        cat(sprintf("The table header was written to the file '%s'.\n", outfile))
    }


    return(out)
}


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
