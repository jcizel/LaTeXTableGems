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
