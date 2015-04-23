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
    digits = 2,
    head = TRUE,
    coef.presentation = 'T1',
    table.width = 14,                    #cm
    first.col.width = 3,
    position = "C",    
    model_num = TRUE,
    stars = TRUE,
    print = TRUE,
    caption = "Test table",
    label = "test.table",
    add.rows = NULL,
    notes = "Test note.",
    textsize = 'scriptsize',
    outfile = NULL,
    drop_coef = NULL,
    keep_coef = NULL,
    order_coef = NULL,    
    drop_stats = NULL,
    keep_stats = NULL,
    order_stats = NULL,            
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
    label_pattern = "[%s]"
){   
    N = length(obj_list) + 1

    parse_result_list_coef(obj_list,
                           digits = digits,
                           stars = stars,
                           type = 'latex',
                           coef.presentation = coef.presentation,
                           label_coef = label_coef,
                           drop_coef = drop_coef,
                           keep_coef = keep_coef,
                           order_coef = order_coef,
                           lookup_table_coef = lookup_table_coef,
                           lookup_table_coef_name_col = lookup_table_coef_name_col,
                           lookup_table_coef_label_col = lookup_table_coef_label_col,
                           label_pattern = label_pattern
                           ) ->
                               coefs

    parse_result_list_static(obj_list,
                             digits = digits,
                             stars = stars,
                             type = 'latex',
                             drop_stats = drop_stats,
                             keep_stats = keep_stats,
                             order_stats = order_stats,                             
                             label_stats = label_stats,
                             lookup_table_stats = lookup_table_stats,
                             lookup_table_stats_name_col = lookup_table_stats_name_col,
                             lookup_table_stats_label_col = lookup_table_stats_label_col,
                             label_pattern = label_pattern
                             ) ->
                                 stats
    
    ## ---------------------------------------------------------------------- ##
    ## Define widths                                                          ##
    ## ---------------------------------------------------------------------- ##
    col.width = (table.width - first.col.width) / (N-1)

    1:N %>>%
    list.map({
        if (.i == 1){
            sprintf(
                fmt = "P{%scm}",
                first.col.width %>>% round(0)
            )
        } else {
            sprintf(
                fmt = "%s{%scm}",
                position,
                col.width %>>% round(0)                
            )
        }    
    }) %>>%
    paste(collapse = "") %>>%
    sprintf(fmt = "%s") ->
        table.widths
    
    ## computeColWidth(
    ##     data = list(coefs,stats) %>>% rbindlist,
    ##     digits = digits
    ## ) %>>%
    ## list.map({
    ##     if (.i == 1){
    ##         sprintf(
    ##             fmt = "P{%scm}",
    ##             table.width * . 
    ##         )
    ##     } else {
    ##         sprintf(
    ##             fmt = "%s{%scm}",
    ##             position,
    ##             table.width * . 
    ##         )
    ##     }    
    ## }) %>>%
    ## paste(collapse = "") %>>%
    ## sprintf(fmt = "%s") ->
    ##     table.widths


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
            lookup_table_dep_label_col = lookup_table_dep_label_col,
            label_pattern = label_pattern,
            table.width = table.width,
            first.col.width = first.col.width
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


    ## Additional rows
    if (!is.null(add.rows)){
        add.rows %>>%
        veclist2latex %>>%
        sprintf(fmt = "\\\\\n\\multicolumn{2}{l}{\\textbf{Additional information:}}\\\\ \n%s")->
            add.rows
    } else {
        add.rows <- ""
    }

    ## ---------------------------------------------------------------------- ##
    ## OUTPUT FILE                                                            ##
    ## ---------------------------------------------------------------------- ##
    template <- readLines("./inst/latex_templates/latex_table_2.tex")

    list(
        caption = caption,
        textsize = textsize,
        widths = table.widths,
        content = table_content,
        label = label,
        notes = notes,
        addrows = add.rows
    ) ->
        dt

    whisker.render(template, dt) %>>%
    writeLines(con = sprintf(fmt = "%s", outfile))
    
    cat(sprintf(fmt = "File written to: %s.\n", outfile))

    return(invisible(whisker.render(template, dt)))
}
