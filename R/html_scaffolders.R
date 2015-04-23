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
                              keep_coef = keep_coef,
                              order_coef = order_coef,                              
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
                                keep_stats = keep_stats,
                                order_stats = order_stats,
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

