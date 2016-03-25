
##' @title Convert simple data frames into nice latex tables (Primarily useful
##' for summary stat tables) 
##' @param data 
##' @param digits 
##' @param head 
##' @param table.width 
##' @param position 
##' @param print 
##' @param caption 
##' @param label 
##' @param add.rows 
##' @param notes 
##' @param textsize 
##' @param outfile 
##' @return Function primarily used for its side-effect: the latex table 
##' @author Janko Cizel
##' @export
latextable_basic <- function(
    data = NULL,
    digits = 2,
    head = TRUE,
    table.width = 14,                    #cm
    first.col.width = 3,
    position = "C",    
    print = TRUE,
    caption = "Test table",
    label = "test.table",
    add.rows = NULL,
    notes = "Test note.",
    textsize = 'scriptsize',
    outfile = NULL
){   
    N = NCOL(data) 

    data %>>% (rtable:::latextable_inner(.)) ->
        contents
    
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
    



    ## ---------------------------------------------------------------------- ##
    ## Define head                                                            ##
    ## ---------------------------------------------------------------------- ##    
    data %>>%
    names %>>%
    (rtable:::latextable_inner(.)) ->
        h

    
   c(
       h,
       "\\midrule",
       contents
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
    template <- readLines(system.file('./latex_templates/latex_table_2.tex',package = 'rtable'))

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
    
    message(sprintf(fmt = "File written to: %s.\n", outfile))

    return(invisible(whisker.render(template, dt)))
}
