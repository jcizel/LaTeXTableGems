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





