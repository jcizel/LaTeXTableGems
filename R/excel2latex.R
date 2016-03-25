sanitizexl <- function(str){
    str %>>%
        ## gsub(pattern = "\\\\", replacement = "SANITIZE.BACKSLASH") %>>%
        ## gsub(pattern = "$", replacement = "\\$", fixed = TRUE) %>>%
        gsub(pattern = ">", replacement = "$>$", fixed = TRUE) %>>%
        gsub(pattern = "<", replacement = "$<$", fixed = TRUE) %>>%
        gsub(pattern = "|", replacement = "$|$", fixed = TRUE) %>>%
        ## gsub(pattern = "{", replacement = "\\{", fixed = TRUE) %>>%
        ## gsub(pattern = "}", replacement = "\\}", fixed = TRUE) %>>%
        gsub(pattern = "%", replacement = "\\%", fixed = TRUE) %>>%
        gsub(pattern = "&", replacement = "\\&", fixed = TRUE) %>>%
        gsub(pattern = "_", replacement = "\\_", fixed = TRUE) %>>%
        gsub(pattern = "#", replacement = "\\#", fixed = TRUE) %>>%
        ## gsub(pattern = "^", replacement = "\\verb|^|", fixed = TRUE) %>>%
        gsub(pattern = "~", replacement = "\\~{}", fixed = TRUE) %>>%
        gsub(pattern = "SANITIZE.BACKSLASH", replacement = "$\\backslash$", fixed = TRUE) ->
        result    
    return(result)
}

#' @export
excel2latex <-
    function(
             file,
             outfile,
             sheet = 1,
             skip = 0,
             colwidth = 1.2,
             escaperows = NULL,
             escapecols = NULL,
             caption = '',
             textsize = 'scriptsize',
             label = 'table',
             notes = '',
             type = c('number')
             ){
        read_excel(file, sheet = sheet, skip=skip, col_names = FALSE) %>>%
            data.frame ->
            data

        par.ncol <- (data %>>% NCOL) - 1

        if (!is.null(escaperows)){
            for (r in escaperows){
                if (type == 'number'){
                    data[r,] %>>%
                        sapply(function(c){
                            c[is.na(c)] <- ""
                            sprintf(
                                fmt = "{\\multicolumn{1}{C{%s}}{%s}}",
                                ## fmt = "{\\shortstack{%s}}"
                                colwidth,
                                c
                            )
                        }) %>>% as.character ->
                         new
                    
                    data[r,] <- new
                } else {
                    data[r,] %>>%
                        sapply(function(c){
                            c[is.na(c)] <- ""
                            c %>>%
                                sprintf(
                                    fmt = "\\multicolumn{1}{c}{%s}"
                                )
                        }) %>>% as.character ->
                         new
                    
                    data[r,] <- new
                }
            }
        }

        if (!is.null(escapecols)){
            for (c in escapecols){
                data[,c] %>>%
                    sapply(function(r){
                        r[is.na(r)] <- ""
                        r %>>%
                            sprintf(
                                fmt = "{%s}"
                            )
                    }) %>>% as.character ->
                     new

                data[,c] <- new
            }
        }
        
        data %>>%
            lapply(function(c){
                c %>>%
                    as.character ->
                    c
                c[is.na(c)] <- ""
                c %>>%
                    sanitizer
            }) %>>%
             data.frame %>>%
             apply(1,
                   function(r){
                       r %>>%
                           paste(
                               collapse = " & "
                           ) %>>%
                           sprintf(
                               fmt = "%s \\\\"
                           )
                   }) %>>%
             paste(
                 collapse = "\n"
             ) ->
             CONTENT

        ## ---------------------------------------------------------------------- ##
        ## OUTPUT FILE                                                            ##
        ## ---------------------------------------------------------------------- ##
        if (type == 'number'){
            template <- readLines(system.file('./latex_templates/latex_table_numbers.tex',package = 'rtable'))            
        } else {
            template <- readLines(system.file('./latex_templates/latex_table_text.tex',package = 'rtable'))
        }


        list(
            caption = caption,
            textsize = textsize,
            content = CONTENT,
            colwidth = colwidth,
            ncol = par.ncol,
            label = label,
            notes = notes
        ) ->
            info

        whisker.render(template, info) %>>%
            writeLines(con = sprintf(fmt = "%s", outfile))

        cat(sprintf(fmt = "File written to: %s.\n", outfile))

        return((whisker.render(template, info)))        
        
    }
