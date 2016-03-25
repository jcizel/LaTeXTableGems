require(rtable)
library(data.table)
library(rlist)
library(pipeR)
library(readxl)
library(stringr)


DIR.DATA <- '/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/THESIS'
FILEXL <- '/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/THESIS/TABLES Part 3 -- 2016-03-08.xlsx'

sanitizer <- function(str){
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



read_excel(FILEXL, sheet = "tab1",skip=0,col_names = FALSE) %>>%
    data.table ->
    data

(data %>>% NCOL) - 1 ->
    par.ncol
       
data %>>%
    lapply(function(c){
        c %>>%
            as.character ->
            c
        c[is.na(c)] <- ""
        c %>>%
            sanitizer
    }) %>>%
     data.frame ->
     data

data %>>%
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
template <- readLines(system.file('./latex_templates/latex_table_inner.tex',package = 'rtable'))

list(
    caption = caption,
    textsize = textsize,
    content = table_content,
    colwidth = colwidth,
    ncol = par.ncol
    label = label,
    notes = notes
) ->
    info

whisker.render(template, dt) %>>%
    writeLines(con = sprintf(fmt = "%s", outfile))

cat(sprintf(fmt = "File written to: %s.\n", outfile))




