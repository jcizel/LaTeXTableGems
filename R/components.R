## PATTERNS


## HTMLCOMPONENTS

htmlcomp <- function(){    
    comment <- function(str) "<!--%s-->\n" %>>% sprintf(str)
    table <- function(str) "<table %s>\n %s </table>\n" %>>% sprintf(str)
    environment <- function(str) ""
    tabular <- function(str) ""
    size <- function(str) ""
    label <- function(name, str) "<a name=%s></a>\n" %>>% sprintf(name,str)
    caption <- function(align, str) "<caption align=\"%s\"> %s </caption>\n" %>>% sprintf(align,str)
    row <- function(str) "<tr> %s </tr>\n" %>>% sprintf(str)  
    th <- function(str) "<th> %s </th>\n" %>>% sprintf(str)      
    sth <- function() "</th><th>"
    td <- function(align, str) "<caption align=\"%s\"> %s </caption>\n" %>>% sprintf(align,str)
    
    list(
        comment = comment,
        table = table,
        environment = environment,
        tabular = tabular,
        size = size,
        label = label,
        caption = caption,
        row = row,
        th = th,
        sth = sth
        ## td1 = td1,
        ## td2 = td2,
        ## td3 = td3
    )
}



