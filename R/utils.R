make_labels <- function(
    vars = NULL,
    lookup_table = NULL,
    name_col= NULL,
    label_col= NULL,
    bareLabel = NULL
){
    .lookup <- (data.table(lookup_table, key = name_col) %>>% unique)[vars]

    .lookup %>>%
    plyr::alply(1, as.list) -> .lookup2

    rex <- .lookup2 %>>% list.map(get(name_col) %>>% as.character) %>>% unlist

    if (bareLabel == TRUE){
        names(x) <-
            sprintf(
                "%s",
                .lookup2 %>>% list.map(get(label_col) %>>% as.character) %>>% unlist
            )
    } else {
        names(x) <-
            sprintf(
                "%s; Code: %s",
                .lookup2 %>>% list.map(get(label_col) %>>% as.character) %>>% unlist,
                x
            )
    }

    x <- x[!is.na(x)]

    return(x)
}

regression_labels <- function(
    vars,
    lookup_table,
    bareLabel = TRUE
){
    vars %>>%
    .makeLabels(lookup_table= lookup_table,
                nameCol = "name",
                labelCol = 'label',
                bareLabel = bareLabel) %>>%
    names %>>%
    gsub(pattern = "&amp;",
         replacement = " and ") %>>%
    gsub(pattern = "US$;",
         replacement = "USD") ->
             o

    o[o == "NA"] <- vars[o == "NA"]

    return(o)
}
