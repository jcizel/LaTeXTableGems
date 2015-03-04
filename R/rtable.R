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
