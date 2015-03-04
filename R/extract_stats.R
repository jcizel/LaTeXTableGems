extract_stats <- function(obj, ...){
    UseMethod('extract_stats')
}

extract_stats.default <- function(
    obj = NULL,
    digits =3
){
    obj %>>%
    summary %>>%
    list.filter(
        length(.) == 1 &
            !is.list(.)
    ) %>>%
    list.map({
        x = .

        if (!is.integer(x) & !is.logical(x) & is.numeric(x))
            x %>>% round(digits)
        else
            x
    }) %>>%
    ({
        data.table(
            id = names(.),
            value = sprintf("%s", .)
        )
    }) 
}


extract_stats.lm <- function(
    obj = NULL,
    digits =3
){
    obj %>>%
    summary %>>%
    ({
        list(
            data.table(
                id = 'sigma',
                value = .[['sigma']] %>>% round(digits = digits)
            ),
            data.table(
                id = 'r.squared',
                value = .[['r.squared']] %>>% round(digits = digits)
            ),
            data.table(
                id = 'adj.r.squared',
                value = .[['adj.r.squared']] %>>% round(digits = digits)
            ),
            data.table(
                id = 'fstatistic',
                value = .[['fstatistic']] %>>%
                (sprintf(
                    fmt = "F(%s,%s)=%s",
                    .[[2L]],
                    .[[3L]],
                    .[[1L]] %>>% round(digits = digits)
                ))   
            ),
            data.table(
                id = 'N',
                value = .[['df']] %>>%
                (.[[1L]] + .[[2L]]) %>>%
                round(digits = digits)
            )
        ) %>>%
        rbindlist
    })    
}
