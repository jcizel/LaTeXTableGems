##' Extract desired stats from R estimation object and return result in data.table
##'
##
##' @param obj 
##' @param select 
##' @param digits 
##' @param stars 
##' @return data.table
##' @author Janko Cizel
##'
##' @export
##'
extract_selected <- function(obj,...){
    UseMethod('extract_selected')
}

extract_selected.felm <- function(
    obj,
    select = c('beta','se','pval'),
    digits = 3,
    stars = FALSE,
    type = 'html'                       #'html' or 'latex'
){
    obj[select] %>>%
    list.map({
        o = .
        name = .name
        
        o  %>>%
        (
            data.table(
                id = names(.),
                value = . %>>% as.numeric
                ## key = 'id'
            )
        ) %>>%
        mutate(
            value = value %>>% round(digits = digits)
        ) %>>%
        setnames(
            old = 'value',
            new = name
        )
    }) %>>%
    Reduce(
        f = function(...){
            merge.data.frame(..., by = 'id', all = TRUE, sort = FALSE)
        }
    ) %>>% as.data.table -> o

    ## Stars
    if (stars == TRUE){
        if (!'pval' %in% names(o))
            stop('`pval` must be included in `select` list in order to produce significance stars')
        
        o %>>%
        apply(1, function(r){
            if (r[['pval']] <= 0.01)
                sprintf(star_patterns[[type]],
                        r[['beta']],
                        star_signif[3])
            else if ((r[['pval']] > 0.01) & (r[['pval']] <= 0.05))
                sprintf(star_patterns[[type]],
                        r[['beta']],
                        star_signif[2]) 
            else if ((r[['pval']] > 0.05) & (r[['pval']] <= 0.10))
                sprintf(star_patterns[[type]],
                        r[['beta']],
                        star_signif[1]) 
            else
                sprintf("%s",
                        r[['beta']]) 
        }) ->
            beta_char

        o[, beta.star := beta_char]
    }

    ## Format standard errors
    o %>>%
    apply(1, function(r){
        sprintf(
            "(%s)   ",
            r[['se']]            
        )
    }) ->
        se_char

    o[, se.fmt := se_char]


    ## Final steps
    select <- c('beta.star','se.fmt',select)
    
    o %>>%
    melt.data.table(id.vars = 'id') %>>%
    mutate(
        variable = factor(variable, levels = select)
    ) %>>% 
    arrange(
        id,
        variable
    )    
}


extract_selected.lm <- function(
    obj,
    select = c('beta','se','pval'),
    digits = 3,
    stars = FALSE,
    type = 'html'                       #'html' or 'latex'
){
    obj %>>%
    summary %>>% 
    (coefficients) %>>% as.data.frame %>>%
    (mutate(
        .,
        id = rownames(.)
    )) %>>% as.data.table -> o

    o[, 'beta' := Estimate]

    ## Stars
    if (stars == TRUE){        
        o %>>%
        select(
            - id
        ) %>>%
        apply(1, function(r){
            str(r)
            if (r['Pr(>|t|)']  <= 0.01)
                sprintf(star_patterns[[type]],
                        r['Estimate'] %>>% round(digits = digits),
                        star_signif[3]) 
            else if ((r['Pr(>|t|)']  > 0.01) & (r['Pr(>|t|)']  <= 0.05))
                sprintf(star_patterns[[type]],
                        r['Estimate'] %>>% round(digits = digits),
                        star_signif[2]) 
            else if ((r['Pr(>|t|)']  > 0.05) & (r['Pr(>|t|)']  <= 0.10))
                sprintf(star_patterns[[type]],
                        r['Estimate'] %>>% round(digits = digits),
                        star_signif[1]) 
            else
                sprintf("%s",
                        r['Estimate']) 
        }) ->
            beta_char

        o[, beta.star := beta_char]
    } else {
        o[, beta.star := beta %>>% round(digits = digits)]
    }

    ## Format standard errors
    o[['Std. Error']] %>>%
    round(digits = digits) %>>%
    sprintf(
        fmt = "(%s)   "
    ) -> 
        se_char

    o[, se.fmt := se_char]


    ## Final steps
    select <- c('beta.star','se.fmt')
    
    o %>>%
    melt.data.table(id.vars = 'id') %>>%
    mutate(
        variable = factor(variable, levels = select)
    ) %>>% 
    arrange(
        id,
        variable
    ) %>>%
    subset(
        variable %in% select
    )
}
