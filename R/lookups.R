## Lookup table for stats

lookup_stats <-
    list(
        data.table(
            code = 'N',
            label = 'Observations'
        ),
        data.table(
            code = 'p',
            label = 'Number of parameters'
        ),
        data.table(
            code = 'fstat',
            label = 'F-stat'
        ),
        data.table(
            code = 'pval',
            label = 'P-val of F-stat'
        ),
        data.table(
            code = 'r2',
            label= "R$^2$"
        ),
        data.table(
            code = 'r2adj',
            label = 'Adj. R$^2$'
        ),
        data.table(
            code = 'rdf',
            label = 'Degrees of freedom'
        ),
        data.table(
            code = 'rse',
            label = 'Root Squared Error'
        )
    ) %>>% rbindlist
