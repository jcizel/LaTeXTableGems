require(rtable)
library(data.table)
library(rlist)
library(pipeR)
library(readxl)
library(stringr)
require(whisker)

DIR.DATA <- '/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/THESIS'

FILEXL <- '/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/THESIS/2016 Basel Paper/tables/2016-03-13 Tables.xlsx'

outfile = '/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/THESIS/PhD Thesis -- Latex/memoir/tables/basel/test1.tex'

excel2latex(
    file = FILEXL,
    outfile = outfile,
    escaperows = 1:2,
    escapecols = 1,
    sheet = 'summary',
    skip = 0,
    colwidth = 1.8,
    type = 'number'
)

excel2latex(
    file = FILEXL,
    outfile = outfile,
    escaperows = 1,
    sheet = 'tab2',
    skip = 0,
    colwidth = 3,
    type = 'text'
)
