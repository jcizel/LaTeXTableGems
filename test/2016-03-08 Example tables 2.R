require(rtable)
library(data.table)
library(rlist)
library(pipeR)
library(readxl)
library(stringr)


DIR.DATA <- '/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/THESIS'
FILEXL <- '/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/THESIS/TABLES Part 3 -- 2016-03-08.xlsx'


outfile = '/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/THESIS/PhD Thesis -- Latex/memoir/tables/part3/test1.tex'

excel2latex(
    file = FILEXL,
    outfile = outfile,
    escaperows = 1,
    escapecols = 1,
    sheet = 'tab8d',
    skip = 0,
    colwidth = 1.2,
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
