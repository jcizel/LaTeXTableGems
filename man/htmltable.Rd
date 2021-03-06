% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html_scaffolders.R
\name{htmltable}
\alias{htmltable}
\title{Produce a HTML table from a list of regression objects}
\usage{
htmltable(obj_list = NULL, digits = 3, head = TRUE, model_num = TRUE,
  stars = TRUE, print = TRUE, drop_coef = NULL, keep_coef = NULL,
  order_coef = NULL, drop_stats = NULL, keep_stats = NULL,
  order_stats = NULL, label_coef = FALSE, lookup_table_coef = NULL,
  lookup_table_coef_name_col = NULL, lookup_table_coef_label_col = NULL,
  label_stats = FALSE, lookup_table_stats = NULL,
  lookup_table_stats_name_col = NULL, lookup_table_stats_label_col = NULL,
  label_dep = label_coef, lookup_table_dep = lookup_table_coef,
  lookup_table_dep_name_col = lookup_table_coef_name_col,
  lookup_table_dep_label_col = lookup_table_coef_label_col, cellpadding = 1,
  cellspacing = 1)
}
\arguments{
\item{obj_list}{a list of regression objects}

\item{digits}{number of digits to present in result table}

\item{stars}{}

\item{print}{render the resulting table?}
}
\value{
html table
}
\description{
Produce a HTML table from a list of regression objects
}
\author{
Janko Cizel
}

