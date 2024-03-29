% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lib_table.R
\name{move_data_columns_right}
\alias{move_data_columns_right}
\title{rearrange the data.table columns in place so index columns are put to the left, 
and data columns are put to the right}
\usage{
move_data_columns_right(dt, data_col_pattern)
}
\arguments{
\item{dt}{data.table}

\item{data_col_pattern}{regex to match data col names}
}
\value{
dt modified in place by setcolorder
}
\description{
rearrange the data.table columns in place so index columns are put to the left, 
and data columns are put to the right
}
