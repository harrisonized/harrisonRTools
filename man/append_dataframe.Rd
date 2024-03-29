% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_tools.R
\name{append_dataframe}
\alias{append_dataframe}
\title{Appends df1 with df2}
\usage{
append_dataframe(df1, df2, infront = FALSE, reset_index = TRUE)
}
\description{
Use this to avoids errors like this one:

Can't combine \verb{..1$1} \if{html}{\out{<character>}} and \verb{..2$1} \if{html}{\out{<double>}}.

If you combine dataframes with different types, the data gets coerced to \code{character}.
}
