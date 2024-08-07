% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-optimal-bin-size.R
\name{opt_bin}
\alias{opt_bin}
\title{Get the optimal binwidth for a histogram}
\usage{
opt_bin(.data, .value_col, .iters = 30)
}
\arguments{
\item{.data}{The data set in question}

\item{.value_col}{The column that holds the values}

\item{.iters}{How many times the cost function loop should run}
}
\value{
A tibble of histogram breakpoints
}
\description{
Gives the optimal binwidth for a histogram given a data set, it's value and
the desired amount of bins
}
\details{
\itemize{
\item Supply a data.frame/tibble with a value column. from this an optimal binwidth
will be computed for the amount of binds desired
}
}
\examples{

suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))

df_tbl <- rnorm(n = 1000, mean = 0, sd = 1)
df_tbl <- df_tbl \%>\%
  as_tibble() \%>\%
  set_names("value")

df_tbl \%>\%
  opt_bin(
    .value_col = value
    , .iters = 100
  )

}
\seealso{
Other Utilities: 
\code{\link{save_to_excel}()},
\code{\link{sql_left}()},
\code{\link{sql_mid}()},
\code{\link{sql_right}()}
}
\author{
Steven P. Sanderson II, MPH

Modified from Hideaki Shimazaki
Department of Physics, Kyoto University
shimazaki at ton.scphys.kyoto-u.ac.jp
Feel free to modify/distribute this program.
}
\concept{Utilities}
