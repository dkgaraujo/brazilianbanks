% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ifdata_backend.R
\name{min_capital_requirements_quarter}
\alias{min_capital_requirements_quarter}
\title{Returns a look-up table with the capital requirements in Brazil for each quarter.
Numbers are valid for banks only so far only - especially credit unions and S5 have other values.}
\usage{
min_capital_requirements_quarter(
  yyyymm_start,
  yyyymm_end,
  capital_requirements
)
}
\arguments{
\item{yyyymm_start}{Start calendar quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.}

\item{yyyymm_end}{End calendar quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.}

\item{capital_requirements}{A tibble with the regulatory capital requirements at each date}
}
\description{
Returns a look-up table with the capital requirements in Brazil for each quarter.
Numbers are valid for banks only so far only - especially credit unions and S5 have other values.
}
