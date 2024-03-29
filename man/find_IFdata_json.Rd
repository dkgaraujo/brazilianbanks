% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_IFdata_json}
\alias{find_IFdata_json}
\title{Formats the path to a local or remote IF.data JSON file from the respective quarter.}
\usage{
find_IFdata_json(
  yyyymm = NULL,
  file_name,
  cache_folder_name = "cache_json",
  cache_json = TRUE
)
}
\arguments{
\item{yyyymm}{Quarter}

\item{file_name}{The name of the JSON file (including the extension) to be retrieved locally or remotely.}

\item{cache_folder_name}{The local folder where the JSON files are (to be) cached.}

\item{cache_json}{TRUE. Whether the JSON files with the raw data should be cached locally.}
}
\value{
The path to the JSON file saved locally (if `cache_json` is TRUE) or to its URL if FALSE.
}
\description{
Formats the path to a local or remote IF.data JSON file from the respective quarter.
}
