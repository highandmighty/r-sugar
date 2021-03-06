% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sugar.R
\name{download_parquet}
\alias{download_parquet}
\title{Parquet from URL Fetcher}
\usage{
download_parquet(url)
}
\arguments{
\item{url}{A direct URL of Parquet file}
}
\value{
A dataframe with Parquet file's data.
}
\description{
Fetches Parquet file from URL into dataframe.
}
\examples{
dataframe = download_parquet("http://example.com/data/table.parquet")
}
