% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_fat_adjust.R
\name{check_fat_adjust}
\alias{check_fat_adjust}
\title{check_fat_adjust}
\usage{
check_fat_adjust(data, y, locvar, fatadjust = fat_adjust())
}
\arguments{
\item{data}{A data frame.}

\item{y}{A variable.}

\item{locvar}{Variable name for the locale variable.}

\item{fatadjust}{A list with variables, species and locale combinations that
should be fat adjusted}
}
\value{
1 if fat adjustment should be made, 0 otherwise.
}
\description{
Checks if data should be fat adjusted
}
\details{
Only for internal use
}
\author{
Erik Lampa
}
