% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_met.R
\name{convert_met}
\alias{convert_met}
\title{convert_met()}
\usage{
convert_met(...)
}
\arguments{
\item{...}{Ignored}
}
\value{
A list with conversion rules
}
\description{
Conversion rules for metals
}
\details{
For use in \code{convert}. Produces a list with contaminant names as the
    elements. Each element is itself a list with values for the species.
}
\examples{
## Change the value for Herring and Hg to 1.25
    conv_met <- convert_met()
    conv_met$HG$CLUP <- 1.25
}
\author{
Erik Lampa
}
