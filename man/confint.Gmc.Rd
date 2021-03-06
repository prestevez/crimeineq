% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confint.Gmc.R
\name{confint.Gmc}
\alias{confint.Gmc}
\title{Confint Method for Gmc class}
\usage{
\method{confint}{Gmc}(x, conf.level = 0.95)
}
\arguments{
\item{x}{an object with class \link{Gmc}.}

\item{conf.level}{the value of the confidence interval; defaults to
0.95. Only accepts values between 0 and 1, exclusive.}
}
\value{
A vector of length two with a confidence interval for the Gini
     estimate obtained from the Monte Carlo simulation of counts.
}
\description{
A method to obtain the confidence interval for a \link{Gmc} class object.
}
