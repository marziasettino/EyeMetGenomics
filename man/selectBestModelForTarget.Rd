% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EyeMetGen_Functions.R
\name{selectBestModelForTarget}
\alias{selectBestModelForTarget}
\title{selectBestModelForTarget}
\usage{
selectBestModelForTarget(TargetmodelsToCompare, TargetmodelsAUCToCompare)
}
\arguments{
\item{beta}{is matrix of coefficients}

\item{bystep}{FALSE means which variables were ever nonzero/bystep = TRUE means which variables are nonzero for each step}
}
\value{
integer
}
\description{
get the non zero coefficients
}
