% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymaxlik.R
\name{mymaxlik}
\alias{mymaxlik}
\title{mymaxlik}
\usage{
mymaxlik(lfun, x, param)
}
\arguments{
\item{lfun}{function}

\item{x}{data}

\item{param}{a sequence of numbers}
}
\value{
parami, i, yi, and slope. Also a graph.
}
\description{
mymaxlik
}
\examples{
\dontrun{mymaxlik(x=c(9,9,1,9,9,9),param=seq(0,1,length=1000),lfun=logbin,cex.main=2)}
}
