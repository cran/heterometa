\name{dat.higgins02}
\alias{dat.higgins02}
\docType{data}
\title{Example data}
\description{
The package contains the following dataset: \code{higgins02}.
\loadmathjax}
\usage{data(dat.higgins02)
}
\format{A data frame with 5 observations (meta-analyses) on 13 variables:
  \describe{
     \item{\code{Q}}{Values of the heterogeneity \mjseqn{\chi^2}}
     \item{\code{trials}}{The number of studies in each meta-analysis}
     \item{\code{tau2}}{The value of \mjeqn{\tau^2}{tau2}}
     \item{\code{pval}}{The associated p-value}
     \item{\code{H}}{The value of H}
     \item{\code{Hlo}}{Lower limit of the interval for H}
     \item{\code{Hhi}}{Upper limit of the interval for H}
     \item{\code{R}}{The value of R}
     \item{\code{Rlo}}{Lower limit of the interval for R}
     \item{\code{Rhi}}{Upper limit of the interval for R}
     \item{\code{I2}}{The value of I2}
     \item{\code{I2lo}}{Lower limit of the interval for I2}
     \item{\code{I2hi}}{Upper limit of the interval for I2}
     }
   }
\note{
  These are values from \insertCite{higgins02;textual}{heterometa}.
  They are provided here for testing the package.
  The row names of the data frame are labels for the topic
  of the meta-analysis: Albumin, AdjuvantChemo, Sclerotherapy,
   CDPCholine, GammaNail. The value for \mjseqn{I^2} for AdjuvantChemo
   is as given in the artricle but is almost certainly a
   typo for 29 not 20.
   
   Not all the variables are used in this package but are presented
   for reference.
}
\author{Michael Dewey}


\references{
\insertAllCited{}
}
\examples{
data(dat.higgins02)
}
\keyword{datasets}
