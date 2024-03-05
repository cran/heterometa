\name{higgins}
\alias{higgins}
\alias{print.higgins}
\title{Compute heterogeneity statistics after Higgins}
\description{
  Computes various statistics recommended by Higgins et al for
  quantifying heterogeneity in meta-analysis\loadmathjax
}
\usage{
higgins(Q = NULL, k = NULL, pval = NULL, slab = NULL, conflevel = 0.95)
\method{print}{higgins}(x, type = "I2", na.print = "", ...)
}
\arguments{
  \item{Q}{Numeric: a vector of heterogeneity \mjeqn{\chi^2}{chi-squared} from the meta--analyses}
  \item{k}{Numeric: a vector of number of studies in each meta-analysis}
  \item{pval}{Numeric: a vector of \mjseqn{p} values}
  \item{slab}{Character: a vector of labels for the meta-analyses}
  \item{conflevel}{Numeric: a vector of confidence levels}
  \item{x}{An object of class \code{higgins}}
  \item{type}{One of "H", "I2", "both"}
  \item{na.print}{What to print instead of NA}
  \item{\dots}{Argument(s) to be passed through}
}
\details{
Either \code{Q} or \code{pval} should be provided. 
Limited error checks for illegal parameters are performed.
A warning is given if any \code{conflevel} is \mjseqn{<0.5}.
A print method is provided.
}
\value{
A list of type \code{higgins} containing
  \item{H}{A data frame with columns Q, k, H, ll, ul, where ll and ul are the confidence limits}
  \item{I2}{A data frame with columns Q, k, I2, ll, ul}
  \item{call}{The call}
}
\references{\insertRef{higgins02}{heterometa}}
\author{Michael Dewey}
\examples{
higgins(14.4, 24) # 1    (1, 1.34)      0 (0, 45)
higgins(14.1, 11) # 1.19 (1, 1.64)     20 (0, 65) probably a typo for 29
higgins(81.5, 19) # 2.13 (1.71, 2.64)  78 (66, 86)
higgins(41.5, 7)  # 2.63 (1.90, 3.65)  86 (72, 92)
higgins(130.3, 3) # 8.07 (6.08, 10.72) 98 (97, 99)
data(dat.higgins02)
with(dat.higgins02, higgins(Q, trials, slab = rownames(dat.higgins02)))
}
\keyword{htest}
