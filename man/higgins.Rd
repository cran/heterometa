\name{higgins}
\alias{higgins}
\alias{print.higgins}
\title{Compute heterogeneity statistics after Higgins}
\description{
  Computes various statistics suggested by Higgins and Thompson for
  quantifying heterogeneity in meta-analysis\loadmathjax
}
\usage{
higgins(Q = NULL, k = NULL, I2 = NULL,
   pval = NULL, slab = NULL, conflevel = 95)
\method{print}{higgins}(x, type = "I2", na.print = "", ...)
}
\arguments{
  \item{Q}{Numeric: a vector of heterogeneity \mjeqn{\chi^2}{chi-squared} from the meta--analyses}
  \item{k}{Numeric: a vector of number of studies in each meta-analysis}
  \item{I2}{Numeric: a vector of values of \mjseqn{I^2}}
  \item{pval}{Numeric: a vector of \mjseqn{p} values}
  \item{slab}{Character: a vector of labels for the meta-analyses}
  \item{conflevel}{Numeric: a vector of confidence levels}
  \item{x}{An object of class \code{higgins}}
  \item{type}{One of "H", "I2", "both"}
  \item{na.print}{What to print instead of NA}
  \item{\dots}{Argument(s) to be passed through}
}
\details{
Either \code{Q} or \code{pval} or
\code{I2} should be provided.
If \code{I2} is provided it may be either
as a percentage or a proportion.
If a single value is given for \code{conflevel}
it is used for all the analyses.

Limited error checks for illegal parameters are performed.
If \code{conflevel} is \mjseqn{<=1}
the function proceeds assuming that was meant
as a value of \mjseqn{\alpha}.
If the parameters are supplied as vectors a check
is made for equal length.
If they are not then a warning is issued
but the function tries to return a sensible
result which should be checked to
see if it is what was desired.

For reference the formulae used are
\mjseqn{Q = H^2 (k - 1)},
\mjseqn{H^2 = \frac{Q}{k - 1}},
\mjseqn{I^2 = \frac{H^2 - 1}{H^2}},
\mjseqn{H^2 = - \frac{1}{I^2 - 1}}.

The print method allows for printing \mjseqn{H} or
\mjseqn{I^2} or both of
these and this is controlled by the parameter
\code{type}.
}
\value{
A list of type \code{higgins} containing
  \item{H}{A data frame with columns Q, k, H, ll, ul, where ll and ul are the confidence limits}
  \item{I2}{A data frame with columns Q, k, I2, ll, ul}
  \item{call}{The call}
}
\note{\mjseqn{I^2} is always printed as a percentage even if
the input parameter
was supplied as a proportion.}
\references{\insertRef{higgins02}{heterometa}}
\author{Michael Dewey}
\examples{
# first the examples one by one
higgins(14.4, 24) # 1    (1, 1.34)      0 (0, 45)
higgins(14.1, 11) # 1.19 (1, 1.64)     20 (0, 65) probably a typo for 29
higgins(81.5, 19) # 2.13 (1.71, 2.64)  78 (66, 86)
higgins(41.5, 7)  # 2.63 (1.90, 3.65)  86 (72, 92)
higgins(130.3, 3) # 8.07 (6.08, 10.72) 98 (97, 99)
# now repeat getting from dat.higgins02
data(dat.higgins02)
dat <- dat.higgins02
with(dat, higgins(Q, trials, slab = rownames(dat.higgins02)))
# supply I2 or pval
higgins(I2 = dat$I2[3], k = dat$trials[3])
higgins(pval = dat$pval[3], k = dat$trials[3])
}
\keyword{htest}
