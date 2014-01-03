\name{gte}
\alias{gte}
\alias{plot.gte}
\alias{print.gte}
\title{Generalized Turnbull's Estimator}
\usage{
  gte(formula, data, z, h = NULL, itermax = 1e+05,
    tole = 5e-04)

  \method{print}{gte} (x, ...)

  \method{plot}{gte} (x, overlay = TRUE, shade = TRUE,
    xlab = "time", ylab = "survival", xleg = "bottomleft",
    yleg = NULL, ...)
}
\arguments{
  \item{formula}{A formula object with the response on the
  left of a ~ operator, and the covariates on the right.
  The response must be a survival object as returned by the
  \code{Surv} function from the package \pkg{survival} (see
  \bold{Details}).}

  \item{data}{An optional data frame, list or environment
  containing the variables in the model formula. If not
  found in data, the variables are taken from
  \code{environment(formula)}, typically the environment
  from which \code{gte} is called.}

  \item{z}{A matrix: each row contains the values of a
  covariate vector at which an estimate of the conditional
  survival function is requested.  If there is only one
  covariate, it can be a vector (possibly of length 1).}

  \item{h}{A vector: the values of the bandwidth parameter
  \eqn{h} for each covariate (default = equation 7 of
  Dehghan and Duchesne (2011)).}

  \item{itermax}{maximal number of iterations for the
  algorithm (default=100000).}

  \item{tole}{maximal distance between successive
  iterations tolerated before declaring convergence
  (default=0.0005).}

  \item{x}{An object, produced by the \code{gte} function,
  to print or to plot.}

  \item{overlay}{A logical: Should the curves be overlayed
  when there is more than one estimate of the conditional
  survival function in the \code{gte} object \code{x}?
  (default=\code{TRUE})}

  \item{shade}{A logical: Should the rectangles of
  indeterminate NPMLE (innermost interval) be shaded?
  (default=\code{TRUE})}

  \item{xlab}{A label for the x-axis, by defaut \code{xlab
  = "time"}.}

  \item{ylab}{A label for the y-axis, by defaut \code{ylab
  = "survival"}.}

  \item{xleg}{x location for legend, "bottomleft" by
  default (see \code{\link{legend}}).}

  \item{yleg}{y location for legend, NULL by default (see
  \code{\link{legend}}).}

  \item{\dots}{Further arguments to be passed to
  \code{print.default} or \code{plot.default}.}
}
\value{
  \item{time}{ A vector: the ordered distinct values of the
  left and right endpoints of the censoring interval
  (omitting the smallest value, but always including time
  0).}

  \item{surv}{ A matrix: the estimates of the conditional
  survival function at time \code{time}.  The \eqn{i}th
  column refers to the \eqn{i}th value of the covariate
  vector given in \code{z} (row \eqn{i} of \code{z}). }

  \item{intmap}{A matrix : The intervals of the potential
  steps in the conditional survival function, called
  innermost interval, over which the GTE is indeterminate.
  The left endpoints of the intervals are in the first row,
  and the rigth endpoints in the second. The object
  attribute LRin denotes whether to include each of the
  endpoints or not. This matrix is computed with an
  internal function derived from function \code{Aintmap} of
  the \pkg{interval} package.}

  \item{surv.summary}{A summary of \code{surv}: the
  estimates of the conditional survival function only for
  the intervals of the potential steps in the function
  (innermost intervals). The row names describe the
  intervals, which are detailed in \code{intmap}. }

  \item{Call}{ The function call.}
}
\description{
  The \code{gte} function computes the generalized
  Turnbull's estimator (GTE) proposed by Dehghan and
  Duchesne (2011). It is a nonparametric estimator of a
  conditional survival function given a vector of
  continuous covariates that can handle interval-censored
  lifetimes.\cr The \code{print} method for objects
  obtained from \code{gte} only prints the output value
  \code{surv.summary}.\cr The \code{plot} method for
  objects obtained from \code{gte} plots the estimate of
  the conditional survival function, by default overlaying
  curves if more than one estimate is present and shading
  the innermost interval, in which the GTE is
  indeterminate.
}
\details{
  For interval-censored data, the \code{\link{Surv}}
  function should be called with the argument
  \code{type="interval"} or \code{type="interval2"}. If
  \code{type="interval"}, the \code{event} argument is
  mandatory. Therefore, in addition to the left and right
  endpoints of the censoring interval (called,
  respectively, \code{left} and {right} for illustrative
  purpose), one would need a third variable (\code{status})
  taking the value 0 for right censored data, 1 for an
  event at exact time, 2 for left censored data and 3 for
  interval censored data. The \code{\link{Surv}} function
  would be called as follows:\cr \code{Surv(time=left,
  time2=right, event=status, type="interval")}.

  If \code{type="interval2"}, the \code{event} argument
  cannot be given. The value of \code{event} is derived
  from the \code{time} and \code{time2} argument as
  follows:\cr if \code{time} takes the value \code{NA},
  \code{event=2} (left censored data);\cr if \code{time2}
  takes the value \code{NA}, \code{event=0} (right censored
  data);\cr if \code{time=time2}, \code{event=1} (exact
  time);\cr otherwise, \code{event=3} (interval censored
  data).\cr See the help page of the \code{\link{Surv}}
  function for more details.\cr

  In the \code{gte} function, the data must be given
  through the \code{\link{Surv}} function but it is
  internally transformed in two vectors : \code{L} and
  \code{R} for the left and right endpoints of the
  censoring interval, respectively.\cr If \code{event=0}
  (right censored data), then \code{L=time} and
  \code{R=Inf};\cr if \code{event=1} (exact time), then
  \code{L=time} and \code{R=time};\cr if \code{event=2}
  (left censored data), then \code{L=0} and
  \code{R=time};\cr and if \code{event=3} (interval
  censored data), then \code{L=time} and \code{R=time2};\cr
  If one has vectors \code{L} and \code{R} respecting this
  convention, they can be given directly to \code{gte} by
  calling \code{\link{Surv}} as follows:\cr \code{Surv(L,
  R, type="interval2")}.
}
\examples{
data(simul)

## Calling Surv() with type="interval2"
Fit <- gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=c(10, 20))
Fit

## Calling Surv() with type="interval"
event <- ifelse(is.na(simul$R), 0,
                ifelse(is.na(simul$L), 2,
                       ifelse(simul$R==simul$L, 1, 3)))
time <- ifelse(event==2, simul$R, simul$L)
time2 <- ifelse(event==3, simul$R, NA)
simul_event <- cbind(simul, time, time2, event)

Fit_event <- gte(Surv(time, time2, event, type="interval") ~ Z, data=simul_event, z=c(10, 20))
Fit_event

# The results are the same
all.equal(Fit_event$time, Fit$time)
all.equal(Fit_event$surv, Fit$surv)

## Plotting the results
plot(Fit, xleg="topright")
}
\author{
  Mohammad Hossein Dehghan, Thierry Duchesne and Sophie
  Baillargeon
}
\references{
  Dehghan, M. H. and Duchesne, T. (2011). A generalization
  of Turnbull's estimator for nonparametric estimation of
  the conditional survival function with interval-censored
  data. \emph{Lifetime Data Analysis}, \bold{17}, 234-255.
}
\seealso{
  \code{\link{Surv}}
}

