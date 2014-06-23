\name{bs.opt}
\alias{bs.opt}
\alias{bs.fx.opt}
\title{Black-Scholes Option}
\description{Applications of he Black-Scholes option pricing formula}
\usage{
bs.opt(cp=c("call", "put"), strike, time.to.mat=NULL, val.date=NULL, exp.date=NULL, spot, vol, rate=NULL, fwd=NULL)
bs.fx.opt(cp=c("call", "put"), strike, time.to.mat=NULL, val.date=NULL, exp.date=NULL, spot, vol, rate=NULL, fwd=NULL)
}
\arguments{
  \item{cp}{Direction of the option: call or put.}
  \item{strike}{Strike price of the option.}
  \item{time.to.mat}{Time to maturity of the option, expressed in years. If left \code{NULL}, will be calculated from \code{val.date} and \code{exp.date}.}
  \item{val.date, exp.date}{Valuation and expiration dates. Used to calculate time to maturity if \code{time.to.mat} is \code{NULL}, and ignored otherwise.}
  \item{spot}{Spot price of the underlying.}
  \item{vol}{Volatility of the underlying.}
  \item{rate}{Risk-free or forward rate. If left \code{NULL}, will be implied from \code{fwd}, \code{spot}, and \code{time.to.mat}.}
  \item{fwd}{Forward price of the underlying. Used to calculate forward rate if \code{rate} is \code{NULL}, and ignored otherwise.}
}
\value{A \code{data.frame} with inputs, price, and sensitivities.}
\details{
  These functions take inputs necessary for evaluating options and run them through the Black-Scholes formula and its derivatives.
  The \code{bs.fx.opt} variant implements the conventions of FX options that are priced in the currency of the underlying. E.g., a EUR/USD option (the option to buy or sell EUR vs USD) is priced in EUR.
}
\references{
  \url{http://en.wikipedia.org/wiki/Black\%E2\%80\%93Scholes_model}
}
\examples{
## EUR/USD -- using numeric times (years)
bs.opt("call", strike=135:138/100, time.to.mat=1, spot=1.365, vol=0.07, rate=0)

## use dates instead of time.to.mat
today <- Sys.Date()
bs.opt("call", strike=135:138/100, val.date=today, exp.date=today+365, spot=1.365, vol=0.07, rate=0)

## also use fwd instead of rate
today <- Sys.Date()
bs.opt("call", strike=135:138/100, val.date=today, exp.date=today+365, spot=1.365, vol=0.07, fwd=1.365)
}
