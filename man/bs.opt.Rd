\name{bs.opt}
\alias{bs.opt}
\title{Black-Scholes Option}
\description{Applications of he Black-Scholes option pricing formula}
\usage{
bs.opt(cp=c("call", "put"), strike, time.to.mat, spot, vol, rate)
}
\arguments{
  \item{cp}{Direction of the option: call or put}
  \item{strike}{Strike price of the option}
  \item{time.to.mat}{Time to maturity of the option, expressed in years}
  \item{spot}{Spot price of the underlying}
  \item{vol}{Volatility of the underlying}
  \item{rate}{Risk-free or forward rate}
}
\value{A \code{data.frame} with inputs, price, and sensitivities}
\references{
  \url{http://en.wikipedia.org/wiki/Black\%E2\%80\%93Scholes_model}
}
\examples{
## EUR/USD -- using numeric times (years)
bs.opt("call", 135:138/100, 1, 1.365, 0.07, 0)
}
