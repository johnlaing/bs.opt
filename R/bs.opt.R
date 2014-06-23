bs.opt <- function(cp=c("call", "put"), strike, time.to.mat, spot, vol, rate) {
    stopifnot(cp %in% c("call", "put"))
    ans <- data.frame(cp, strike, time.to.mat, spot, vol, rate, stringsAsFactors=FALSE)
    d1 <- d1(strike, time.to.mat, spot, vol, rate)
    d2 <- d2(strike, time.to.mat, spot, vol, rate)

    cc <- ans$cp == "call"

    ans$price <- NA_real_
    ans$price[cc] <- with(ans[cc, ], pnorm(d1)*spot - pnorm(d2)*strike*exp(-rate*time.to.mat))
    ans$price[!cc] <- with(ans[!cc, ], pnorm(-d2)*strike*exp(-rate*time.to.mat) - pnorm(-d1)*spot)

    ans$delta <- NA_real_
    ans$delta[cc] <- pnorm(d1)
    ans$delta[!cc] <- -pnorm(-d1)

    ans$gamma <- dnorm(d1) / (spot * vol * sqrt(time.to.mat))

    ans$vega <- spot * dnorm(d1) * sqrt(time.to.mat)

    ans$theta <- NA_real_
    ans$theta[cc] <- -spot*dnorm(d1)*vol / (2*sqrt(time.to.mat)) - rate*strike*exp(-rate*time.to.mat) * pnorm(d2)
    ans$theta[!cc] <- -spot*dnorm(d1)*vol / (2*sqrt(time.to.mat)) + rate*strike*exp(-rate*time.to.mat) * pnorm(-d2)

    return(ans)
}

d1 <- function(strike, time.to.mat, spot, vol, rate)
    1/(vol*sqrt(time.to.mat)) * (log(spot/strike) + (rate+vol^2/2) * time.to.mat)
d2 <- function(strike, time.to.mat, spot, vol, rate)
    1/(vol*sqrt(time.to.mat)) * (log(spot/strike) + (rate-vol^2/2) * time.to.mat)
