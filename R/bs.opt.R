bs.opt <- function(cp=c("call", "put"), strike, time.to.mat=NULL, val.date=NULL, exp.date=NULL, spot, vol, rate=NULL, fwd=NULL) {
    cp <- match.arg(cp, several.ok=TRUE)

    if (is.null(time.to.mat)) {
        if (is.null(val.date)) val.date <- Sys.Date()
        time.to.mat <- as.numeric(exp.date - val.date) / 365
    }
    if (is.null(rate)) {
        rate <- log(fwd / spot) / time.to.mat
    }
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

bs.fx.opt <- function(cp=c("call", "put"), strike, time.to.mat=NULL, val.date=NULL, exp.date=NULL, spot, vol, rate=NULL, fwd=NULL) {
    ans <- bs.opt(cp=cp, strike=strike, time.to.mat=time.to.mat, spot=spot, vol=vol, rate=rate)
    ans$price <- ans$price / ans$spot
    ans$delta <- ans$delta - ans$price
    ans$gamma <- ans$vega <- ans$theta <- NULL

    return(ans)
}

d1 <- function(strike, time.to.mat, spot, vol, rate)
    1/(vol*sqrt(time.to.mat)) * (log(spot/strike) + (rate+vol^2/2) * time.to.mat)
d2 <- function(strike, time.to.mat, spot, vol, rate)
    1/(vol*sqrt(time.to.mat)) * (log(spot/strike) + (rate-vol^2/2) * time.to.mat)
