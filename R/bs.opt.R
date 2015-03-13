bs.opt <- function(cp=c("call", "put"), strike=NULL, delta=NULL, time.to.mat=NULL, val.date=NULL, exp.date=NULL, spot, vol=NULL, price=NULL, rate=NULL, fwd=NULL) {
    cp <- match.arg(cp, several.ok=TRUE)

    if (is.null(time.to.mat)) {
        if (is.null(val.date)) val.date <- Sys.Date()
        if (is.null(exp.date)) stop("if 'time.to.mat' is NULL then 'exp.date' must be provided")
        time.to.mat <- as.numeric(exp.date - val.date) / 365
    }
    if (is.null(rate)) {
        if (is.null(fwd) | is.null(spot)) stop("if 'rate' is NULL then 'fwd' and 'spot' must be provided")
        rate <- log(fwd / spot) / time.to.mat
    }
    if (is.null(strike)) {
        if (is.null(delta)) stop("if 'strike' is NULL then 'delta' must be provided")
        if (is.null(vol)) stop("if 'strike' is NULL then 'vol' must be provided")
        if (any(abs(delta) < 0 | abs(delta) > 1)) stop("'delta' must be between 0 and 1")

        strike <- spot
        ans <- bs.opt(cp=cp, strike=strike, time.to.mat=time.to.mat, spot=spot, vol=vol, rate=rate)
        if (any(c(call=1, put=-1)[ans$cp] != sign(delta))) stop("calls must have positive delta and puts must have negative delta")
        while (any(abs(ans$delta - delta) > .Machine$double.eps ^ 0.5)) {
            strike <- strike + strike * (ans$delta - delta) / ans$gamma
            ans <- bs.opt(cp=cp, strike=strike, time.to.mat=time.to.mat, spot=spot, vol=vol, rate=rate)
        }
        return(ans)
    }
    if (is.null(vol)) {
        if (is.null(price)) stop("if 'vol' is NULL then 'price' must be provided")

        vol <- 0.1
        ans <- bs.opt(cp=cp, strike=strike, time.to.mat=time.to.mat, spot=spot, vol=vol, rate=rate)
        while (any(abs(ans$price - price) > .Machine$double.eps ^ 0.5)) {
            vol <- ans$vol - (ans$price - price) / ans$vega
            ans <- bs.opt(cp=cp, strike=strike, time.to.mat=time.to.mat, spot=spot, vol=vol, rate=rate)
        }
        return(ans)
    }
    ans <- data.frame(cp, strike, time.to.mat, spot, vol, rate, stringsAsFactors=FALSE)
    d1 <- with(ans, d1(strike, time.to.mat, spot, vol, rate))
    d2 <- with(ans, d2(strike, time.to.mat, spot, vol, rate))

    cc <- ans$cp == "call"

    ans$price <- NA_real_
    ans$price[cc] <- with(ans[cc, ], pnorm(d1[cc])*spot - pnorm(d2[cc])*strike*exp(-rate*time.to.mat))
    ans$price[!cc] <- with(ans[!cc, ], pnorm(-d2[!cc])*strike*exp(-rate*time.to.mat) - pnorm(-d1[!cc])*spot)

    ans$delta <- NA_real_
    ans$delta[cc] <- pnorm(d1[cc])
    ans$delta[!cc] <- -pnorm(-d1[!cc])

    ans$gamma <- dnorm(d1) / (spot * vol * sqrt(time.to.mat))

    ans$vega <- spot * dnorm(d1) * sqrt(time.to.mat)

    ans$theta <- NA_real_
    ans$theta[cc] <- with(ans[cc, ], -spot*dnorm(d1[cc])*vol / (2*sqrt(time.to.mat)) - rate*strike*exp(-rate*time.to.mat) * pnorm(d2[cc]))
    ans$theta[!cc] <- with(ans[!cc, ], -spot*dnorm(d1[!cc])*vol / (2*sqrt(time.to.mat)) + rate*strike*exp(-rate*time.to.mat) * pnorm(-d2[!cc]))
    ans$theta <- ans$theta / 365 ## daily

    return(ans)
}

bs.fx.opt <- function(cp=c("call", "put"), strike=NULL, delta=NULL, time.to.mat=NULL, val.date=NULL, exp.date=NULL, spot, vol, rate=NULL, fwd=NULL) {
    if (is.null(strike)) {
        if (is.null(delta)) stop("if 'strike' is NULL then 'delta' must be provided")
        if (any(abs(delta) < 0 | abs(delta) > 1)) stop("'delta' must be between 0 and 1")

        strike <- spot
        ans <- bs.fx.opt(cp=cp, strike=strike, time.to.mat=time.to.mat, val.date=val.date, exp.date=exp.date, spot=spot, vol=vol, rate=rate, fwd=fwd)
        if (any(c(call=1, put=-1)[ans$cp] != sign(delta))) stop("calls must have positive delta and puts must have negative delta")
        while (any(abs(ans$delta - delta) > .Machine$double.eps ^ 0.5)) {
            strike <- strike + strike * (ans$delta - delta) / ans$gamma
            ans <- bs.fx.opt(cp=cp, strike=strike, time.to.mat=time.to.mat, val.date=val.date, exp.date=exp.date, spot=spot, vol=vol, rate=rate, fwd=fwd)
        }
        return(ans)
    }
    ans <- bs.opt(cp=cp, strike=strike, time.to.mat=time.to.mat, val.date=val.date, exp.date=exp.date, spot=spot, vol=vol, rate=rate, fwd=fwd)
    ans$price <- ans$price / ans$spot
    ans$delta <- ans$delta - ans$price
    ans$gamma <- ans$gamma - ans$delta / ans$spot
    ans$vega <- ans$vega / ans$spot
    ans$theta <- ans$theta / ans$spot

    return(ans)
}

d1 <- function(strike, time.to.mat, spot, vol, rate)
    1/(vol*sqrt(time.to.mat)) * (log(spot/strike) + (rate+vol^2/2) * time.to.mat)
d2 <- function(strike, time.to.mat, spot, vol, rate)
    1/(vol*sqrt(time.to.mat)) * (log(spot/strike) + (rate-vol^2/2) * time.to.mat)
