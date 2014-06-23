bs.opt <- function(cp=c("call", "put"), strike, time.to.mat, spot, vol, rate) {
    stopifnot(cp %in% c("call", "put"))
    ans <- data.frame(cp, strike, time.to.mat, spot, vol, rate, stringsAsFactors=FALSE)
    d1 <- d1(strike, time.to.mat, spot, vol, rate)
    d2 <- d2(strike, time.to.mat, spot, vol, rate)

    ans$price <- NA_real_
    ans$price[ans$cp == "call"] <- with(ans[ans$cp == "call", ], pnorm(d1)*spot - pnorm(d2)*strike*exp(-rate*time.to.mat))
    ans$price[ans$cp == "put"] <- with(ans[ans$cp == "call", ], pnorm(-d2)*strike*exp(-rate*time.to.mat) - pnorm(-d1)*spot)
    return(ans)
}

d1 <- function(strike, time.to.mat, spot, vol, rate)
    1/(vol*sqrt(time.to.mat)) * (log(spot/strike) + (rate+vol^2/2) * time.to.mat)
d2 <- function(strike, time.to.mat, spot, vol, rate)
    1/(vol*sqrt(time.to.mat)) * (log(spot/strike) + (rate-vol^2/2) * time.to.mat)
