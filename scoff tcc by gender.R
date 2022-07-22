tccm <- function(b, a, c) {
  J <- length(b)
  if (missing(c)) c <- rep(0, J)
  if (missing(a)) a <- rep(1, J)
  theta <- seq(-4, 4, .1)
  ts <- rep(0, length(theta))
  for (j in 1:J) {
    P <-  c[j] + (1 - c[j]) / (1 + exp(-a[j]*(theta-b[j])))
    ts <- ts + P
  }
  plot(theta, ts, type="l", xlim=c(-4,4), ylim=c(0,J),
    xlab=~theta, ylab=expression(paste("Expected True Score")),
    main="", cex.lab = 1.1, lty=c(1), col = ("black"), lwd = 1)
}

tccf <- function(b, a, c) {
  J <- length(b)
  if (missing(c)) c <- rep(0, J)
  if (missing(a)) a <- rep(1, J)
  theta <- seq(-4, 4, .1)
  ts <- rep(0, length(theta))
  for (j in 1:J) {
    P <-  c[j] + (1 - c[j]) / (1 + exp(-a[j]*(theta-b[j])))
    ts <- ts + P
  }
  plot(theta, ts, type="l", xlim=c(-4,4), ylim=c(0,J), 
       xlab=~theta, ylab=expression(paste("Expected True Score")),
       main="", cex.lab = 1.1, lty=c(2), col = ("black"), lwd = 1)
}

maleb <- c(1.56, 1.26, 1.68, 1.50, 1.45)
malea <- c(3.25, 3.30, 1.41, 2.86, 2.51)
femaleb <- c(1.81, .62, 2.78 , 1.07, 1.48)
femalea <- c(1.55, 2.82, .73, 1.31, 1.61)

tccm(maleb, malea)

par(new = TRUE)

tccf(femaleb, femalea)

legend("topleft", c("Females", "Males"), lty=c(1, 2), lwd = 1, col=c("black", "black"), 
       bty="n", cex = .85)

