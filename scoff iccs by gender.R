iccm1 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-4, 4, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 1", lty=c(1), col = ("black"), lwd = 1)
}

iccf1 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-4, 4, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 1", lty=c(2), col = c("black"), lwd = 1)
}

m1 <- iccm1(1.56, 3.25)
par(new = TRUE)
f1 <- iccf1(1.55, 1.81)
legend("topleft", c("Males", "Females"), lty=c(1, 2), lwd = 1, col=c("black", "black"), 
       bty="n", cex = .85)

iccm2 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-4, 4, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0, 
       xlab=~theta, ylab="Probability", main = "Item 2", lty=c(1), col = ("black"), lwd = 1)
}

iccf2 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-3, 3, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 2", lty=c(2), col = c("black"), lwd = 1)
}

m2 <- iccm2(1.26, 3.30)
par(new = TRUE)
f2 <- iccf2(.62, 2.82)
legend("topleft", c("Males", "Females"), lty=c(1, 2), lwd = 1, col=c("black", "black"),
       bty="n", cex = .85)

iccm3 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-4, 4, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 3", lty=c(1), col = ("black"), lwd = 1)
}

iccf3 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-4, 4, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 3", lty=c(2), col = ("black"), lwd = 1)
}

m3 <- iccm3(1.68, 1.41)
par(new = TRUE)
f3 <- iccf3(2.78, .738)
legend("topleft", c("Males", "Females"), lty=c(1, 2), lwd = 1, col=c("black", "black"), 
       bty="n", cex = .85)

iccm4 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-4, 4, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 4", lty=c(1), col = ("black"), 
       lwd = 1)
}

iccf4 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-4, 4, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 4", lty=c(2), col = ("black"), 
       lwd = 1)
}

m4 <- iccm4(1.67, 1.55)
par(new = TRUE)
f4 <- iccf4(.67, 1.25)
legend("topleft", c("Males", "Females"), lty=c(1, 1), lwd = 1, col=c("black", "black"), 
       bty="n", cex = .85)

iccm5 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-4, 4, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 5", lty=c(1), col = ("black"), 
       lwd = 1)
}

iccf5 <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,5,3))
  theta <- seq(-3, 3, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  plot(theta, P, type="l", xlim=c(-4,4), ylim=c(0,1), cex.lab = 1.0,
       xlab=~theta, ylab="Probability", main = "Item 5", lty=c(2), col = ("black"), 
       lwd = 1)
}


m5 <- iccm5(1.73, 2.45)
par(new = TRUE)
f5 <- iccf5(1.51, 2.16)
legend("topleft", c("Males", "Females"), lty=c(1, 2), lwd = 1, col=c("black", "black"), 
       bty="n", cex = .85)

