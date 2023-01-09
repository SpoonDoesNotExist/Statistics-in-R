size1 <- 89
size2 <- 90


# 1) ------------------------------------------------------------
MEAN1 <- 5
SD1 <- 2
MEAN2 <- 15.04
SD2 <- 2

x1 <- rnorm(size1, MEAN1, SD1)
x2 <- rnorm(size2, MEAN2, SD2)

mean(x1);mean(x2)
var(x1);var(x2)

var.test(x1,x2)
t.test(x1,x2, var.equal = T)



col1 <- rgb(0,0,1,1/4)
col2 <- rgb(1,0,0,1/4)


p1 <- hist(x1, prob=T, col=col1)
lines(density(x1))
abline(v = mean(x1), col = col1, lwd = 3)

p2 <- hist(x2, prob=T, col=col2, add=T)
lines(density(x2))
abline(v = mean(x2), col = col2, lwd = 3, add=T)

# xlim <- range(c(p1$breaks, p2$breaks));xlim

boxplot(x1, x2, col=c(col1,col2))
# 2) ------------------------------------------------------------
MEAN1 <- 5
SD1 <- 2
MEAN2 <- 7
SD2 <- 2

x1 <- rnorm(size1, MEAN1, SD1)
x2 <- rnorm(size2, MEAN2, SD2)

mean(x1);mean(x2)
var(x1);var(x2)

var.test(x1,x2)
t.test(x1,x2, var.equal = T)



col1 <- rgb(0,0,1,1/4)
col2 <- rgb(1,0,0,1/4)


p1 <- hist(x1, prob=T, col=col1)
lines(density(x1))
abline(v = mean(x1), col = col1, lwd = 3)

p2 <- hist(x2, prob=T, col=col2, add=T)
lines(density(x2))
abline(v = mean(x2), col = col2, lwd = 3, add=T)

# xlim <- range(c(p1$breaks, p2$breaks));xlim

boxplot(x1, x2, col=c(col1,col2))
# 3) ------------------------------------------------------------
MEAN1 <- 5
SD1 <- 2.4
MEAN2 <- 5.4
SD2 <- 2

x1 <- rnorm(size1, MEAN1, SD1)
x2 <- rnorm(size2, MEAN2, SD2)

mean(x1);mean(x2)
var(x1);var(x2)

var.test(x1,x2)
t.test(x1,x2, var.equal = F)



col1 <- rgb(0,0,1,1/4)
col2 <- rgb(1,0,0,1/4)


p1 <- hist(x1, prob=T, col=col1)
lines(density(x1))
abline(v = mean(x1), col = col1, lwd = 3)

p2 <- hist(x2, prob=T, col=col2, add=T)
lines(density(x2))
abline(v = mean(x2), col = col2, lwd = 3, add=T)

# xlim <- range(c(p1$breaks, p2$breaks));xlim

boxplot(x1, x2, col=c(col1,col2))
# 4) ------------------------------------------------------------

MEAN1 <- 5
SD1 <- 5
MEAN2 <- 8
SD2 <- 3.5

x1 <- rnorm(size1, MEAN1, SD1)
x2 <- rnorm(size2, MEAN2, SD2)

mean(x1);mean(x2)
var(x1);var(x2)

var.test(x1,x2)
t.test(x1,x2, var.equal = F)



col1 <- rgb(0,0,1,1/4)
col2 <- rgb(1,0,0,1/4)


p1 <- hist(x1, prob=T, col=col1)
lines(density(x1))
abline(v = mean(x1), col = col1, lwd = 3)

p2 <- hist(x2, prob=T, col=col2, add=T)
lines(density(x2))
abline(v = mean(x2), col = col2, lwd = 3, add=T)

# xlim <- range(c(p1$breaks, p2$breaks));xlim

boxplot(x1, x2, col=c(col1,col2))


