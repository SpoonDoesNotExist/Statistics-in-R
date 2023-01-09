size <- 1000; size

x <- rnorm(size, 0, 1)
r_xy <- 0.1
y <- r_xy * x + sqrt(1 - r_xy^2) * r_xy * rnorm(size, 0, 1)
cor.test(x,y)
# cmap <- palette(c("red","green","blue"))

plot(x, y, col=cmap)
abline(v = 0,  lty = 2)
abline(h = 0, lty = 2)

cor.test(x,y)
