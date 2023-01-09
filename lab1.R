sizes <- sample(30:50, 3)
sizes

MEANS <- c(7,15,10)
SD <- 3

g1 <- data.frame(
  x = rnorm(sizes[1], MEANS[1], SD),
  group='A'
)
g2 <- data.frame(
  x = rnorm(sizes[2], MEANS[2], SD),
  group='B'
)
g3 <- data.frame(
  x = rnorm(sizes[3], MEANS[3], SD),
  group='C'
)

g_full= rbind(g1,g2, g3)
g_full


g1_mean <- mean(g1$x)
g2_mean <- mean(g2$x)
g3_mean <- mean(g3$x)
g_full_mean <- mean(g_full$x)
min_v <- min(g_full$x)
max_v <- max(g_full$x)
means <- c(min_v,max_v, g1_mean,g2_mean,g3_mean,g_full_mean)
means_labels <- c('min', 'max', 'm1','m2','m3','m')

COLORS <- c('red', 'green', 'blue')
palette(COLORS)
plot(
  x=g_full$x,
  col = as.factor(g_full$group),
  pch=1,
  xlab='i',
  ylab='y',
  yaxt='n'

)

axis(2, at=means, labels=means_labels, las=1, cex.axis=0.9)

x_b <- c(0,nrow(g1), nrow(g1)+nrow(g2))
x_e <- x_b+nrow(g3)
y_b <- c(g1_mean,g2_mean,g3_mean)
y_e <- y_b

segments(x_b, y_b, x_e, y_e, col = COLORS)
abline(g_full_mean, 0, lty = "dashed")
