size <- 100
sigma <- 42

mu <- 0
eps <- rnorm(size, mu, sigma)

a <- 17
b <- 3
x <- 1:size
y <- a + b * x + eps
plot(x, y)

simple.fit <- lm(y ~ x)
abline(simple.fit, col='red')

summary(simple.fit)


y_mean <- mean(y)
x_mean <- mean(x)

b_ <- sum((y - y_mean) * (x - x_mean)) / sum((x - x_mean)^2)
a_ <- y_mean - b_ * x_mean

b_; a_

summary(simple.fit)

y_ <- a_ + b_ * x

S2 <- sum((y - y_)^2) / (size - 2); S2
b_var <- S2 / sum((x - x_mean)^2); b_var
a_var <- b_var * mean(x^2); a_var

sqrt(b_var); sqrt(a_var)
summary(simple.fit)

t_a <- a_ / sqrt(a_var); t_a
t_b <- b_ / sqrt(b_var); t_b
2 * (1 - pt(t_a, 2))
qt(0.975,size-2)
t_a
2 * (1 - pt(t_b, 2))
t_b

k <- 2 - 1
SSE <- sum((y - y_)^2)
sqrt(SSE / (size - (1 + k)))

R2 <- 1 - sum((y - y_)^2) / sum((y - y_mean)^2); R2

F <- (size - 2) * R2 / (1 - R2); F
pf(F, 1, size - 2, lower.tail = FALSE)
summary(simple.fit)


# ---------------------------------------------------------------------------------

size <- 100
sigma <- 0.2
mu <- 0
eps <- rnorm(size, mu, sigma)

a <- 1
b <- 3
x <- 1:size
# y <- a + b / x + eps
y <- a * x^b + eps
plot(x, y)
lines(x, nonlinear.fit$fitted.values, col='red')
x1 <- 1/x
nonlinear.fit <- lm(y ~ x1)
plot(x1, y)
summary(nonlinear.fit)
abline(nonlinear.fit, col='red')


# ---------------------------------------------------------------------------------

size <- 100
sigma <- 0.5
mu <- 0
eps <- rlnorm(size, mu, sigma)

a <- 1
b <- 3
x <- 1:size
y <- a * (x^b)*eps
plot(x, y)
nonlinear.fit <- lm(log(y) ~ log(x))
lines(x, exp( nonlinear.fit$fitted.values ), col='red')
plot(log(y) ~ log(x))
summary(nonlinear.fit)
abline(nonlinear.fit, col='red')

