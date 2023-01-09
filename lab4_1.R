data <- read.csv("Rent 22 01 11.csv", header = TRUE, sep = ';', dec=',')
data
simple.fit <- lm(rent ~ m2, data=data)

plot(data$rent ~ data$m2)
lines(data$m2, simple.fit$fitted.values, col='red')
summary(simple.fit)

# library(broom)
plot(simple.fit, which=1)
# broom::glance(simple.fit)

unseen <- data.frame(m2 = c(10, 15, 30));unseen
predict(simple.fit, unseen)
predict(simple.fit, unseen, interval = 'prediction', level = 0.95)