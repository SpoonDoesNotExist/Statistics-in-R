library(caret)

data <- read.table("flats 1.txt", header = TRUE)
data$F <- as.numeric(data$F=='yes')
data$Type <- as.numeric(data$Type=='st')

data <- data[-c(48,43,47),]


model <- lm(Rent ~ M2, data = data)
summary(model)
plot(model)

model_m <- lm(Rent ~ .-Type -Total, data = data)
summary(model_m)

# library(rms)
# rms::vif(model_m)
# cor(data)
library(car)
# cor(data)
vif(model_m) # коэффициент инфляции дисперсии
durbinWatsonTest(model_m)
plot(model_m, which = 1)
plot(model_m, which = 2)
hist(model_m$residuals, main = "Residual Histogram")
shapiro.test(model_m$residuals)

library(lmtest)
gqtest(model_m, order.by= ~M2, data=data, fraction = 10)

data$M2_INV <- 1
data <- data/data$M2

data
model_new <- lm(Rent ~M2_INV, data = data)
summary(model_new)
plot(model_new, which = 1)
plot(model_new, which = 2)
hist(model_new$residuals, main = "Residual Histogram")
gqtest(model_new, order.by= ~M2, data=data, fraction = 10)