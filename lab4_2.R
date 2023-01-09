library(caret)
data <- read.csv("Rent 22 01 11.csv", header = TRUE, sep = ';', dec=',')
data
data$district <- as.factor(data$district)

dmy <- dummyVars(" ~ .", data = data)
data <- data.frame(predict(dmy, newdata = data))
data

multiple_reg <- lm(rent ~., data=data)
summary(multiple_reg)

multiple_reg <- lm(rent ~. -district.1, data=data)
summary(multiple_reg)


cor(data)
library(car)
vif(multiple_reg) # коэффициент инфляции дисперсии
durbinWatsonTest(multiple_reg)
plot(multiple_reg, which = 1)
plot(multiple_reg, which = 2)
hist(multiple_reg$residuals, main = "Residual Histogram")
shapiro.test(multiple_reg$residuals)

my_data <- data.frame(
  m2 = 50,
  floor = 5,
  rooms = 2,
  walls = 1,
  district.1 = 0,
  district.2 = 0,
  district.3 = 0,
  district.4 = 1
)

prediction <- predict(multiple_reg, newdata = my_data)
prediction
