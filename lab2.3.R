# 1) -------------------
data <- read.csv("Rent 22 01 11.csv", header = TRUE, sep = ';', dec=',')
# data <- na.omit(data)
data
# 2) -------------------
# data$district <- as.character(data$district)
# data$walls <- as.character(data$walls)
#
# data$rent <- as.double(data$rent)
# data$m2 <- as.double(data$m2)

unique(data$floor)
data$floor <- ordered(data$floor, levels = 1:max(unique(data$floor)))
unique(data$rooms)
data$rooms <- ordered(data$rooms, levels = 1:max(unique(data$rooms)))

sapply(data, class)
# 3) -------------------
hist(data$rent)
hist(data$m2)
# 4) -------------------
shapiro.test(data$rent)
shapiro.test(data$m2)
# 5) -------------------
boxplot(data = data, rent ~ district)
unique(data$district)
kruskal.test(data = data, rent ~ district)


boxplot(data = data, rent ~ walls)
x1 <- data[data$walls==0,]$rent;x1
x2 <- data[data$walls==1,]$rent;x2
shapiro.test(x1)
shapiro.test(x2)
wilcox.test(x1,x2)

# ------------------------------------------

boxplot(data = data, m2 ~ district)
kruskal.test(data = data, m2 ~ district)


boxplot(data = data, m2 ~ walls)
x1 <- data[data$walls==0,]$m2;x1
x2 <- data[data$walls==1,]$m2;x2
shapiro.test(x1)
shapiro.test(x2)
wilcox.test(x1,x2)
# 6) -------------------
nrow(data)
max(subset(data, district == 2)$rent)
data_filtered <- subset(data, !((rent == 20000) & (district==2)) )
nrow(data_filtered)

# install.packages("dplyr")
# library(dplyr)
#
#
#
# q <- .95
# data %>%
#   group_by(district) %>%
#   summarize(critical_quant = quantile(rent, probs = q))
#
#
#
# data_filtered <- data %>%
#   group_by(district) %>%
#   filter(rent < quantile(rent, 0.95))
#
# boxplot(data = data, rent ~ district)
# boxplot(data = data_filtered, rent ~ district)
# 7) -------------------
cor(data_filtered$rent, data_filtered$m2, method = "pearson")
cor.test(data_filtered$rent, data_filtered$m2)

cor.test(as.numeric(data_filtered$floor), as.numeric(data_filtered$rooms), method = "spearman")



# shapiro.test(data %>% filter(walls == 0))
# data %>%
#   group_by(district) %>%
#   shapiro.test()
#
# shapiro.test(data$m2)
# 8) -------------------
contingency_table <- table(data_filtered$walls, data_filtered$district)
contingency_table

chisq.test(contingency_table)

# install.packages("rcompanion")
library("rcompanion")

cramerV(contingency_table, ci=TRUE)






