strength <- read.csv("C:/Users/Saptaparni/Desktop/Biostat_18/Datasets/strength/strength.csv", header = T, sep = ",")
View(strength)
dim(strength)
names(strength)
summary(strength)
library(Hmisc)
describe(strength)


# Exploratory data analysis
table(strength$txt)
tapply(strength$age, strength$txt, mean)
tapply(strength$age, strength$txt, sd)
tapply(strength$str, strength$txt, mean)
tapply(strength$str, strength$txt, sd)

# Hypothesis testing
cor(strength$age, strength$str)
t.test(strength$str ~ strength$txt)

# Visual exploratory data analysis
par(mfrow = c(1,3))
hist(strength$str, xlab = "Strength", main = "Histogram of strength")
boxplot(strength$age ~ strength$txt, xlab = "Intervention group", ylab = "Age, y",
        las = 1, main = "Variation of age in both treatment groups")
boxplot(strength$str ~ strength$txt, xlab = "Intervention group", ylab = "Strength",
        las = 1, main = "Variation of strength in both treatment groups")
par(mfrow = c(1,1))

plot(strength$age, strength$str, xlab = "Age, y", ylab = "Strength",
     main = "Variation of strength with age", las = 1, col = strength$txt+1, pch = 16, cex = 2)
legend(65, 35, legend = c("Control", "Treatment"), col = c("Black", "Red"), pch = 16, box.lty = 0)
lines(lowess(strength$age, strength$str), lty = 5, col = "Blue")
lines(lowess(strength$age[strength$txt == 0], strength$str[strength$txt == 0]), col = "Black", lwd = 3)
lines(lowess(strength$age[strength$txt == 1], strength$str[strength$txt == 1]), col = "Red", lwd = 3)

# Linear regression models
## The strength varies between two groups based on the intervention, hence effect of
## treatment on strength is analyzed
strenght_lm2 <- lm(str ~ txt, data = strength)
summary(strenght_lm2)
confint(strenght_lm2)

## We can see how strength varies over the entire age range but this is incorrect way of viewing the data
## because there are two distinct groups
strenght_lm1 <- lm(str ~ age, data = strength)
summary(strenght_lm1)
strength_p1 <- predict(strenght_lm1)
plot(strength$age, strength$str, xlab = "Age, y", ylab = "Strength",
     main = "Variation of strength with age", las = 1, pch = 16, cex = 2)
lines(lowess(strength$age, strength$str), lwd = 2)
lines(strength$age, strength_p1, col = "Red", lwd = 3)

## The variation of strength in both treatment group when controlled for age
strenght_lm3 <- lm(str ~ age + txt, data = strength)
summary(strenght_lm3)
confint(strenght_lm3)
strength_p3 <- predict(strenght_lm3)
plot(strength$age, strength$str, xlab = "Age, y", ylab = "Strength",
     main = "Variation of strength with age", las = 1, col = strength$txt+1, pch = 16, cex = 2)
legend(65, 35, legend = c("Control", "Treatment"), col = c("Black", "Red"), pch = 16, box.lty = 0)
lines(lowess(strength$age[strength$txt == 0], strength$str[strength$txt == 0]), col = "Black", lty = 5)
lines(lowess(strength$age[strength$txt == 1], strength$str[strength$txt == 1]), col = "Red", lty = 5)
lines(strength$age[strength$txt == 0], strength_p3[strength$txt == 0], col = "Black", lwd = 3)
lines(strength$age[strength$txt == 1], strength_p3[strength$txt == 1], col = "Red", lwd = 3)


## Strength decreases with increase of age irrespective of the treatment group.
## However, a person in the intervention group has higher strength than a person
## in the control group when matched for age.