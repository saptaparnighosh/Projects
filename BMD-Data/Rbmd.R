bmd <- read.csv("C:/Users/Saptaparni/Desktop/Biostat_18/Datasets/bmd/bmd.csv", header = T, sep = ",")
View(bmd)
names(bmd)
summary(bmd)
library(Hmisc)
describe(bmd)

# Tables
tapply(bmd$hipbmd, bmd$diab, mean)
tapply(bmd$hipbmd, bmd$diab, sd)

tapply(bmd$hipbmd, bmd$sex, mean)
tapply(bmd$hipbmd, bmd$sex, sd)

table(bmd$diab, bmd$sex)

# Visual exploratory data analysis
par(mfrow = c(1, 3))
hist(bmd$hipbmd, xlab = "BMD g/cm2", main = "BMD histogram", las = 1, ylim = c(0, 12))
boxplot(bmd$hipbmd, ylab = "BMD g/cm2", las =1, main = "Boxplot of hipBMD")
plot(bmd$age, bmd$hipbmd, xlab = "Age, y", ylab = "BMD g/cm2", main = "Variation of BMD with age",
     las = 1, pch = 16, cex = 1.5, ylim = c(0.6, 1.23))
lines(lowess(bmd$age, bmd$hipbmd), lwd = 2)

par(mfrow = c(1,2))
boxplot(bmd$hipbmd ~ bmd$diab, ylab = "BMD g/cm2", xlab = "Diabetes status", las =1,
        main = "Boxplot of hipBMD with diabetes status")
plot(bmd$age, bmd$hipbmd, col = bmd$diab+1, pch =16, cex = 1.5, xlab = "Age, y", ylab = "BMD g/cm2",
     main = "Variation of BMD with age and diabetes status", las = 1, ylim = c(0.6, 1.23))
legend(68, 1.25, legend = c("Non-diabetic", "Diabetic"), col = c("Black", "Red"), pch = 16, box.lty = 0)
lines(lowess(bmd$age[bmd$diab == 0], bmd$hipbmd[bmd$diab == 0]), col = bmd$diab+1, lwd = 2)
lines(lowess(bmd$age[bmd$diab == 1], bmd$hipbmd[bmd$diab == 1]), col = bmd$diab+2, lwd = 2)

boxplot(bmd$hipbmd ~ bmd$sex, ylab = "BMD g/cm2", xlab = "Gender", las =1, main = "Boxplot of hipBMD with gender")
plot(bmd$age, bmd$hipbmd, col = bmd$sex, pch =16, cex = 1.5, xlab = "Age, y", ylab = "BMD g/cm2",
     main = "Variation of BMD with age and sex", las = 1, ylim = c(0.6, 1.23))
legend(68,1.25, legend = c("Female", "Male"), col = c("Black", "Red"), pch = 16, box.lty = 0)
lines(lowess(bmd$age[bmd$sex == 1], bmd$hipbmd[bmd$sex == 1]), col = bmd$sex[bmd$sex == 1], lwd = 2)
lines(lowess(bmd$age[bmd$sex == 2], bmd$hipbmd[bmd$sex == 2]), col = bmd$sex[bmd$sex == 2], lwd = 2)

par(mfrow = c(1,1))

bmd$age60 <- bmd$age - 60

# Linear regressions models
bmd_lm1 <- lm(hipbmd ~ age60, data = bmd)
summary(bmd_lm1)
bmd_p1 <- predict(bmd_lm1)
plot(bmd$age, bmd$hipbmd, xlab = "Age, y", ylab = "BMD g/cm2", main = "Variation of BMD with age", las = 1, cex = 2, pch =16)
lines(lowess(bmd$age, bmd$hipbmd), lwd = 2)
lines(bmd$age, bmd_p1, col = "Red", lwd = 3)
legend(72, 1.18, legend = c("Fit", "Lowess"), col = c("Red", "Black"), lwd = 2, lty =1)
confint(bmd_lm1)
## Variation of BMD with age for Model 1 for different diabetes status
plot(bmd$age, bmd$hipbmd, xlab = "Age, y", ylab = "BMD g/cm2", main = "Variation of BMD with age", las = 1, cex = 2, pch =16)
lines(bmd$age[bmd$diab == 0], bmd_p1[bmd$diab == 0], col = bmd$diab+1, lwd = 2)
lines(bmd$age[bmd$diab == 1], bmd_p1[bmd$diab == 1], col = bmd$diab+2, lwd = 2)
## Variation of BMD with age for Model 1 for different genders
plot(bmd$age, bmd$hipbmd, xlab = "Age, y", ylab = "BMD g/cm2", main = "Variation of BMD with age", las = 1, cex = 2, pch =16)
lines(bmd$age[bmd$sex == 1], bmd_p1[bmd$sex == 1], col = bmd$sex[bmd$sex == 1], lwd = 2)
lines(bmd$age[bmd$sex == 2], bmd_p1[bmd$sex == 2], col = bmd$sex[bmd$sex == 2], lwd = 2)

residue <- bmd$hipbmd - bmd_p1
plot(bmd$age, residue, pch =16, cex = 1.5, las = 1, xlab = "Age, y", ylab = "Residuals", main = "Plot of residual")
lines(lowess(bmd$age, residue), lwd =2)
abline(h=0)
  
bmd_lm2 <- lm(hipbmd ~ age60 + diab, data = bmd)
summary(bmd_lm2)
bmd_p2 <- predict(bmd_lm2)
plot(bmd$age, bmd$hipbmd, col = bmd$diab+1, xlab = "Age, y", ylab = "BMD g/cm2",
     main = "Variation of BMD with age and diabetes status", las = 1, cex = 2, pch =16)
lines(lowess(bmd$age[bmd$diab == 0], bmd$hipbmd[bmd$diab == 0]), lwd = 2)
lines(lowess(bmd$age[bmd$diab == 1], bmd$hipbmd[bmd$diab == 1]), lwd = 2)
lines(bmd$age[bmd$diab == 0], bmd_p2[bmd$diab == 0], col = "Black", lwd = 3)
lines(bmd$age[bmd$diab == 1], bmd_p2[bmd$diab == 1], col = "Red", lwd = 3)

anova(bmd_lm1, bmd_lm2)

bmd_lm3 <- lm(hipbmd ~ age60 + sex, data = bmd)
summary(bmd_lm3)
bmd_p3 <- predict(bmd_lm3)
plot(bmd$age, bmd$hipbmd, col = bmd$sex, xlab = "Age, y", ylab = "BMD g/cm2",
     main = "Variation of BMD with age and sex", las = 1, cex = 2, pch = 16)
lines(lowess(bmd$age[bmd$sex == 1], bmd$hipbmd[bmd$sex == 1]), col = "Black", lwd = 2)
lines(lowess(bmd$age[bmd$sex == 2], bmd$hipbmd[bmd$sex == 2]), col = "Black", lwd = 2)
lines(bmd$age[bmd$sex == 1], bmd_p3[bmd$sex == 1], col = "Black", lwd = 3)
lines(bmd$age[bmd$sex == 2], bmd_p3[bmd$sex == 2], col = "Red", lwd = 3)

anova(bmd_lm1, bmd_lm3)


bmd_lm4 <- lm(hipbmd ~ age60 + sex + diab, data = bmd)
summary(bmd_lm4)
bmd_p4 <- predict(bmd_lm4)
plot(bmd$age, bmd$hipbmd, xlab = "Age, y", ylab = "BMD g/cm2",
     main = "Variation of BMD with age and sex", las = 1, cex = 2, pch = 16)
lines(bmd$age[bmd$sex == 1 & bmd$diab == 0], bmd_p4[bmd$sex == 1 & bmd$diab == 0], col = "Black", lwd = 2)
lines(bmd$age[bmd$sex == 1 & bmd$diab == 1], bmd_p4[bmd$sex == 1 & bmd$diab == 1], col = "Red", lwd = 2)
lines(bmd$age[bmd$sex == 2 & bmd$diab == 0], bmd_p4[bmd$sex == 2 & bmd$diab == 0], col = "Green", lwd = 2)
lines(bmd$age[bmd$sex == 2 & bmd$diab == 1], bmd_p4[bmd$sex == 2 & bmd$diab == 1], col = "Blue", lwd = 2)
legend(72, 1.17, legend = c("Non-diabetic female", "Diabetic female"), col = c("Black", "Red"), lty =1, lwd =2)
legend(65.5, 0.73, legend = c("Non-diabetic male", "Diabetic male"), col = c("Green", "Blue"), lty =1, lwd =2)

anova(bmd_lm1, bmd_lm4)



## In this dataset, as age increases the hip BMD decreases. Self-reported diabetes or gender has no statistically significant
## effect on hip BMD and is therefore neither a confounder nor effect modifier. However, on average the hip bmd of non-diabetic 
## people is greater than their diabetic counterpart and this is true for both genders. Also, males have a lower bmd than their 
## age controlled female counterparts.