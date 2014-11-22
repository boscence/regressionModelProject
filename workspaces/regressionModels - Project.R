library(knitr)
library(ggplot2)

mtcars$amL = as.factor(mtcars$am)
mtcars$cylL = as.factor(mtcars$cyl)

###Explority Data Analysis

length(mtcars[,1])

par(mfrow=c(3,1)) 

hist(mtcars$mpg, main = "Chart 1 - Histogram - Miles Per Gallon (MPG)",xlab = "Miles Per Gallon", col =4)

plot(density(mtcars$mpg), main = "Chart 2 - Density Plot - Miles Per Gallon (MPG)", col =4, lwd = 2)

qqnorm(mtcars$mpg, main = "Chart 3 - Normal Q-Q Plot - Miles Per Gallon (MPG)")
qqline(mtcars$mpg, col = 4)

shapiro.test(mtcars$mpg)

par(mfrow=c(1,1)) 
qplot(amL, mpg, data = mtcars,color = cylL,
      main = "Miles Per Gallon by Transmission Type") + geom_boxplot(col = 4,alpha = .1) + 
  geom_point(lwd = 3)

all = lm(mpg ~ . , data = mtcars)
plot(all)

