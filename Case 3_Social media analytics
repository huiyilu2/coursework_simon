Facebook.data<- read.csv("C:/Users/odyss/Downloads/HW2 Data_2021.csv")
Facebook.data <- Facebook.data[,-2]
fb.reg <- lm(clickPerDollar ~ factor(adType)+ factor(category) + factor(placement) + factor(keywords)+ factor(body) + ageMean, data = Facebook.data)
summary(fb.reg)
coef(fb.reg)

