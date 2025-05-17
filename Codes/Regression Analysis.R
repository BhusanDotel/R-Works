ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)


reg <- lm(x2~x1+x3+x4+x5,data=ccpp)
summary(reg)

