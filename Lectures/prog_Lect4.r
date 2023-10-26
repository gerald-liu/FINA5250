library(MASS)
rm(list = ls())
setwd("d:/workspace/FINA5250/")

#Recall: one factor model
rm(list = ls())
BH2009_2023 <- read.table("BH2009-2023.csv",sep=',',header=TRUE)
attach(BH2009_2023)
head(BH2009_2023)
rBH_ex <- rBH-rf
Onefactor <- lm(rBH_ex ~ rM_ex)
summary(Onefactor)

#Multifactor Models
pairs(cbind(rBH_ex,rM_ex,rSmB,rHmL))

#Model specification
FF3factor <- lm(rBH_ex ~ rM_ex + rSmB + rHmL)
summary(FF3factor)

#Compare the two model
anova(Onefactor,FF3factor)
round(c(summary(Onefactor)$r.squared, summary(FF3factor)$r.squared),3)

fakeX<-rnorm(length(rM_ex))

Twofactor<- lm(rBH_ex ~ rM_ex + fakeX)

summary(Twofactor)

anova(Onefactor, Twofactor)

#Facebook
#Training and Testing Splitting
rm(list = ls())
facebook <- read.table("facebook.csv",sep=',',header=TRUE)
attach(facebook)
head(facebook)
library(caTools)
set.seed(1)
spl <- sample.split(Nbr.Post_Clicks,SplitRatio= 0.8)
Train <- subset(facebook, spl==TRUE)
Test <- subset(facebook, spl==FALSE)

#Full Model
full_model <- lm(Nbr.Post_Clicks~factor(Category)+Type+factor(Post_month)+factor(Post_day)
                 +Post_time+Paid + Page_total_likes, data=Train)
summary(full_model)

#Smaller Model
small_model <- lm(Nbr.Post_Clicks~ Type, data=Train)
summary(small_model)

#In-sample Comparison
anova(full_model, small_model)
c(summary(full_model)$r.squared, summary(small_model)$r.squared)

#Out of sample Comparison:Prediction
### out-of-sample R-squared
evaluate <- function(model){
  y_Hat <- predict.lm(model, newdata=Test)
  error <- Test$Nbr.Post_Clicks - y_Hat
  SSE <- sum(error^2)
  TSS <- sum((Test$Nbr.Post_Clicks - mean(Train$Nbr.Post_Clicks))^2)
  Predicted.Rsq <- 1 - SSE/TSS
  print(Predicted.Rsq)}
evaluate(full_model)
evaluate(small_model)

y_pred_full = predict(full_model, Test)

###Optional: 
facebook <- read.csv("facebook.csv", header = TRUE)
attach(facebook)

names(facebook)
summary(facebook)

Category <- as.factor(facebook$Category)
Type <- as.factor(facebook$Type)
Post_month <- as.factor(facebook$Post_month)
Post_day <- as.factor(facebook$Post_day)
Post_time <- as.factor(facebook$Post_time)
Paid <- as.factor(facebook$Paid)

boxplot(Nbr.Post_Clicks ~ Category,  main="Number of Post Clicks",
        xlab="Category ", ylab="Number clicks")

boxplot(Nbr.Post_Clicks ~ Type,  main="Number of Post Clicks",
        xlab="Type ", ylab="Number clicks")

boxplot(Nbr.Post_Clicks ~ Paid,  main="Number of Post Clicks",
        xlab="Paid ", ylab="Number clicks")

boxplot(Nbr.Post_Clicks ~ Post_month,  main="Number of Post Clicks",
        xlab="Post_month", ylab="Number clicks")

# test about Paid
mean(Nbr.Post_Clicks[Paid == 1])
mean(Nbr.Post_Clicks[Paid == 0])
t.test(Nbr.Post_Clicks ~ Paid)


# test about Type
facebook_photo_video <- subset(facebook, Type  %in% c("Photo", "Video"))
t.test(facebook_photo_video$Nbr.Post_Clicks ~ facebook_photo_video$Type)
