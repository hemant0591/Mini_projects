data = read.csv("C:/Users/guhemant/Desktop/Somedata.csv", header = T)
names(data)

#[1] "IsJonesboro"    "IsRiverdale"    "IsForest.Park"  "IsConyers"     
#[5] "IsCovington"    "IsAustell"      "IsMarietta"     "IsSFR"         
#[9] "IsCondo"        "Bed"            "Bath"           "SqFt"          
#[13] "SqFt.2"         "SqFt.3"         "Is1990"         "Is2000"        
#[17] "SB.1.mile"      "SB.2.mile"      "SB.3.mile"      "SB.5.mile"     
#[21] "X.SB.5.mile"    "Rent...Monthly"


names(data)[names(data) == "Rent...Monthly"] = "Rent"
m = cor(data[c("SB.1.mile","SB.2.mile","SB.3.mile","SB.5.mile","X.SB.5.mile","Rent")])
#install.packages("corrplot")
library(corrplot)
#corrplot(m, method = "circle")

data = data[, -which(names(data) %in% c("IsConyers","IsCovington","SB.2.mile","SB.3.mile","SB.5.mile","X.SB.5.mile"))]
dim(data)
names(data)

apply(data,2,function(x) sum(is.na(x)))
#plot(Rent~SqFt,data)

data = data[data$Rent <=1400,]
data = data[data$SqFt <= 2500,]
#plot(Rent~SqFt,data)
dim(data)

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

lm.fit <- lm(Rent~., data=train)
summary(lm.fit)

pr.lm <- predict(lm.fit,test)
#pr.lm.full = predict(lm.fit, data)
MSE.lm <- sum((pr.lm - test$Rent)^2)/nrow(test)
#MSE.lm.full = sum((pr.lm.full- data$Rent)^2)/nrow(data)
print(paste(MSE.lm))
#write.csv(pr.lm.full,"Output.lm.csv", row.names = FALSE)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
dim(scaled)

train_ <- scaled[index,]
test_ <- scaled[-index,]

#install.packages("neuralnet")
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("Rent ~", paste(n[!n %in% "Rent"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,2),linear.output=T)

#plot(nn)
#names(nn)
#nn$model.list

pr.nn <- compute(nn,test_[,1:15])
#pr.nn.full <- compute(nn,scaled[,1:16])
pr.nn_ <- pr.nn$net.result*(max(data$Rent)-min(data$Rent))+min(data$Rent)
#pr.nn.full_ <- pr.nn.full$net.result*(max(data$Rent)-min(data$Rent))+min(data$Rent)
test.r <- (test_$Rent)*(max(data$Rent)-min(data$Rent))+min(data$Rent)
#full.r <- (scaled$Rent)*(max(data$Rent)-min(data$Rent))+min(data$Rent)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
#MSE.nn.full <- sum((full.r - pr.nn.full_)^2)/nrow(data)

print(paste(MSE.lm,MSE.nn))
#print(paste(MSE.lm,MSE.lm.full,MSE.nn,MSE.nn.full))

par(mfrow=c(1,2))

plot(test$Rent,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$Rent,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$Rent,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$Rent,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

library(boot)
set.seed(200)
lm.fit <- glm(Rent~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

set.seed(450)
cv.error <- NULL
my_weights <- NULL
k <- 10
my.file = letters[1:k]
my.out = 1:10

#install.packages("plyr")
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
    index <- sample(1:nrow(data),round(0.9*nrow(data)))
    train.cv <- scaled[index,]
    #write.table(train.cv, paste(my.file[i], ".csv", sep = ""), row.names = FALSE, sep = ",")
    test.cv <- scaled[-index,]
    #write.csv(test.cv, paste(my.out[i],".csv",sep = ""), row.names = FALSE) 
    nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
   
    pr.nn <- compute(nn,test.cv[,1:15])
    pr.nn <- pr.nn$net.result*(max(data$Rent)-min(data$Rent))+min(data$Rent)
    
    test.cv.r <- (test.cv$Rent)*(max(data$Rent)-min(data$Rent))+min(data$Rent)
    
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    my_weights[i] <- nn$weights
    pbar$step()
}

cv.error
a = which.min(cv.error)
#my_weights[a]
a
mean(cv.error)
min(cv.error)
#boxplot(cv.error,xlab='MSE CV',col='cyan',
 #       border='blue',names='CV error (MSE)',
  #      main='CV error (MSE) for NN',horizontal=TRUE)

