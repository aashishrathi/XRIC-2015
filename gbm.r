library(gbm)
data <- read.table("Training_Dataset.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
test <- read.table("Leaderboard_Dataset.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
data$mvar_2 <- as.numeric(sub(",","",sub("\\$","",data$mvar_2)))
data$mvar_3 <- as.numeric(sub(",","",sub("\\$","",data$mvar_3)))
data$mvar_4 <- as.numeric(sub(",","",sub("\\$","",data$mvar_4)))
data$mvar_5 <- as.numeric(sub(",","",sub("\\$","",data$mvar_5)))
data$mvar_6 <- as.numeric(sub(",","",sub("\\$","",data$mvar_6)))
data <- data[sample(nrow(data)),]
size <- nrow(data)
data.train <- data[1:round(0.6*size),]
data.test <- data[round((0.6*size) + 1):round(size),]

model <- gbm(actual_vote~. -Citizen.ID,data.train,distribution='multinomial',n.trees=200,interaction.depth=4,shrinkage=0.02,cv.folds = 5, bag.fraction = 0.7,verbose=TRUE)
pred <- predict(model,n.trees=200, newdata=data.test,type='response')

p.pred <- apply(pred, 1, which.max)
actual <- data.test$actual_vote
p.pred_final <- (ifelse(p.pred == 1,"CENTAUR",ifelse(p.pred == 2,"COSMOS",ifelse(p.pred==3,"EBONY",ifelse(p.pred == 4,"ODYSSEY","TOKUGAWA")))))
p1 <- ifelse(actual == p.pred_final,1,0)
sum(p1)
length(p1)
sum(p1)/length(p1)
