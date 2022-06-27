#Binarization - Section 3.2
data.all <- read.csv("../gapminderDataFiveYear.csv",stringsAsFactors = TRUE)
library(caret)
binarizer <- dummyVars(formula = '~ continent', data.all, sep = '_') # formula = '~ var1 + var2 + var3'
continent.binarized <- predict(binarizer, data.all)

data.binarized <- cbind(data.all, continent.binarized)
data.binarized[data.binarized$year == 2007, 
               c("country",
                 "continent",
                 "continent_Africa", 
                 "continent_Americas", 
                 "continent_Asia",
                 "continent_Europe",
                 "continent_Oceania")][1:10,]


#Cleaning data - Section 3.3
data.test.new <- data.test[!is.na(data.test$Age),setdiff(colnames(data.test),c("Income"))]
data.test.new

#Random sampling - Section 3.6
data.gmd <- read.csv("../gapminderDataFiveYear.csv",stringsAsFactors = TRUE)
data.sample2 <- data.gmd[sample(1:nrow(data.gmd), size = 20, replace = FALSE),]
library(caret)
samples <- createDataPartition(c(1:nrow(data.gmd)), p = 0.1)
data.sample5 <- data.gmd[ samples$Resample1, ]

#Merge data - Section 3.7
merged.data <- merge(x = merged.data, y = demographic.data, by.x = c("Zip", "Sex"), by.y = c("ZipCode", "Sex"), all.x = TRUE, all.y = FALSE)

#Data visualization - Section 3.8
library(ggplot2)
library(dplyr)
diamonds <- as.data.frame(diamonds)

df <- diamonds %>% group_by(carat) %>% summarize(meanprice = mean(price),
                                                 medianprice = median(price),
                                                 q1price = quantile(price, 0.25),
                                                 q3price = quantile(price, 0.75))
ggplot(data = diamonds, aes(x = carat)) +
  geom_point(aes(y=price), alpha = 1/30) +
  xlim(0, 2.5) +  geom_line(data = df, aes(y=meanprice), color = "red") +
  geom_line(data = df, aes(y = medianprice), color = "blue") +
  geom_line(data = df, aes(y = q1price), color = "orange") +
  geom_line(data = df, aes(y = q3price), color = "orange")

library(GGally)
ggpairs(diamonds, c(1:2, 7))


#Data transformation - Section 3.9
original <- runif(10000, min = 0, max = 100)
standardized <- as.data.frame(scale(original)) # (x -mean) / std

#Data Preparation Practice - Section 3.10

#Boxplot - Section 4.1
library(ggplot2)
library(gridExtra)
data.mortality <- read.csv("../Module 3/soa_mortality_data.csv")
p1 <- ggplot(data = data.mortality, aes(x = prodcat, y = attage)) +
  geom_boxplot(fill = c(1:4), alpha = 0.5) 
p2 <- ggplot(data = data.mortality, aes(x = prodcat, y = issage)) +
  geom_boxplot(fill = c(1:4), alpha = 0.5) 
p3 <- ggplot(data = data.mortality, aes(x = prodcat, y = duration)) +
  geom_boxplot(fill = c(1:4), alpha = 0.5) 
p4 <- ggplot(data = data.mortality, aes(x = "attage", y = attage)) +
  geom_boxplot(alpha = 0.5) 
p5 <- ggplot(data = data.mortality, aes(x = "issage", y = issage)) +
  geom_boxplot(alpha = 0.5) 
p6 <- ggplot(data = data.mortality, aes(x = "duration", y = duration)) +
  geom_boxplot(alpha = 0.5) 
grid.arrange(p1,p4,p2,p5,p3,p6,ncol=2)

#Frequency Tables
library(data.table)
table(data.mortality$prodcat)
table(data.mortality$prodcat) / nrow(data.mortality)
prop.table(table(data.mortality$prodcat))


#Conditional distribution - Section 4.2
p1 <- ggplot(data = data.mortality, aes(issage, fill=prodcat, ..density..)) +
  geom_histogram(binwidth = 5, alpha = 0.5)
p2 <- ggplot(data = data.mortality, aes(issage, fill=prodcat, ..density..)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "dodge")
grid.arrange(p1,p2,ncol=1)

ggplot(data = subset(data.mortality, prodcat == "TRM"), aes(x = duration, y = actual_face, color = sex)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 80)) + 
  #scale_y_continuous(limits = c(0, 1.0e+07)) +
  scale_y_log10(limits = c(1, 1.0e+07))

#Correlations
library(reshape2)
vars.numeric <- colnames(data.mortality)[sapply(data.mortality, is.numeric)]
cormat <- cor(data.mortality[vars.numeric])
melted.cormat <- melt(cormat)
ggplot(data = melted.cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1))

#Principal Component Analysis - Section 4.3
diamonds <- ggplot2::diamonds
diamonds.5d.pca <- subset(diamonds[ ,c(5, 7:10)], x >= 3 & x <= 10 & z > 0 & z <=7.5) # five variables
d.pca.5d <- prcomp(diamonds.5d.pca, center = TRUE, scale. = TRUE)
summary(d.pca.5d)
d.pca.5d$rotation

#Data transformation - Section 5.1
data.mortality$duration_log <- log(data.mortality$duration)
data.mortality$issage_norm <- (data.mortality$issage - min(data.mortality$issage)) / (max(data.mortality$issage - min(data.mortality$issage)))
data.mortality$issage_bin10 <- cut(data.mortality$issage, 10)
data.mortality$issage_bin20 <- cut(data.mortality$issage, 20, labels = FALSE) # Note the difference between using labels or not

#Binarize variables
library(caret)
vars.bin <- c("sex", "smoker", "prodcat", "region", "distchan", "uwkey", "uwtype", "resind_ind")
for (var in vars.bin) {
  data.mortality[, var] <- as.character(data.mortality[, var])
}
binarizer <- caret::dummyVars(paste("~", paste(vars.bin, collapse = "+")) , data = data.mortality, fullRank = F)
data.mortality <- cbind(data.mortality,
                        data.frame(predict(binarizer, newdata = data.mortality)))

#Correlations - Section 5.2
data.all <- read.csv("../BreastCancerWisconsinDataSet.csv",stringsAsFactors = TRUE)
b.Pearson <- cor(data.all, method = "pearson")[ ,c("target")] # method="kendall" or method="spearman"
b.Pearson <- sort(b.Pearson, decreasing = TRUE)

#Mutual information
library(reshape2)
b.mi <- mutinformation(discretize(data.all, "equalfreq"), method = "emp")
melted.b.mi <- melt(b.mi)
ggplot(data = melted.b.mi, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Linear Regression - Section 5.3
library(glmnet)
X <-as.matrix(df[,3:8]) #a matrix is required for the glmnet function formula
formula.lm <- as.formula("y~X1+X2+X3+X4+X5+X6")
model.lm <- glmnet(X, y = df$y,family = "gaussian", alpha = 0, lambda = 0) #ordinary least squares
df$pred <- predict(model.lm, newx = X) #predict
p1 <- ggplot(data = df, aes(x = x, y = y)) + geom_point(color = "blue", size = 3) + geom_line(aes(y=df$pred))
model.lm$a0 # intercept
model.lm$beta # coefficients

#Fit the tree
# Fit the tree
library(rpart)
library(rpart.plot)
library(caret)
formula.dt <- as.formula(paste("target~",paste(colnames(data.training[,c(1:30)]),collapse = "+")))
control.dt <- list(maxdepth = 5, cp = 0)
model.dt <- rpart(formula.dt, data = data.training, control = control.dt)
rpart.plot(model.dt)
importance <- as.data.frame(varImp(model.dt, competes = FALSE))
data.frame(row.names = row.names(importance)[order(importance, decreasing = TRUE)],
           Importance = importance[order(importance, decreasing = TRUE),])

#Save csv file
write.csv(df, "C:\\Users\\Bob\\Desktop\\data.csv", row.names=FALSE)

library(data.table)
fwrite(df, "C:\\Users\\Bob\\Desktop\\data.csv")

library(readr)
write_csv(df, "C:\\Users\\Bob\\Desktop\\data.csv")

