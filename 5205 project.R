library(ggplot2)
library(leaps)
library(bestglm)

df <- read.csv("/Users/apple/Desktop/salary.txt",header=T)
head(df)
n <- dim(df)[1]
df$city <- ifelse(df$city=="yes", 1, 0)
df$deg <- ifelse(df$deg=="yes", 1, 0)

df$reg.midwest <- ifelse(df$reg=="midwest", 1, 0)
df$reg.northeast <- ifelse(df$reg=="northeast", 1, 0)
df$reg.south <- ifelse(df$reg=="south", 1, 0)
df$race.white <- ifelse(df$race=="white", 1, 0)
df$race.other <- ifelse(df$race=="other", 1, 0)




# overview of the data set
plot(df$wage)
ggplot(data = data.frame(df$wage), aes(x = 1:n, y = df$wage, colour = df$wage)) + geom_point(shape=2)

quantile(df$wage, probs = 0.995)
df2 <- df[df$wage < 2500, ]
n2 <- dim(df2)[1]
plot(df2$wage)
ggplot(data = data.frame(df2$wage), aes(x = 1:n2, y = df2$wage, colour = df2$wage)) + geom_point(shape=2)


boxplot(log(df2$wage)~df2$race)
ggplot(data=df2, aes(x = factor(race), y = log(wage))) +
  geom_jitter(alpha = .3, color = 'grey') +
  geom_boxplot(alpha = .5, color = 'dark blue') +
  stat_summary(fun.y = "mean", 
               geom = "point",
               color = "red", 
               shape = 8, 
               size = 1)

# find mean wage for different year of experience in different races
mean.exp_wage <- tapply(log(df$wage), df$exp, mean)
mean.exp_white <- tapply(log(df$wage[df$race=="white"]), df$exp[df$race=="white"], mean)
mean.exp_black <- tapply(log(df$wage[df$race=="black"]), df$exp[df$race=="black"], mean)
mean.exp_other <- tapply(log(df$wage[df$race=="other"]), df$exp[df$race=="other"], mean)

plot(mean.exp_wage, type = "l", col="red", ylim = c(4.5, 7), ylab = "Average wage level", 
     xlab = "years of experience", main = "Average wage level vs Races", xaxt="n")
lines(mean.exp_white, col="blue", lty=2)
lines(mean.exp_black, col="blue", lty=3)
lines(mean.exp_other, col="blue", lty=4)
legend("bottom", legend = c("all races", "white", "black", "other"), 
       col=c("red","blue", "blue", "blue"), lty=1:4, cex=0.7)
axis(side=1, at=seq(-4, 60, by=16),  labels=c("-4", "12", "28", "44", "60"))

boxplot(log(df$wage)~df$exp)
# abline(lm(log(df$wage)~df$exp), col="purple")
lines(supsmu(df$exp, log(df$wage)), col="red")
ggplot(data=df, aes(x = exp, y = log(wage), group=exp)) +
  geom_jitter(alpha = .3, color = 'grey') +
  geom_boxplot(alpha = .5, color = 'dark blue') +
  stat_summary(fun.y = "mean", 
               geom = "point",
               color = "red", 
               shape = 8, 
               size = 0.5)


boxplot(log(df$wage)~df$emp)
# lines(supsmu(log(df$wage),df$exp),col="red")
# ggplot(data.frame(mean.exp_wage, mean.exp_white, mean.exp_black, mean.exp_other), aes(x=1:length(mean.exp_wage), y=mean.exp_wage), colour = mean.exp_wage) + geom_line()


# find mean wage level for different education years in different races
mean.edu_wage <- tapply(log(df$wage), df$edu, mean)
mean.edu_white <- tapply(log(df$wage)[df$race=="white"], df$edu[df$race=="white"], mean)
mean.edu_black <- tapply(log(df$wage)[df$race=="black"], df$edu[df$race=="black"], mean)
mean.edu_other <- tapply(log(df$wage)[df$race=="other"], df$edu[df$race=="other"], mean)
plot(mean.edu_wage, type = "l", col="red", ylab = "wage level", xlab = "years of education",
     ylim = c(5.4, 6.9))
lines(mean.edu_white, col="blue", lty=2)
lines(mean.edu_black, col="blue", lty=3)
lines(mean.edu_other, col="blue", lty=4)
legend("topleft", legend = c("all races", "white", "black", "other"), 
       col=c("red", "blue", "blue", "blue"), lty=1:4, cex=0.7)

boxplot(log(df$wage)~df$edu)
lines(supsmu(df$edu, log(df$wage)), col="red")
ggplot(data=df, aes(x = edu, y = log(wage), group = edu)) +
  geom_jitter(alpha = .3, color = 'grey') +
  geom_boxplot(alpha = .5, color = 'dark blue') +
  stat_summary(fun.y = "mean", 
               geom = "point",
               color = "red", 
               shape = 8, 
               size = 0.5)
# abline(lm(log(df$wage)~df$edu),col="purple")


# find mean wage level for different region
mean.reg_wage <- tapply(log(df$wage), df$reg, mean)
mean.reg_white <- tapply(log(df$wage)[df$race=="white"], df$reg[df$race=="white"], mean)
mean.reg_black <- tapply(log(df$wage)[df$race=="black"], df$reg[df$race=="black"], mean)
mean.reg_other <- tapply(log(df$wage)[df$race=="other"], df$reg[df$race=="other"], mean)
mean.reg <- data.frame(mean.reg_wage, mean.reg_white, mean.reg_black, mean.reg_other)
plot(mean.reg_wage, type="l", col="red", ylab = "wage level", xlab = "region", 
     ylim = c(5.8, 6.5), xaxt="n")
lines(mean.reg_white, col="blue", lty=2)
lines(mean.reg_black, col="blue", lty=3)
lines(mean.reg_other, col="blue", lty=4)
legend("bottomright", legend = c("all races", "white", "black", "other"), 
       col=c("red", "blue", "blue", "blue"), lty=1:4, cex=0.8)
axis(side=1, at=1:4, labels=c("midwest", "northeast", "south", "west"))
# qplot(1:4, mean.reg_wage, geom = 'line')+qplot(1:4, mean.reg_white, geom = 'line')
mw <- c(mean.reg_wage[1], mean.reg_white[1], mean.reg_black[1], mean.reg_other[1])
ne <- c(mean.reg_wage[2], mean.reg_white[2], mean.reg_black[2], mean.reg_other[2])
so <- c(mean.reg_wage[3], mean.reg_white[3], mean.reg_black[3], mean.reg_other[3])
we <- c(mean.reg_wage[4], mean.reg_white[4], mean.reg_black[4], mean.reg_other[4])
qplot(1:4, mean.reg_wage,geom="line",colour="whole", xlab="region",ylab="mean log(wage)")+
  geom_line(aes(1:4,mean.reg_white,colour="white"))+
  geom_line(aes(1:4,mean.reg_black,colour="black"))+
  geom_line(aes(1:4,mean.reg_other,colour="other"))+
  labs(xlab="region", ylab="mean log(wage)")+
  theme(axis.text.x = element_text(c("m", "n", "s", "w")))


# find mean wage level for college graduate in different races
mean.deg_wage <- tapply(log(df$wage), df$deg, mean)
mean.deg_white <- tapply(log(df$wage)[df$race=="white"], df$deg[df$race=="white"], mean)
mean.deg_black <- tapply(log(df$wage)[df$race=="black"], df$deg[df$race=="black"], mean)
mean.deg_other <- tapply(log(df$wage)[df$race=="other"], df$deg[df$race=="other"], mean)

plot(mean.deg_wage, type="l", col="red", ylab = "wage level", xlab = "region", 
     ylim = c(5.9, 6.8), xaxt="n")
lines(mean.deg_white, col="blue", lty=2)
lines(mean.deg_black, col="blue", lty=3)
lines(mean.deg_other, col="blue", lty=4)
legend("bottomright", legend = c("all races", "white", "black", "other"), 
       col=c("red", "blue", "blue", "blue"), lty=1:4, cex=0.9)
axis(side=1, at=1:2, labels=c("without college degree", "with college degree"))







#Use bestglm package to find the best model based on AIC:
bestglm(overall.data,IC="AIC")

# create qqplot and qqnorm using ggplot
ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample=.resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="blue")
  
  return(p)
}

# rough model 0
model.0 <- lm(wage ~ edu+exp+city+reg+race+deg+com+emp, data= df)
summary(model.0)
plot(df$wage, rstudent(model.0))
ggplot(data = data.frame(df$wage), aes(x = df$wage, y = rstudent(model.0), colour = df$wage)) + geom_point(shape=1)
qqnorm(rstudent(model.0))
qqline(rstudent(model.0), col="red")
ggQQ(model.0)

AIC(model.0)

# test model 1 (change wage to log(wage))
model.1 <- lm(log(wage) ~ edu+exp+city+reg+race+deg+com+emp, data= df)
summary(model.1) # R square increase
plot(df$wage, rstudent(model.1))
qqnorm(rstudent(model.1)) # observe H shape
qqline(rstudent(model.1), col="red")
AIC(model.1) # AIC increase, not a good model


# test model 2 (includes more boolean values)
model.2 <- lm(log(wage) ~ edu+exp+city+reg.midwest+reg.northeast+reg.south+
                com+emp+race.white+race.other, data= df)
summary(model.2) # reg.midwest & com are not significant
plot(log(train.data$wage), rstudent(model.2)) 
qqnorm(rstudent(model.2))
qqline(rstudent(model.2), col="red")
AIC(model.2) # AIC drop from model.1, better than model.0

# observe nonlinearity in edu and exp
par(mfrow=c(2,2))
for (i in c('edu','exp','com','emp')){
  plot(df[,i],rstudent(model.2),ylab='Deleted Residual') 
  abline(h=0,lty=2) 
  lines(supsmu(df[,i],rstudent(model.2)),col=2)
}

# test model 3 (include deg)
model.3 <- lm(log(wage) ~ edu+exp+city+reg.midwest+reg.northeast+reg.south+
               com+deg+emp+race.white+race.other, data= df)
summary(model.3) # regwest, degyes, com are not significant at alpha=0.05
AIC(model.3)

# test model 4 (add polynomial function)
model.4 <- lm(log(wage)~edu+exp+city+reg.northeast+reg.south+deg+com+emp
             +race.white+race.other,data=df)
summary(model.4) # regwest, degyes, com are not significant at alpha=0.05
AIC(model.4)


#Apply the best subsets procedure using Mallow's Cp criterion.
predictors <- cbind(df$edu, df$exp, df$deg, df$city, df$com, df$emp,
                    df$reg.midwest, df$reg.northeast, df$reg.south,
                     df$race.white, df$race.other)
leaps.sub <- leaps(x=predictors,y=log(df$wage))
plot(leaps.sub$size,leaps.sub$Cp,xlab="p",ylab=expression(C[p]))
lines(leaps.sub$size,leaps.sub$size,col=2)
select <- leaps.sub$Cp==min(leaps.sub$Cp)
select <- leaps.sub$which[select,]
predictors[,select]

regsubsets.sub <- regsubsets(x=predictors,y=log(train.data$wage))
summary(regsubsets.sub,scale="Cp")
plot(regsubsets.sub,scale="Cp")


# test model 5 (includes variables from above best subset procedure)
model.5 <- lm(log(wage) ~ edu+exp+city+deg+reg.northeast+reg.south+emp+race.white+race.other, 
              data= df)
summary(model.5)
plot(log(train.data$wage), rstudent(model.4))
qqnorm(rstudent(model.5))
qqline(rstudent(model.5), col="red")
AIC(model.5)



# find correlation between each variable
# observe there might exist linear relationship between edu & exp
cor(overall.data)


# test model 5 (use standardized edu, exp, emp) observe emp not sigificant
model.6 <- lm(log(wage) ~ edu+I(edu^2)+exp+I(exp^2)+deg+city+reg.northeast+reg.south+emp+I(emp^2)+
                race.white+race.other, data= df)
summary(model.6)
plot(log(df$wage), rstudent(model.6))
qqnorm(rstudent(model.6))
qqline(rstudent(model.6), col="red") 
AIC(model.6)


# test model 6 (remove deg, emp square) all significant
model.7 <- lm(log(wage) ~ edu+I(edu^2)+exp+I(exp^2)+city+reg.northeast+reg.south+emp+
                race.white+race.other, data= df)
summary(model.7)
plot(log(df$wage), rstudent(model.7))
qqnorm(rstudent(model.7))
qqline(rstudent(model.7, col="red")) 
AIC(model.7)
ggplot(data = data.frame(df$wage), aes(x = log(df$wage), y = rstudent(model.7))) + geom_point(shape=1)
ggQQ(model.7)

plot(predict(model.7), rstudent(model.7))
lines(supsmu(predict(model.7), rstudent(model.7)), col="red")
abline(h=0, lty=3)

boxplot(rstudent(model.7))

plot(predict(model.7), rstudent(model.7)^2)



# Another approach
#Forward stepwise regression(doesn't use in the report)
min.model <- lm(log(wage) ~ 1, data= train.data)
full.model <- formula(lm(log(wage)~edu+exp+deg+city+reg.midwest+reg.northeast+reg.south
                         +com+emp+race.white+race.other,
                         data= train.data))
step(min.model, scope= full.model, direction= c("forward"))



# model validation
# Training data and testing data
set.seed(0)
index.list <- sample(1:n, 4965,replace = F) # n*0.2
train.data <- df[-index.list,]
data <- train.data
test.data <- df[index.list,]

# sort by race
black <- df$race=="black"
white <- df$race=="white"
other <- df$race=="other"

# quality control check
table(train.data$race,train.data$city)/nrow(train.data)
table(test.data$race,test.data$city)/nrow(test.data)
table(train.data$race,train.data$reg)/nrow(train.data)
table(test.data$race,test.data$reg)/nrow(test.data)



# MSE
final.model <- lm(log(wage) ~ edu+I(edu^2)+exp+I(exp^2)+city+reg.northeast+reg.south+emp+
                    race.white+race.other, data= train.data)
MSE <- sum((residuals(final.model))^2)/(nrow(train.data)-11)
MSE.earlier <- sum((residuals(model.1))^2)/(nrow(df)-11)
MSE
MSE.earlier

# names(train.data[,-1])

# names(test.data[,-1])
Y.test <- log(test.data[,1])
X.test <- test.data[,-1]

y.hat.test <- predict(bad.final.model,newdata = X.test)
MSPR <- mean((log(Y.test)-y.hat.test)^2)
MSPR


# 

#DFFITS
p <- 11
n<- length(train.data$edu)
dffits(final.model)
ggplot(data = data.frame(dffits(final.model)), aes(x = 1:length(dffits(final.model)), 
     y = dffits(final.model))) + geom_point(shape=1) + 
     geom_abline(slope = 0, intercept = 2*sqrt(p/n), color="blue") + 
     geom_abline(slope = 0, intercept = -2*sqrt(p/n), color="blue")

plot(dffits(final.model),main="DFFITS",ylim=c(-0.25,0.25))
abline(h=2*sqrt(p/n), col="red")
abline(h=-2*sqrt(p/n),col="red")


#Cooks
cooks.distance(final.model)
plot(cooks.distance(final.model),main="Cook's Distance",ylim=c(-.1,1))
f.star=qf(.5,df1=p,df2=n-p)
abline(h=f.star, col=2)
abline(h=0,lty=2, col=2)
ggplot(data = data.frame(cooks.distance(final.model)), aes(x = 1:length(cooks.distance(final.model)), 
  y = cooks.distance(final.model))) + geom_point(shape=1) + 
  geom_abline(slope = 0, intercept = f.star, color="blue") + 
  geom_abline(slope = 0, intercept = 0, color="blue")


#DFBETAS
#DFBETAS-race.other
n1 <- dim(train.data)[1]
dfbeta2 <- dfbetas(final.model)[,10]
plot(dfbeta2,main="DFBETAS-race.other",ylim=c(-.15,.15),
     ylab="dfbetas(other)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
ggplot(data = data.frame(dfbeta2), aes(x = 1:length(dfbeta2), y = dfbeta2)) + geom_point(shape=1) + 
  geom_abline(slope=0, intercept=2/sqrt(n1), color="blue")+
  geom_abline(slope=0, intercept=-2/sqrt(n1), color="blue")

#DFBETAS-race.white
dfbeta3 <- dfbetas(final.model)[,9]
plot(dfbeta3,main="DFBETAS-white",ylim=c(-.15,.15),
     ylab="dfbetas(white)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
ggplot(data = data.frame(dfbeta3), aes(x = 1:length(dfbeta3), y = dfbeta3)) + geom_point(shape=1) + 
  geom_abline(slope=0, intercept=2/sqrt(n1), color="blue")+
  geom_abline(slope=0, intercept=-2/sqrt(n1), color="blue")



#Research Question 1
final.model.reduced1 <- lm(log(wage)~ edu+I(edu^2)+exp+I(exp^2)+city+reg.northeast+reg.south+
               emp+race.other, data= train.data)
summary(final.model.reduced1)
plot(log(train.data$wage), rstudent(final.model.reduced1))
qqnorm(rstudent(final.model.reduced1))
qqline(rstudent(final.model.reduced1), col="red")
AIC(final.model.reduced1)
# anova(final.model.reduced1)
anova(final.model.reduced1, final.model)

#Research Question 2-model.7.r2
final.model.reduced2 <- lm(log(wage)~ edu+I(edu^2)+exp+I(exp^2)+city+reg.northeast+reg.south
                 +emp, data= train.data)
summary(final.model.reduced2)
plot(log(train.data$wage), rstudent(final.model.reduced2))
qqnorm(rstudent(final.model.reduced2))
qqline(rstudent(final.model.reduced2), col="red")
AIC(final.model.reduced2)
anova(final.model.reduced2)
anova(final.model.reduced2, final.model)

