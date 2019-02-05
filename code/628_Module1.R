library(tidyverse)
library(knitr)
library(kableExtra)
library(reshape)
library(corrplot)
library(dplyr)
library(ggplot2)
library(car)


# Data Cleaning -----------------------------------------------------------

#Change the following code to set your own directory
setwd("/Users/moran/Documents/GitHub/Stat628/Data")

options(digits = 2)

dat = read.csv('BodyFat.csv') %>%
  .[,-1]

#Outlier Detection

is_outlier <- function(x) {#Function to detect outlier by percentile
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

clean_and_plot <- function(dt){
  k = nrow(dt)
  dat_tmp = dt %>%
    scale(.) %>%
    as.data.frame(.) %>%
    melt(.) %>%
    group_by(variable) %>%
    mutate(outlier = ifelse(is_outlier(value), value, as.numeric(NA)))
  
  for(i in 1:nrow(dat_tmp)){ #Change the outlier value to their index
    if(!is.na(dat_tmp$outlier[i])) 
      dat_tmp$outlier[i] = i %% nrow(dt)
  }
  
  p1 = ggplot(data = dat_tmp,aes(x = variable, y = value))+ #Boxplot with outlier indexed
    geom_boxplot(alpha = 0.7)+
    geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.8)
  return(list(p1, dat_tmp, k)) #Return Original Boxplot, transformed data matrix and number of rows.
}

tidyup <- function(dt,s){ # Try to put red color on the user-defined observations
  dat_tmp = dt[[2]]
  dat_tmp$idx = rep(1:dt[[3]],16)
  idx_save = c()
  for(i in 1: nrow(dat_tmp)){
    if(!is.na(dat_tmp$outlier[i]) & dat_tmp$outlier[i] %in% s){
      idx_save = c(idx_save ,i)
    }
  }
  p1 = dt[[1]] + geom_point(data = dat_tmp[idx_save,], color = 'red', shape = 18, size = 5)
  return(p1)
}
tidyup(clean_and_plot(dat), c(39,41,42))
ggsave('boxplot1.png', tidyup(clean_and_plot(dat), c(39,41,42)), device = 'png',width = 10, height = 8)

#Delete 39 and 41, they are over-weighted and can be seen as outlier in many variables.
#Delete 42. Because this man only 70cm tall.
dat <- dat[-c(39, 41, 42),]
tidyup(clean_and_plot(dat), c(31, 83))
ggsave('boxplot2.png', tidyup(clean_and_plot(dat), c(31,83)), device = 'png',width = 10, height = 8)

#Delete 31 and 83 because it does not seem normal for these two people have such huge ANKLEs
dat <- dat[-c(31, 83),]
tidyup(clean_and_plot(dat), c())
ggsave('boxplot3.png', tidyup(clean_and_plot(dat), c()), device = 'png',width = 10, height = 8)

#I think we are fine right now.



# The Inconsistent between real BODYFAT and the BODYFAT calculated

df = data.frame(Real = dat$BODYFAT, Fit = 495 / dat$DENSITY - 450)
is_outlier2 <- function(x) {#Function to detect outlier by standard deviation
  return(x < mean(x) - 2 * sd(x) | x > mean(x) + 2 * sd(x)) #Use normal assumption to detect outlier
}
df_tmp = df %>%
  mutate(dif = (Fit - Real)) %>%
  mutate(outlier = ifelse(is_outlier2(dif), 1,  as.numeric(NA)))

for(i in 1:nrow(df_tmp)){ #Change the outlier value to their index
  if(!is.na(df_tmp$outlier[i])) 
    df_tmp$outlier[i] = i %% nrow(df_tmp)
}
df$outlier = df_tmp$outlier
p1 = ggplot(data = df, aes(x = Real, y = Fit))+
  geom_point()+
  geom_point(data = df[!is.na(df$outlier),], color = 'red')+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.2, color = 'red')+
  labs(x = 'Real BODYFAT', y = 'BODYFAT from DENSITY')
ggsave('BODYFAT.png', p1, device = 'png',width = 8, height = 6)

#We do regression on these four points.
dat_train = dat[-c(44, 72, 91, 177),]
dat_test = dat[c(44, 72, 91, 177),]
fit = lm(BODYFAT~.-DENSITY, data = dat_train)
table1 = rbind(dat_test$BODYFAT, predict(fit, dat_test), 495 / dat_test$DENSITY - 450)
rownames(table1) = c('Real BODYFAT', 'lm Prediction', 'BODYFAT from DENSITY')
colnames(table1) = c(44, 72, 91, 177)
kable(table1)%>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F)%>%
  column_spec(4, bold = T, color = "white", background = "coral")


#I tend to believe that 91 data has the real BODYFAT.
#Delete 44, 72 and 177

dat <- dat[-c(44, 72,177),]

# The Inconsistent between Weight, Height and BMI(ADIPOSITY)
df = data.frame(Real = 703 * dat$WEIGHT / dat$HEIGHT ^ 2, Fit = dat$ADIPOSITY)

df_tmp = df %>%
  mutate(dif = (Fit - Real)) %>%
  mutate(outlier = ifelse(is_outlier2(dif), 1,  as.numeric(NA)))

for(i in 1:nrow(df_tmp)){ #Change the outlier value to their index
  if(!is.na(df_tmp$outlier[i])) 
    df_tmp$outlier[i] = i %% nrow(df_tmp)
}
df$outlier = df_tmp$outlier
p1 = ggplot(data = df, aes(x = Real, y = Fit))+
  geom_point()+
  geom_point(data = df[!is.na(df$outlier),], color = 'red')+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.2, color = 'red')+
  labs(x = 'Real BMI', y = 'BMI from ADIPOSITY')
ggsave('BMI.png', p1, device = 'png',width = 8, height = 6)

#We do regression on these two points.
dat_train = dat[-c(156, 213),]
dat_test = dat[c(156, 213),]
fit = lm(BODYFAT~.-DENSITY-ADIPOSITY-HEIGHT-WEIGHT, data = dat_train)
table1 = rbind(703 * dat_test$WEIGHT / dat_test$HEIGHT ^ 2, predict(fit, dat_test), dat_test$ADIPOSITY)
rownames(table1) = c('Real BMI', 'lm Prediction', 'BMI from ADIPOSITY')
colnames(table1) = c(156, 213)
kable(table1)%>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F)
#I do not trust any of these
#Delete 156 and 213

dat <- dat[-c(156, 213),-2]

#Now our data has been cleaned, we remove DENSITY as well.

write.csv(dat,'clean.csv', row.names = F)


# Stepwise Variable Selection ------------------------------------------------------
dat = read.csv('clean.csv')
step(lm(BODYFAT ~ ., data = dat), direction = "both", k = 2, trace = 0)
df <- data.frame(v = vif(lm(BODYFAT ~ AGE + HEIGHT + ADIPOSITY + THIGH + WEIGHT + ABDOMEN + WRIST, data = dat)))
df <- data.frame(variable = rownames(df), vif = vif(lm(BODYFAT ~ AGE + HEIGHT + ADIPOSITY + THIGH + WEIGHT + ABDOMEN + WRIST, data = dat)))
ggplot(data = df, aes(x = variable, y = vif))+
  geom_point(aes(color = vif, fill = vif),shape = 22, size = 5)+
  geom_hline(yintercept = 10, linetype = 'dashed', color = 'red')


# Single variable enumeration ------------------------------------------------------

#Concerning about the simpleness, we try models with single variable. 
#The products or divisions of two variables are also recognized as single variables.
#Using leave-one-out cross validation to select models with lowest MSE.
cv_single <- function(i){
  mse <- 0
  for(k in 1:242){
    train <- dat[-k,]
    test <- dat[k,]
    coef <- lm(BODYFAT ~ train[,i], train)$coef
    y_hat <- coef%*%c(1, test[,i])
    mse <- mse + (y_hat - test$BODYFAT)^2
  }
  return(mse/242)
}

cv_div <- function(i,j){
  mse <- 0
  for(k in 1:242){
    train <- dat[-k,]
    test <- dat[k,]
    aa <- train[,i]/train[,j]
    coef <- lm(BODYFAT ~ aa, train)$coef
    y_hat <- coef%*%c(1, test[,i]/test[,j])
    mse <- mse + (y_hat - test$BODYFAT)^2
  }
  return(mse/242)
}

cv_mul <- function(i,j){
  mse <- 0
  for(k in 1:242){
    train <- dat[-k,]
    test <- dat[k,]
    aa <- train[,i]*train[,j]
    coef <- lm(BODYFAT ~ aa, train)$coef
    y_hat <- coef%*%c(1, test[,i]*test[,j])
    mse <- mse + (y_hat - test$BODYFAT)^2
  }
  return(mse/242)
}

min_mul <- Inf
for(i in 2:15){
  for(j in 2:15){
    if(i==j)
      next
    temp <- cv_mul(i,j)
    if(temp< min_mul){
      var_mul1 <- names(dat)[i]
      var_mul2 <- names(dat)[j]
      min_mul <- temp
    }
  }
}

min_div <- Inf
for(i in 2:15){
  for(j in 2:15){
    if(i==j)
      next
    temp <- cv_div(i,j)
    if(temp< min_div){
      var_div1 <- names(dat)[i]
      var_div2 <- names(dat)[j]
      min_div <- temp
    }
  }
}

min_single <- Inf
for(i in 2:15){
  temp <- cv_single(i)
  if(temp< min_single){
    var_single <- names(dat)[i]
    min_single <- temp
  }
}

mse <- matrix(c(min_single, min_mul, min_div))
rownames(mse) <- c(var_single, paste(var_mul1,"*",var_mul2, sep = ""), paste(var_div1,"/",var_div2, sep = ""))
colnames(mse) <- "MSE of CV"

kable(mse)%>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F)
#The table shows that the lowest MSE belongs to the model with ABDOMEN/HEIGHT as the variable.


#Campare the R^2 of the model with four independent variables mentioned above and the model
#with ABODMEN/HEIGHT as the independent variable
#The simpler model's R^2 is not less than the complicated model's very much.
#So we choose the second model as the final model.
ratio <- dat$ABDOMEN/dat$HEIGHT
r.squared <- matrix(c(summary(lm(dat$BODYFAT ~ ratio))$r.squared, summary(lm(BODYFAT ~ AGE + HEIGHT + ABDOMEN + WRIST, data = dat))$r.squared))
colnames(r.squared) <- "R^2"
rownames(r.squared) <- c("BODYFAT ~ ABDOMEN/HEIGHT", "BODYFAT ~ AGE + HEIGHT + ABDOMEN + WRIST")
kable(r.squared)%>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F)


# Diagnostics ------------------------------------------------------

d=dat
d$AH=d$ABDOMEN/d$HEIGHT
modelx=lm(d$BODYFAT~d$AH)
d$predict=predict(modelx)
d$resid=resid(modelx)
d$stanresid=rstandard(modelx)
d$n=c(1:242)
d$cook=cooks.distance(modelx)
d$cookspecial=c(rep(0,227),1,rep(0,11),1,rep(0,2))

## In this step,we will visualize data then check these plots to ensure linear model works.
##We want to recheck whether there's still influncial point, two point has a relatively hight cook's distance
ggplot(d,aes(n,cook,color=factor(cookspecial)))+
  geom_point(shape=17,size=2)+
  theme(legend.position='none')+
  labs(x = "Index(Each Observation)", y = "Distance")
##,title="Influence Values (Cook's Distance)"

##Next we will diagnostic the result of model
##Next we draw the scatterplot of Bodyfat vs Abdomen/Height
##Linearity:  Reasonable (both based on diagnostic residual plot and original scatterplot), and it's intuitive that BodyFat% increase given Height decrease and Abdomen increase. 
ggplot(d, aes(AH, BODYFAT))+
  geom_point(shape=17,color="red")+
  labs(x = "Abdomen/Height (cm/inch)", y = "BODYFAT(%)")+
  geom_abline(intercept = -39.8,slope =44.7,color="blue")+
  annotate("text",x=1.2,y=c(41,39),label=c("Regression Line:","slope=  44.7 ,intercept=  -39.8"),size=5)
##,title="Scatterplot of BodyFat% and Abodomen/Height"
##Here is the Residuals vs predict plot, based on this graph and the result of shapiro.test, the residuals may fits normal distribution.
##Constant variance: The variance seems constant in most places, however, when A/H gets larger, we couldn't ensure it is still constant because of the lack of data.

ggplot(d,aes(predict,resid))+ 
  geom_point(shape=17,color="red")+
  labs(x = "Predicted BodyFat (%)", y = "Residuals (%)")+
  geom_abline(intercept = 0,slope =0,color="black")
##,title="Residual Plot"
##Then we draw a normal QQ plot, the result looks good
##Normally distributed residuals: Seems good from the  diagnostics and plots.
qplot(sample=stanresid, data=d,color="red")+
  geom_abline(intercept = 0,slope =1)+
  theme(legend.position='none')+
  annotate("text",x=c(-2,-2),y=c(2,1.4),label=c("p-value(Shapiro.test):","0.46"),color=c("black","darkgreen"),size=6)



