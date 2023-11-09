Event <- read.csv("/Users/wilsontai/Downloads/overview_of_events_datasets (1).csv")
attach(Event)
View(Event)

##loading libraries
library(stargazer)
library(lmtest)
library(sandwich)
library(portes)
library(Hmisc)
library(PerformanceAnalytics)
library(olsrr)
library(plyr)
library(car)
library(ggcorrplot)


##getting general overview
head(Event)
tail(Event)
summary(Event)
str(Event)
dim(Event)
colSums(is.na(Event)) ##are there any NAs?

##Re-naming columns more appropriately
colnames(Event)[2] = 'Total_Tweets'
colnames(Event)[3] = 'Emotional_Tweets'
colnames(Event)[4] = 'Emotional_Tweets_Percentage'
colnames(Event)[5] = 'Type'

# Converting all 'character' variables to 'factor'
Event <- as.data.frame(unclass(Event),         
                       stringsAsFactors = TRUE)
Event$Total_Tweets <- as.numeric(Event$Total_Tweets)

#Checking they are now factors; and columns are corrected
str(Event)
head(Event)


###Analysis

##Graphics 
par(mfrow = c(1,1))
boxplot(Event$Emotional_Tweets, col = "blue", main = "Box Plot for Emotional Tweets Percentage")
abline(h=quantile(Event$Emotional_Tweets,0.25), col="red", lty=2)
abline(h=quantile(Event$Emotional_Tweets,0.75), col="red", lty=2)

boxplot(Event$Total_Tweets, col = "blue", main = "Box Plot for Total Tweets")
abline(h=quantile(Event$Total_Tweets,0.25), col="red", lty=2)
abline(h=quantile(Event$Total_Tweets,0.75), col="red", lty=2)


par(mfrow = c(1,2))
hist(Event$Emotional_Tweets, col = "blue", 
     xlab = "Percentage of Emotional Tweets", main = "Histogram of Emotional Tweets")
options(scipen=3)
hist(Event$Total_Tweets, col = "blue", 
     xlab = "Total Tweets per Event", main = "Histogram of Total Tweets",
     ylim =c(0,20))

#changing plot layout so all three graphs could fit on one plot
par(mfrow = c(1,3))
par(oma=c(0,0,4,0))

##changing variables as factor to be able to graph it
Event$Sarcasm <- as.factor(Event$Sarcasm)
Event$Irony <- as.factor(Event$Irony)
Event$Humour <- as.factor(Event$Humour)

##graphing feelings
plot(Event$Sarcasm, col = "blue", ylim = c(0,20), ylab = "Frequency",
     main = "Sarcasm Frequency")
plot(Event$Irony, col = "red", ylim = c(0,25), ylab = "Frequency",
     main = "Irony Frequency")
plot(Event$Humour, col = "orange", ylim = c(0,20), ylab = "Frequency",
     main = "Humour Frequency")
mtext(~bold("Number of Times per Event"), # Add main title
      side = 3,
      line = 0,
      cex = 2,
      outer = TRUE)
##Setting back to original layout
par(mfrow = c(1,1)) 
par(oma=c(0,0,0,0))

##Proportion
emotion_total <- sum(Sarcasm) + sum(Irony) + sum(Humour)
proportion <- function(x){
  sum<-0
  for(i in 1:(length(x))){ ##beginning from first observation
    sum=sum+x[i] ##using a loop to sum first obs with the 0, and then the next one, and so on
  }
  proportion = sum/emotion_total ##grab for loop results and find proportion
  return(proportion)
}
proportion(Sarcasm)
proportion(Irony)
proportion(Humour)

##ensuring my above are correct
sum(Sarcasm)/emotion_total
sum(Irony)/emotion_total
sum(Humour)/emotion_total

##Regression
##turning back to factors
Event$Sarcasm <- as.factor(Event$Sarcasm)
Event$Irony <- as.factor(Event$Irony)
Event$Humour <- as.factor(Event$Humour)
##Creating Dummies to make Regression work
Event$SarcasmDummy <- ifelse(Event$Sarcasm != 0, 1, 0)
Event$IronyDummy <- ifelse(Event$Irony != 0, 1, 0)
Event$HumourDummy <- ifelse(Event$Humour != 0, 1, 0)

##subsetting dataset to have a look at correlations: ensure no multicolliinearity
EventNumeric <- dplyr::select_if(Event, is.numeric)
head(EventNumeric) ##looking at top few observations to ensure everything is correct
EventNumeric <- EventNumeric[,-2] ##can eliminate one of the emotional_tweets
head(EventNumeric) ##ensuring it worked
str(EventNumeric) ##checking structure

rcorr(as.matrix(EventNumeric)) ##correlation matrix
eventnumeric_cor <- cor(EventNumeric)
ggcorrplot(eventnumeric_cor,
           title = "Correlation Matrix",
           lab = TRUE) ##correlation plot

##Running Regression
lm1 <- lm(Emotional_Tweets_Percentage ~ Total_Tweets + SarcasmDummy + 
            IronyDummy + HumourDummy, Event)
bptest(lm1) ##Heteroskedasticity test
dwtest(lm1) ##autocorrelation test
vif(lm1) ##all less than the rule of thumb of 5: multicollinearity not present
summary(lm1) ##no interesting results: not statistically significant

lm2 <- lm(Total_Tweets ~ Emotional_Tweets_Percentage +SarcasmDummy + 
            IronyDummy + HumourDummy, data = Event) ##linear regression
poisson1 <- glm(Total_Tweets ~ Emotional_Tweets_Percentage +SarcasmDummy + 
                  IronyDummy + HumourDummy, data = Event, family = "poisson") ##poisson regression
stargazer(lm2, poisson1, 
          title = "OLS vs Poisson",
          dep.var.labels.include = FALSE,
          dep.var.caption  = "Dependent Variable: Total Tweets",
          type = "text")


##Package
#lmtest
#Part 1: Describe package
#Part 2: Talk about heteroskedasticity, autocorrelation and RESET Test
library(lmtest)
reset(Emotional_Tweets_Percentage ~ Total_Tweets + SarcasmDummy + 
        IronyDummy + HumourDummy, data = Event, power = 2:3)
Total_Tweets2 <- Event$Total_Tweets^2
reset(Emotional_Tweets_Percentage ~ Total_Tweets + Total_Tweets2 + SarcasmDummy + 
        IronyDummy + HumourDummy, data = Event, power = 2:3)


lm1 <- lm(Emotional_Tweets_Percentage ~ Total_Tweets + Total_Tweets2 + SarcasmDummy + 
            IronyDummy + HumourDummy, Event)
bptest(lm1)
bgtest(lm1)
summary(lm1)

lm1robust <- coeftest(lm1, function(x) vcovHC(x, type="HC0"))
lm1robust
summary(lm1)
stargazer(lm1, lm1robust, 
          title = "OLS vs Robust Standard Errors",
          dep.var.labels.include = FALSE,
          column.labels=c("OLS","Robust Standard Errors"), 
          dep.var.caption  = "Dependent Variable: Emotional Tweets Percentage",
          type = "text", model.names = FALSE)


## Function
####
attach(EventNumeric)
EventNumeric <- as.list(EventNumeric) ##creating EventNumeric as 'list' just like was taught in class
class(EventNumeric) <- "events"
is.list(EventNumeric) ## did my list function really work?
class(EventNumeric) ##ensuring it is now a list called 'events'
str(EventNumeric)

#####
##summary
summary.events <- function(x){
  print(length(x[[1]])) ##number of observations
  ##function for mean, median, min, max and quantiles
  multiple.func <- function(x) {
    c(min = min(x), mean = mean(x), max = max(x), 
      quantile = quantile(x, probs = c(0.25, 0.5, 0.75))) 
  }
  x <- lapply(x, multiple.func) ##applying this to list
  return(x) ##spitting results out
}
summary(EventNumeric)

##Plot
plot.events <- function(EventNumeric){
  par(mfrow = c(1,2)) #one row two columns
  hist(EventNumeric$Total_Tweets, xlab = "Total_Tweets", main =  "Total_Tweets", 
       col = "blue")
  hist(EventNumeric$Emotional_Tweets_Percentage, xlab = "Emotional Tweets (Percentage)", 
       main = "Emotional Tweets",
       col = "blue")
  minmax <- function(x){
    c(min = min(x), max = max(x), range = max(x) - min(x)) ##producing an overview of range
  }
  return(lapply(EventNumeric[1:2], minmax))
}
plot(EventNumeric)

##print
print.events <- function(x){
  return(t(do.call(rbind, x)))
}
print(EventNumeric)


#######
GaussMarkov <- function(y, ...){ ##y is dependent variable, '...' is one or more independent variables
  x <- data.frame(...) ##creating a dataframe
  lm <- lm(y ~ ., data = x) ##running regression
  ehat <- lm$resid ##retrieving residuals, all the unexplained variables agglomerated into
  yhat <- lm$fitted.values ##predicted values
  print(bptest(lm)) ##heteroskedasticity test
  print(dwtest(lm)) ##autocorrelation test (Durbin Watson)
  print(bgtest(lm)) ##autocorrelation test (Breusch Godfrey)
  print(ols_test_normality(ehat)) ##normality test
  par(mfrow = c(2,2)) ##setting plot layout
  plot(yhat, ehat, col = "blue",
       main = "Heteroskedasticity and Normality Graph",
       xlab = "Fitted Values", ylab = "Residuals") ##Heteroskedasticity and Normality Graph
  acf(ehat, main = "Autocorrelation") ## Autocorrelation Graph
  hist(ehat, xlab = "Residuals",
       main = "Histogram of Residuals", col = "blue") ##histogram of residuals: 
  ##should be normally distributed
  plot(yhat, y, main= "Fitted vs Actual", 
       xlab = "Fitted Values", ylab = "Actual",
       pch = 19,
       col = "blue")
  abline(a = 0, b = 1,col='red') ##a plot of heteroskedasticity
}
GaussMarkov(Emotional_Tweets_Percentage, SarcasmDummy, Total_Tweets, IronyDummy, HumourDummy) ##testing


best_model <- function(y,...){
  x <- data.frame(...)
  lm <- lm(y ~ ., data = x)
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) 
  ##use function (more specifically the bptest function) to 
  ##grab a value that is not considered a number when we use a function
  BPpvalue <- specify_decimal(bptest(lm)$p.value, 5) 
  ###creating object called BPvalue and rounding it to 5 decimal points
  if(BPpvalue > 0.05){ ##heteroskedasticity test: we want to fail this test
    print("Failed to reject null that model is homoskedastic")
    print(paste0("Breusch Pagan P-Value is ", BPpvalue))  
    pvalue <- as.data.frame(summary(lm)$coefficients[,4])[-1, ] 
    ##getting p-values for significance from lm but 
    #remove intercept p-value: intercept is only there to make equation work
    if(y %% 1 == 0){ ##count variables dont have remainders
      poisson <- glm(y~., data = x, family = "poisson")
      pvalue <- as.data.frame(summary(poisson)$coefficients[,4])[-1, ]
      ##getting pvalues for significance from poisson but remove intercept p-value
      if(pvalue < 0.05){
        summary(poisson)}
    }
    else if (y %% 1 != 0 & pvalue < 0.05) {  ##OLS is versatile: try with OLS
      ##also, this p-value follows the OLS one, not poisson, as its inside different statements
      summary(lm) ##show this model (OLS)
    }
    ##But what if the above conditions dont hold? Then:
    else {
      print("No good model: OLS not significant and Dependent Variable is not count variable")
    }
  }
  ##If BP > 0.05, so heteroskedasticity is present, then:
  else{
    print("Reject null that model is homoskedastic 
          and correct with robust standard errors") 
    #if we reject Breusch Pagan Test, then our model 
    #is heteroskedastic and we thus need a robust standard errors model
    print(paste0("Breusch Pagan P-Value is ", BPpvalue))    
    lmrobust <- coeftest(lm, function(x) vcovHC(x, type="HC0"))
    print(lmrobust)
  }
}
best_model(Total_Tweets, SarcasmDummy, 
           Emotional_Tweets_Percentage, IronyDummy, HumourDummy) ##Poisson Example
best_model(Emotional_Tweets_Percentage, 
           SarcasmDummy, HumourDummy, IronyDummy, Total_Tweets) ##OLS/Insignificance example
##OLS dont work, double check to make sure
set.seed(123)
n = 1000
x <- 1:n
sd<-runif(n, min=0, max=5)
error<-rnorm(n,0,sd*x) 
y <- x+error
par(mfrow =c(1,1))
plot(x,y, main = "Sample Heteroskedasticy")    
abline(0,1, col="red")
best_model(y, x) ##doesnt work

