library(tidyverse)
library(olsrr)
library(glmnet)
library(randomForest)
library(caret)
library(esquisse)
library(pdp)
library(Metrics)
############################ Cleaning #####################
options(scipen=999)
raw = read.csv("Clean2.csv", header = TRUE,stringsAsFactors = FALSE)
data = raw
str(data)
data[which(substring(data$Subdivision,1,5) == "Green"),"Subdivision"] = "Greenleaves"
data[which(substring(data$Subdivision,1,3) == "New"),"Subdivision"] = "New Golden Shores"
data[which(substring(data$Foundation,1,6) == "Raised"),"Foundation"] = "Raised"
data[,c("Subdivision","Foundation","Cond")] = lapply(data[,c("Subdivision","Foundation","Cond")],as.factor)
data = data %>% filter(Foundation == "Raised" | Foundation == "Slab") 
data = data %>% mutate(Sold.Date = str_sub(Sold.Date,start=-2))
data$Sold.Date = as.numeric(data$Sold.Date) + 2000
#Considering Houses Built after 1900
data = data %>% filter(Age<140)
#Adjusting for Inflation at 2% annually
data = data %>% group_by(Sold.Date) %>% mutate(Sold.Price.Adjusted = Sold.Price*(1.02**(2020-Sold.Date))) %>% ungroup()
#Missing values (later)
sapply(data, function(x) sum(is.na(x)))
##Missing Values
missing = data[which(is.na(data$Baths.Half)),c("Subdivision",
                                               "Approx.Living.Area","Beds.Total",
                                               "Baths.Full","Baths.Half")]

imputing_data = data[-which(is.na(data$Baths.Half)),c("Subdivision",
                                                      "Approx.Living.Area","Beds.Total",
                                                      "Baths.Full","Baths.Half")] %>% filter(((Subdivision =="Beau Chene" & Beds.Total==5 & (Baths.Full==4 | Baths.Full==5))| 
                   (Subdivision == "Greenleaves" & Beds.Total==4 & (Baths.Full==4 | Baths.Full==2))))
APA_imputing = missing$Approx.Living.Area
t1 = imputing_data %>% filter(Subdivision=="Beau Chene" & Baths.Full == "4")
t1$APA_imputing = APA_imputing[1]
t2 = imputing_data %>% filter(Subdivision=="Beau Chene" & Baths.Full == "5")
t2$APA_imputing = APA_imputing[2]
t3 = imputing_data %>% filter(Subdivision=="Greenleaves" & Baths.Full == "2")
t3$APA_imputing = APA_imputing[3]
t4 = imputing_data %>% filter(Subdivision=="Greenleaves" & Baths.Full == "4")
t4$APA_imputing = APA_imputing[4]
imputing_data = rbind(t1,t2,t3,t4)
#1/2 Baths by neighborhood by full baths (full beds is same so convenient)
imputing_data %>% ggplot(aes(x=Baths.Half)) +geom_bar()+facet_grid(~Subdivision + Baths.Full)
#Approximate Living Area by neighborhood by full bath (full beds is same so convenient)
imputing_data %>% ggplot(aes(x=Approx.Living.Area)) +geom_histogram()+ geom_vline(aes(xintercept=APA_imputing)) +facet_grid(~Subdivision + Baths.Full)
# mode for t1, mode for t2, max for t3, min for t4
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
imputing_data = imputing_data %>% group_by(Subdivision,Baths.Full) %>% 
  mutate(Mode = Mode(Baths.Half),Min = min(Baths.Half),Max = max(Baths.Half))
data[which(is.na(data$Baths.Half))[1],"Baths.Half"] = 1 #mode of BC 4
data[which(is.na(data$Baths.Half))[2],"Baths.Half"] = 1 #mode of BC 5
data[which(is.na(data$Baths.Half))[1],"Baths.Half"] = 1 #max of GL 2
data[which(is.na(data$Baths.Half))[1],"Baths.Half"] = 0 #min of GL 4

#Drop Unused Levels from cleaning
data = droplevels.data.frame(data)

################################ EDA ############################# 
##Raised Houses Trend
#Trend of Raised Houses Built Over Time
Build_Raised_Plot = data  %>% mutate(Build.Year = Sold.Date - Age)  %>% filter(Build.Year > 2000) %>% 
  group_by(Build.Year,Foundation) %>% summarise(n=n()) %>% 
  mutate(freq = (n / sum(n))*100) %>% filter(Foundation=="Raised") %>% 
  ggplot(aes(x=Build.Year,y=freq)) +geom_bar(stat="identity",fill="steelblue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_vline(aes(xintercept=2005)) + 
  labs(y = "Percentage of Houses", x = "Year Built") +
  ggtitle("Trend of Raised Houses Built Over Time")+ theme_bw()
plot(Build_Raised_Plot)

#Trend of Raised Houses Sold Over Time
Sold_Raised_Plot = data %>% mutate(Sold.Date=as.factor(Sold.Date)) %>% group_by(Sold.Date,Foundation) %>% 
  summarise(n=n()) %>% mutate(freq = (n / sum(n))*100) %>% filter(Foundation=="Raised") %>%
  mutate(label = paste(round(freq,digits=1),"%")) %>% 
  ggplot(aes(x=Sold.Date,y=freq)) + geom_bar(stat="identity",fill="steelblue") + 
  geom_text(aes(x=Sold.Date,y=freq,label=label)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "Percentage of Houses", x = "Year Sold") +
  ggtitle("Trend of Raised Houses Sold Over Time") + theme_bw()
plot(Sold_Raised_Plot)

## Neighborhood Value 
#Mean Price Per Subdivision
Price_Neighborhood_Plot = data %>%  mutate(Sold.Price.Adjusted = Sold.Price.Adjusted/1000000) %>% filter(Sold.Price.Adjusted < 1.5) %>% 
  ggplot( aes(x=Sold.Price.Adjusted, color=Subdivision, fill=Subdivision)) +
  geom_density() + facet_wrap(~Subdivision) +
  labs(y = "Density", x = "Sale Price (In Millions)") +
  ggtitle("Sale Price Across Subdivisions Adjusted for 2% Annual Inflation") + theme_bw()
plot(Price_Neighborhood_Plot)

#Number of Bedrooms in Neighborhood
Bed_Neighborhood_Plot = data %>% group_by(Subdivision,Beds.Total) %>% 
  summarise(n=n())%>% 
  mutate(freq = (n / sum(n))*100) %>% 
  mutate(label = paste(round(freq,digits=1),"%")) %>% 
  ggplot(aes(x=Beds.Total,y=freq,color=Subdivision,fill=Subdivision)) + 
  geom_bar(stat="identity") + facet_wrap(~Subdivision) +
  geom_text(aes(x=Beds.Total,y=freq,label=label),color="black") +
  labs(y="Count",x="Total Number of Bedrooms") +
  ggtitle("Bedroom Totals in Sales Across Subdivisions")+ theme_bw()
plot(Bed_Neighborhood_Plot)

#Number of Bathrooms in Neighborhood
Bathroom_Neighborhood_Plot = data %>% 
  mutate(Bathrooms = as.numeric(data$Baths.Full) + 0.5*as.numeric(data$Baths.Half)) %>% 
  group_by(Subdivision,Bathrooms) %>% summarise(n=n())%>% 
  mutate(freq = (n / sum(n))*100) %>% mutate(label = paste(round(freq,digits=1))) %>% 
  ggplot(aes(x=Bathrooms,y=freq,color=Subdivision,fill=Subdivision)) + 
  geom_bar(stat="identity") + facet_wrap(~Subdivision) +
  geom_text(aes(x=Bathrooms,y=freq,label=label),color="black") +
  labs(y="Count",x="Total Number of Bathrooms") +
  ggtitle("Bathrooms Totals in Sales Across Subdivisions")+ theme_bw()
plot(Bathroom_Neighborhood_Plot)

##Purely Home Value
#Mean Price Per number of bedrooms
Price_Bedroom_Plot = data %>%  mutate(Sold.Price.Adjusted = Sold.Price.Adjusted/1000000) %>% filter(Sold.Price.Adjusted < 1.5) %>% 
  ggplot( aes(x=Sold.Price.Adjusted,fill=Beds.Total,color=Beds.Total)) +
  geom_density() + facet_wrap(~Beds.Total) +
  labs(y = "Density", x = "Sale Price (In Millions)") +
  ggtitle("Sale Price Across Number of Bedrooms Adjusted for 2% Annual Inflation") + theme_bw()
plot(Price_Bedroom_Plot)

#Relationship between ALA and Sale Price appears to be non-linear
ALA_Sale_Plot = data  %>%  ggplot(aes(x=Approx.Living.Area,y=Sold.Price.Adjusted)) +
  geom_point(size=.3)  + geom_smooth(method=lm,size=1,se=FALSE) + 
  geom_smooth(color="red",se=FALSE,size=1) + 
  labs(y = "Total Sale Price",x="Approximate Living Area") + theme_bw()
plot(ALA_Sale_Plot)

#Relationship between Age and Sale Price appears to be non-linear
Age_Sale_Plot = data  %>%  ggplot(aes(x=Age,y=Sold.Price.Adjusted)) +
  geom_point(size=.3)  + geom_smooth(method=lm,size=1,se=FALSE) + 
  geom_smooth(color="red",se=FALSE,size=1) + 
  labs(y = "Total Sale Price",x="Age") + theme_bw()
plot(Age_Sale_Plot)

#Relationship between ALA and Number of Bedrooms
ALA_Bed_Plot = data %>% ggplot(aes(x=Approx.Living.Area,y=Beds.Total)) +
  geom_point(size=.2) + labs(y = "Total Bedrooms",x="Approximate Living Area") 
+ theme_bw()
plot(ALA_Bed_Plot)  
  

################################### Modeling on Entire Data Set ###############################
##OLS
#Set Reference Groups
data <- within(data, Subdivision <- relevel(Subdivision,ref="Beau Chene"))
data <- within(data, Foundation <- relevel(Foundation, ref = "Slab"))
# Since number of observations is so small for raised houses will examine variable 'significance'
# in model on full data set
model_ols = lm(Sold.Price.Adjusted ~ Subdivision + Baths.Full + 
                 Baths.Half + Beds.Total  + Age  + Approx.Living.Area + 
                 Cond + Foundation + Foundation:Subdivision,data=data)
summary(model_ols)
View(ols_coll_diag(model_ols))

#EDA for Agnostic Feature Reduction
raised_plot = data %>% filter(Subdivision=="Riverwood" | Subdivision == "Lewisburg") %>% 
  mutate(Sold.Price.Adjusted = Sold.Price.Adjusted/1000000) %>% 
  ggplot(aes(x=Sold.Price.Adjusted,color=Foundation)) + geom_density() +
  facet_wrap(~Subdivision) +  labs(y = "Density", x = "Sale Price (In Millions)") +
  ggtitle("Sale Price Across Subdivisions Adjusted for 2% Annual Inflation") + theme_bw()
plot(raised_plot)

#EDA for Agnostic Feature Reduction
cond_plot = data %>% filter(Cond!="") %>% mutate(Sold.Price.Adjusted = Sold.Price.Adjusted/1000000) %>% 
  ggplot(aes(x=Sold.Price.Adjusted,color=Cond,fill=Cond)) + geom_density() +
  facet_wrap(~Cond) +  labs(y = "Density", x = "Sale Price (In Millions)") +
  ggtitle("Sale Price Across Subdivisions Adjusted for 2% Annual Inflation") + theme_bw()
plot(cond_plot)


################################### Train/Test Splitting of Data ###############################

set.seed(2)
#training index
samp <- createDataPartition(data$Sold.Price.Adjusted, p = .8, 
                                  list = FALSE, 
                                  times = 1)#Regular Training Data
train = as.data.frame(data[samp,])
#Regular Testing Data
test = as.data.frame(data[-samp,])
#Training Data for 'Simplified' Models
data_simp_train = data_simp[samp,]
#Testing Data for 'Unsimplified' Models
data_simp_test = data_simp[-samp,]
#y vecotrs for both
y_train = train$Sold.Price.Adjusted
y_test = test$Sold.Price.Adjusted

################################### Cross Validation ###############################
#cv setup
set.seed(3)
train_control = trainControl(method="repeatedcv", number=10,repeats=3)
###OLS
#cv for linear model on full data
#Standardizing in order to compare to Ridge (RMSE , R^2, ect. all scale invariant)
nums <- unlist(lapply(train, is.numeric))
pp = preProcess(train, method = list(center = colnames(train[nums])[-9], scale = colnames(train[nums])[-9]))
standardized_train = predict(pp,train)
set.seed(3)
cv_ols_model_full = train(Sold.Price.Adjusted ~ Subdivision + Baths.Full + 
                       Baths.Half + Beds.Total  + Age  + Approx.Living.Area + 
                       Cond + Foundation, data=standardized_train, 
                       trControl=train_control, method="lm")


##cv for inference models (nothing to tune)
nums <- unlist(lapply(data_simp_train, is.numeric))
pp = preProcess(train, method = list(center = colnames(data_simp_train[nums]), 
                                     scale = colnames(data_simp_train[nums])))
standardized_data_simp_train = predict(pp,data_simp_train)
###Ridge
##cv for Ridge on full data 
lambdas = expand.grid(lambda=10^seq(-4, 0, by=0.1))
set.seed(3)
cv_ridge_full = train(Sold.Price.Adjusted ~ factor(Subdivision) + Baths.Full + 
                        Baths.Half + Beds.Total  + Age  + Approx.Living.Area + 
                        factor(Cond) + factor(Foundation), trControl=train_control, 
                      data=standardized_train,method="ridge",tuneGrid=lambdas)
plot(cv_ridge_full)

###MARS
##Examining MultiVariate Adaptive Regression Splines on Full Data
marsGrid =  expand.grid(nprune = c(2, 4, 6, 8, 10,12,14,16,18,20,22,24,26,28), degree = c(1, 2, 3))
set.seed(3)
cv_mars = train(Sold.Price.Adjusted ~ factor(Subdivision) + Baths.Full + 
                     Baths.Half + Beds.Total  + Age  + Approx.Living.Area + 
                     factor(Cond) + factor(Foundation), 
                  data =standardized_train, trControl=train_control,method='earth',
                   tuneGrid=marsGrid)
plot(varImp(model_mars_full))



###xgBoost
set.seed(3)
#rounds=250, gamma=0, eta=0.1, colsample=0.5, childweight=1, subsampe=1
#Optimal Parameters already chosen through CV (not going to rerun)
treeparams = expand.grid(nrounds=250, 
                         max_depth = 1, eta =0.1, gamma=0, 
                         colsample_bytree = 0.5,
                         min_child_weight=1, subsample = 1)

cv_xg_simp = train(Sold.Price.Adjusted ~ factor(Subdivision) + Baths.Full + 
                        Baths.Half + Beds.Total  + Age  + Approx.Living.Area + 
                        factor(Cond) + factor(Foundation), 
                      data=standardized_train, trControl=train_control, method="xgbTree",
                      tuneGrid=treeparams)

##Final Cross Validation Results comparing all models on full and simplifed data
results <- resamples(list(OLS_Full=cv_ols_model_full,
                          Ridge_Full=cv_ridge_full,
                          Mars_Full=cv_mars,XG = cv_xg_simp))
bwplot(results) 
summary(results)

#Extracting Coefficients for Ridge Model
train_x_full <- model.matrix(Sold.Price.Adjusted~Age + Beds.Total + Baths.Full + Baths.Half + 
                               Age  + Approx.Living.Area + factor(Cond) + factor(Subdivision)+ 
                               , train)
train_y <- train$Sold.Price.Adjusted
test_x_full =  model.matrix(Sold.Price.Adjusted~Age + Beds.Total + Baths.Full + Baths.Half + 
                              Age  + Approx.Living.Area + factor(Cond) + factor(Subdivision) + 
                              , test)
test_y = test$Sold.Price.Adjusted
model_ridge_full_best <- glmnet(train_x_full, train_y, alpha = 0, lambda = cv_ridge_full$bestTune)
coef(model_ridge_full_best)



################################### Fitting Final Models to Full Training Set ###############################
###OLS Unsimplifieds
#Built On Full Training Set 
model_ols_full_best = lm(Sold.Price.Adjusted ~ Subdivision + Baths.Full + 
                           Baths.Half + Beds.Total  + Age  + Approx.Living.Area + 
                           Cond + Foundation,data=standardized_train)

###MARS
#Built On Full Training Set using best hyperparams chosen from CV
model_mars_best = train(Sold.Price.Adjusted ~ factor(Subdivision) + Baths.Full + 
                          Baths.Half + Beds.Total  + Age  + Approx.Living.Area + 
                          factor(Cond) + factor(Foundation) + factor(Foundation), 
                        data =standardized_train,
                        method='earth',trControl= trainControl(method="none"), tuneGrid = 
                          data.frame(.degree = cv_mars$bestTune[2],.nprune = cv_mars_full$bestTune[1]))
plot(varImp(model_mars_best))
#Can Show Extreme Depreciation within the first year
p_age <- partial(model_mars_best, pred.var = "Age", grid.resolution = 10,plot=TRUE,rug=TRUE) 
p_apa <- partial(model_mars_best, pred.var = "Cond", grid.resolution = 10,plot=TRUE,rug=TRUE)
p_subdivision <- partial(model_mars_best, pred.var = "Subdivision", grid.resolution = 10,plot=TRUE,rug=TRUE) 
p_halfbaths <- partial(model_mars_best, pred.var = "Baths.Half", grid.resolution = 10,plot=TRUE,rug=TRUE) 
p_apa_age <- partial(model_mars_best, pred.var = c("Subdivision", "Foundation"), grid.resolution = 10) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE, screen = list(z = -20, x = -60))

###xgBoost

model_xg_best = train(Sold.Price.Adjusted ~ factor(Subdivision) + Baths.Full + 
                          Baths.Half + Beds.Total  + Age  + Approx.Living.Area + 
                          factor(Cond) + factor(Foundation) + 
                        factor(Foundation), data =standardized_train,
                        method='xgbTree',trControl= trainControl(method="none"), tuneGrid = 
                          data.frame(.nrounds = cv_xg_simp$bestTune[1],
                                     .maxdepth = cv_xg_simp$bestTune[2],
                                     .eta=cv_xg_simp$bestTune[3],
                                     .gamma=cv_xg_simp$bestTune[4],
                                     .colsample_bytree=cv_xg_simp$bestTune[5],
                                     .min_child_weight=cv_xg_simp$bestTune[6],
                                     .subsample=cv_xg_simp$bestTune[7]))


###Preparing test data
#Regular Test Data (OLS, Ridge, MARS)
nums <- unlist(lapply(test, is.numeric))
pp = preProcess(test, method = list(center = colnames(test[nums])[-9], scale = colnames(test[nums])[-9]))
standardized_test = predict(pp,test)

#Creating RMSE Function
rm = function(predictions,true,modelname){
  rms = rmse(predictions,true)
  temp = data.frame(pred = predictions,true=true)
  g = ggplot(temp,aes(x=pred,y=true)) + geom_point(size=.2,col="steelblue") + 
    labs(title = "Scatterplot of Predictions vs. Actual", x="Predictions", y="Actual",
         subtitle = "Test Set RMSE for ",modelname,"is: ",rms)
  print(g)
}
###Calculating Test Prediction
OLS_Full_test_predictions = as.vector(predict(model_ols_full_best,
                                              newdata = standardized_test))
rm(OLS_Full_test_predictions,y_test,OLS)
#Ridge?
MARS_test_predictions = as.vector(predict(model_mars_best,newdata = standardized_test))
rm(MARS_test_predictions,y_test,"MARS")
XG_test_predictions = as.vector(predict(model_xg_best,
                                        newdata=standardized_test))
rm(XG_test_predictions,y_test,"xgBoost")

test_set_pred =  c(OLS_Full_test_predictions,MARS_test_predictions,
                       XG_test_predictions)
Full = data.frame(Pred=test_set_pred,Alg=rep(c("OLS","MARS","xgBoost"),each=329))
#Make Prettier
#Full Test Set MSE
Full %>% group_by(Alg) %>% summarise(sqrt(mean((y_test-Pred)^2))) %>% 
  rename(RMSE=2) %>% ggplot(aes(x=Alg,y=RMSE)) + geom_bar(stat="identity")
#Binning by quartile
bin = Full %>% mutate(y=rep(y_test,times=3)) %>% mutate(Group = ifelse(y<quantile(y,probs=0.25),"Middle",
                                                                 ifelse(y>=quantile(y,probs=0.25) & 
                                                                          y < quantile(y,probs = 0.75),"Upper-Middle",
                                                                        "Upper")))
#Number in Each Group
bin %>% group_by(Group) %>% summarise(n=n())
#Bins
bin %>% group_by(Alg,Group) %>% summarise(RMSE = sqrt(mean((y-Pred)^2))) %>%
  mutate(facet = factor(Group, levels = c("Middle", "Upper-Middle","Upper" ))) %>% 
  ggplot(aes(x=Alg,y=RMSE,color=Alg,fill=Alg))+geom_bar(stat="identity") +facet_wrap(~facet)



  
  
#Finding out Customers who may have overpaid or underpaid for their house (i.e. not having a great selling agent)
  # - overpaid can be defined as paying > 125% of predicted value by model
  # - 'bargain' can be defined as paying < 75% of predicted value by model
bad_deal = data.frame(Actual = test$Sold.Price.Adjusted, Pred_RF = p_rf)
op = which((bad_deal$Actual/bad_deal$Pred_RF) >=1.25)
up = which((bad_deal$Actual/bad_deal$Pred_RF) <=0.75)
bad_for_buyer = test[op,]
good_for_buyer = test[up,]







