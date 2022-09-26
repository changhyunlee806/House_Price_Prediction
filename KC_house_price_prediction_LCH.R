#################################################
#
# King Country House Price Prediction Model
# Changhyun Lee
#
#################################################


##### 1. 환경 세팅 #####

# 1.1 필요한 library 설치 및 불러오기 #####   

if (require("tidyverse") == F) install.packages("tidyverse")  # for data rangling 
if (require("PerformanceAnalytics") == F) install.packages("PerformanceAnalytics")  # for Correlation plot  
if (require("ggpubr") == F) install.packages("ggpubr")
if (require("caret") == F) install.packages("caret")      # for modeling 

if (require("doParallel") == F) install.packages("doParallel")  # for Parallel Processing windows OS  
# if (require("doMC") == F) install.packages("doMC")  # for Parallel Processing Unix and Mac OS   

library(tidyverse)
library(lubridate)
library(SummarizedExperiment)
library(ggpubr)
library(corrplot)

library(pryr)

library(doParallel)
# library(doMC)

# 1.2 환경 변수 세팅하기 #####
set.seed(1234)
# getwd() 
setwd("C:/Users/thomas/Documents/R_prj/ex_06/") #원하는 디렉토리 선택 
# list.files()
options(tibble.width =Inf, scipen=10000) # tibble  데이터 생략 안하도록, 숫자의 지수표현을 안하도록 

# doParallel Parallel Processing 설정
cl <- makePSOCKcluster(2)
registerDoParallel(cl)
# stopCluster(cl) # 종료 

##### 2. 데이터 불러오기 #####
# 2.1 데이터 불러오기 
house <- read_csv("kc_house_data.csv")

# 2.2 데이터 확인
head(house)
str(house)

# 2.3 target 및 변수 분포 확인  #####

summary(house$price)

house %>% 
  ggplot(aes(price)) +
  geom_histogram() + 
  xlim(78000, NA)

house %>% 
  ggplot(aes(price)) +
  geom_boxplot(aes(y=price))


##### 3. NA 값 처리 #####
# 3.1 NA 값 확인 #####
sum(!complete.cases(house))

# NA 값을 갖는 변수 
sum(colSums(is.na(house))>0)

# 각 변수별 NA 값 빈도  
colSums(is.na(house))


# 3.3 이상치 확인 및 제거  #####
summary(house)

# price
house %>% 
  ggplot(aes(price))+
  geom_boxplot(aes(y=price))

quantile(house$price, seq(0,1,0.01))

house %>% 
  summarise(mean = mean(price),sd =sd(price), 
            three_sigma = mean(price)+(3*sd(price)),
            six_sigma = mean(price)+(6*sd(price)))

table(house$bedrooms)

house %>% 
  summarise(mean = mean(sqft_lot), sd = sd(sqft_lot),
            three_sigma = mean(sqft_lot)+ (3* sd(sqft_lot)),
            six_sigma = mean(sqft_lot) + (6* sd(sqft_lot)))


#함수로 3 시그마 처리해보기 
house %>% 
  summarise_each(c(mean,sd), c(bedrooms, sqft_lot,sqft_living))


# 이상치 처리 
house_outlier_trit <-
  house %>%
  mutate(out_price = mean(price)+ (6 * sd(price)),
         out_sqft_lot = mean(sqft_lot)+(6 * sd(sqft_lot))) %>% 
  filter(price < out_price , 
         bedrooms < 12, 
         sqft_lot < out_sqft_lot) %>% 
  select(-out_price, -out_sqft_lot)

# 변수 변환 
data_df  <-
  house_outlier_trit %>% 
  mutate(date = as.Date(date,format = "%m/%d/%Y"),
         year = as.factor(year(date)),
         month = as.factor(month(date)),
         waterfront = ifelse(waterfront == 1 , TRUE, FALSE),
         yr_renovated_yn = ifelse(yr_renovated == 0 ,FALSE,TRUE)) %>% 
  select(-id, -date)

summary(data_df)
str(data_df)


##### 4. EDA #####

# 4.1 탐색 함수 생성 #####
# 연속형 시각화화 함수 
graph_cor <- function(df_name,col_name, target_col_name,sample_size = 1,ylim_max =100)
{eval(str2expression(
  paste0(
    "print(",df_name,"%>% 
    sample_frac(",sample_size,") %>% 
    ggplot(aes(x= ",col_name,", y =",target_col_name,")) +
    geom_point()+
    ylim(NA,",ylim_max,")+
    geom_smooth(method = 'auto')+
    stat_cor(aes(x=",col_name,", y= ",target_col_name,"))+
    ggtitle('",col_name," & ",target_col_name," scatter plot' )+
    theme(plot.title = element_text (hjust =0.5, face= 'bold',size = 15)))")))}



# 명목형 시각화 함수

graph_box <- function(df_name,col_name,target_col_name,sample_size = 1, ylim_max =100){
  eval(str2expression(
    paste0(
      "print(",df_name," %>% 
      sample_frac(0.10) %>% 
      ggplot() +
      geom_boxplot(aes(",col_name,",",target_col_name,", fill =",col_name,"))+
      ylim(NA, ",ylim_max,")+
      geom_hline(yintercept = mean(",df_name,"$",target_col_name,"), color = 'black',linetype='dashed')+
      theme(legend.position = 'bottom',plot.title = element_text (hjust =0.5, face= 'bold',size = 15),
      axis.text.x =element_text(angle = -45,vjust = 1,hjust = 0))+
      ggtitle('",col_name," & ",target_col_name," box plot' ))")))}

# test 
graph_cor("data_df","bedrooms", "price", 1,ylim_max = 3000000)
graph_box("data_df","yr_renovated_yn","price",1,ylim_max = 3000000)

# 4.2 연속형 변수 탐색 그래프 생성 #####
# 연속형 변수 리스트 생성 
col_name_num <- 
  data_df %>% 
  select_if(is.numeric) %>%
  names()


pdf("EDA_house_price_NUM.pdf", width = 12, height = 10)
# 연속형 변수 시각화 
for(i in col_name_num ) graph_cor("data_df",i,"price",1,ylim_max= 3000000)

dev.off() 



# 4.3 명목형 변수 탐색 그래프 생성 #####
col_name_cat <-
  data_df %>% 
  select_if(function(col) is.factor(col) | is.character(col)| is.logical(col)) %>% 
  colnames()  

col_name_cat <- c(col_name_cat,  "as.factor(floors)","as.factor(view)","as.factor(condition)",
                  "as.factor(zipcode)")

# length(col_name_cat)

pdf("EDA_house_price_cat.pdf", width = 12, height = 10)
# 연속형 변수 시각화 
for(i in col_name_cat ) graph_box("data_df",i,"price",1, ylim_max= 3000000)

dev.off() 



# 연속형 변수간 상관관계 

cor_house<- 
  data_df %>% 
  select_if(is.numeric) %>% 
  select(-zipcode) %>% 
  cor()

corrplot(cor_house, method =  'circle', order = 'hclust', type = 'lower', diag = FALSE)

##### 6. 모델링 #####
# 6.1  feature engineering 구간화 변수 생성 #####

data_df_feature <- 
  data_df %>% 
  mutate(floors = as.factor(ifelse(floors >= 3 ,3, floors)),
         view_c = as.factor(ifelse(view == 1 | view ==2 ,1.5,view)),
         condition_c = as.factor(ifelse(condition == 3 | condition == 4, 3.5, condition)),
         lot_diff = sqft_lot15 - sqft_lot,
         lot_diff = case_when(lot_diff == 0 ~ "same",
                              lot_diff > 0 ~ "up",
                              lot_diff < 0 ~ "down",
                              TRUE ~ "ECT"),
         living_diff = sqft_living15 - sqft_living,
         living_diff = case_when(living_diff == 0 ~"same",
                                living_diff > 0 ~"up",
                                living_diff < 0 ~"down",
                                TRUE ~ "ECT")) %>% 
  select(-zipcode,-yr_renovated, -year, -yr_built)


# 6.2  test data set 생성  #####
train_seed <- createDataPartition(data_df_feature$price, p= 0.7,list = F)

train_df <- data_df_feature[train_seed,]
test_df <- data_df_feature[-train_seed,]


# flag별 데이터 및 타겟의 분포 확인
train_df %>% 
  bind_rows(test_df, .id = "flag") %>% 
  ggplot()+
  geom_boxplot(aes(flag, price))

train_df %>% 
  bind_rows(test_df, .id = "flag") %>% 
  group_by(flag) %>% 
  summarise(mean(price),sd(price),n(), mean(sqft_lot), sd(sqft_lot))



# 6.3  후보 모델 생성 및 비교 #####


## lm 모델 
fitControl <- trainControl(method = "none")

system.time(
  lm_fit <- train(price~.,data= train_df, method ="lm",trControl= fitControl) 
)

summary(lm_fit$finalModel)

# pdf("lm_fit.pdf")
par(mfrow=c(2,2))
plot(lm_fit$finalModel)
par(mfrow=c(1,1))

# dev.off()

varImp(lm_fit)


train_df %>% 
  mutate(lm_r = predict(lm_fit ,train_df)) %>% 
  summarise(rmse = sqrt(mean((lm_r - price)^2)))

test_df %>% 
  mutate(lm_r = predict(lm_fit ,test_df)) %>% 
  summarise(rmse = sqrt(mean((lm_r - price)^2)))

## elasticnet 모델
fitControl <- trainControl(method = "cv", number =4, verboseIter = TRUE)

system.time(
  elasticnet_fit <- train(price ~. ,data= train_df, method ="enet",trControl= fitControl) 
)

summary(elasticnet_fit)
elasticnet_fit$bestTune
plot(elasticnet_fit)
varImp(elasticnet_fit$finalModel)


train_df %>% 
  mutate(ela_r = predict(elasticnet_fit ,train_df)) %>% 
  summarise(rmse = sqrt(mean((ela_r - price)^2)))

test_df %>% 
  mutate(ela_r = predict(elasticnet_fit ,test_df)) %>% 
  summarise(rmse = sqrt(mean((ela_r - price)^2)))


## boosted linear model 모델
fitControl <- trainControl(method = "cv", number =4, verboseIter = TRUE)
# fitControl <- trainControl(method = "boot")

system.time(
  blm_fit <- train(price ~. ,data= train_df, method ="BstLm",trControl= fitControl) 
)

summary(blm_fit)
blm_fit$bestTune
plot(blm_fit)
varImp(blm_fit$finalModel)


train_df %>% 
  mutate(blm_r = predict(blm_fit ,train_df)) %>% 
  summarise(rmse = sqrt(mean((blm_r - price)^2)))

test_df %>% 
  mutate(blm_r = predict(blm_fit ,test_df)) %>% 
  summarise(rmse = sqrt(mean((blm_r - price)^2)))


## xgbTree 모델
fitControl <- trainControl(method = "cv", number =4, verboseIter = TRUE)

system.time(
  xgb_fit <- train(price ~. ,data= train_df, method ="xgbTree",trControl= fitControl) 
)


summary(xgb_fit)
xgb_fit$bestTune
plot(xgb_fit)
varImp(xgb_fit)

train_df %>% 
  mutate(xgb_r = predict(xgb_fit ,train_df)) %>% 
  summarise(rmse = sqrt(mean((xgb_r - price)^2)))

test_df %>% 
  mutate(xgb_r = predict(xgb_fit ,test_df)) %>% 
  summarise(rmse = sqrt(mean((xgb_r - price)^2)))


## randomforest 모델
fitControl <- trainControl(method = "cv", number =4, verboseIter = TRUE)

system.time(
  rf_fit <- train(price ~. ,data= sample_frac(train_df,0.5), method ="rf",trControl= fitControl) 
)

summary(rf_fit)
rf_fit$bestTune
plot(rf_fit)


train_df %>% 
  mutate(rf_r = predict(rf_fit ,train_df)) %>% 
  summarise(rmse = sqrt(mean((rf_r - price)^2)))

test_df %>% 
  mutate(rf_r = predict(rf_fit ,test_df)) %>% 
  summarise(rmse = sqrt(mean((rf_r - price)^2)))

varImp(rf_fit)

# 6.4  최종 모델 선택 및 파라미터 튜닝 #####

# xgb turning 
xgb_fit$modelInfo$parameters

grid_xgb <- expand.grid(nrounds = seq(150,200,20),
                        max_depth = 3,
                        eta = 0.4,
                        gamma = seq(0,0.2,0.1),
                        colsample_bytree = 0.8,
                        min_child_weight = c(0.8,1),
                        subsample = c(0.8,1))
system.time(
  xgb_f_fit <- train(price ~. ,data= train_df, method ="xgbTree",tuneGrid = grid_xgb) 
)

summary(xgb_f_fit)
xgb_f_fit$bestTune
plot(xgb_f_fit)


train_df %>% 
  mutate(rf_r = predict(xgb_f_fit ,train_df)) %>% 
  summarise(rmse = sqrt(mean((rf_r - price)^2)))

test_df %>% 
  mutate(rf_r = predict(xgb_f_fit ,test_df)) %>% 
  summarise(rmse = sqrt(mean((rf_r - price)^2)))

varImp(xgb_f_fit)
