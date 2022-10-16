library(tidyverse)
library(xgboost)
library(iml)
library(shapviz) 

##################################
##### Data Loading/TT Split ######
##################################
data = read.csv("freMTPL2freq.csv") %>% 
  mutate_if(is.character,.funs = as.factor) %>% 
  filter(Exposure>0.05) %>% 
  select(-IDpol) %>% 
  mutate(ClaimNb = pmin(ClaimNb,5)) # cap claims at 5

set.seed(2)
train = sample(1:nrow(data),size = round(0.8*nrow(data)),replace = FALSE) %>% sort()

# subsets for XAI
set.seed(2)
viz = list()
viz$train = sample(train,size = 5000,replace = FALSE) %>% sort()
viz$test = sample(setdiff(1:nrow(data),train),size = 5000,replace = FALSE) %>% sort()

plots = list()

##################################
######## Helper Functions ########
##################################
encode = function(x,xgb=T,label=F){
  
  x %>% 
    dplyr::select(-ClaimNb,-Exposure) %>% 
    data.table::data.table() %>% 
    {if(xgb) data.matrix(.) else . } %>% 
    {if(xgb & !label) xgb.DMatrix(.) else if(xgb & label) xgb.DMatrix(.,label = x$ClaimNb) else . } %>% 
    return()
  
}

iml_predict_f = function(model,newdata){
  predict(model,newdata %>% encode(label = T)) %>% return()
}

