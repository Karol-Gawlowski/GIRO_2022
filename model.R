##################################
########## Grid Search ###########
##################################
params = expand.grid(eta = seq(0.2,0.4,by=0.1),
                     max_depth=6:9,
                     min_child_weight = c(0.25,0.5,1),
                     colsample_bytree = c(0.5,0.7,1)) %>% 
  data.matrix()

scores = data.frame(i = NA,
                    train_loglik = NA,
                    test_loglik = NA,
                    train_SE = NA,
                    test_SE = NA)

for (i in 1:nrow(params)){

  temp <- xgb.train(
    params = c(params[i,],objective = "count:poisson") %>% as.list(), 
    data = data[train,] %>% encode(xgb = T,label = T),
    nrounds = 200, 
    verbose = 1,
    print_every_n = 2,
    watchlist = list(train = data[train,] %>% encode(label = T),
                     eval = data[-train,] %>% encode(label = T)),
    early_stopping_rounds = 10,
    weight = data[train,2])

  temp$evaluation_log[temp$best_iteration,]$train_poisson_nloglik
  
  scores[i,] = c(i,
                 temp$evaluation_log[temp$best_iteration,]$train_poisson_nloglik,
                 temp$evaluation_log[temp$best_iteration,]$eval_poisson_nloglik,
                 (predict(temp,data[train,] %>% encode(label = T)) - data$ClaimNb[train])^2,
                 (predict(temp,data[-train,] %>% encode(label = T)) - data$ClaimNb[-train])^2)
}

scores %>% 
  select(i,test_loglik,test_SE) %>% 
  pivot_longer(cols = test_loglik:test_SE) %>%  ggplot(aes(x = i,y=value))+geom_line()+geom_point()+facet_wrap(~name,scales="free")

merge(cbind(i=1:48,params),scores,by="i") %>% arrange(test_loglik)

##################################
########## Final Model ###########
##################################
xgb = xgb.train(
  params = c(params[(scores %>% arrange(test_loglik))$i[1],],objective = "count:poisson") %>% as.list(), 
  data = data[train,] %>% encode(xgb = T,label = T),
  nrounds = 700, 
  verbose = 1,
  print_every_n = 2,
  watchlist = list(train = data[train,] %>% encode(label = T),
                   eval = data[-train,] %>% encode(label = T)),
  
  early_stopping_rounds = 10,
  weight = data[train,2])

xgb.save(model = xgb,fname = "models/xgb_final.rd")

##################################
######## Quick Analysis ##########
##################################
results = cbind(data[-train,],
                predicted = predict(xgb,data[-train,] %>% encode())) %>% 
  mutate(se = (predicted-ClaimNb)^2)

results %>% arrange(-se) %>% head(50)
mean(results$se)

results %>% 
  group_by(ClaimNb) %>% 
  summarise(mean = mean(predicted),
            q1 = quantile(predicted,probs = 0.25),
            q2 = quantile(predicted,probs = 0.5),
            q3 = quantile(predicted,probs = 0.75))

results %>% ggplot(aes(x = se))+
  geom_density()+
  xlim(0,quantile(results$se,probs = 0.9))
