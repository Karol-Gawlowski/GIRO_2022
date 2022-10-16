##################################
############### IML ##############
##################################
PR = Predictor$new(model = xgb,
                   data = data[viz$train,],
                   predict.function = iml_predict_f) 

ICE = iml::FeatureEffects$new(predictor = PR,
                              feature = "VehAge",
                              method = "ice", 
                              grid.size = 32)

plots$ICE = ICE$plot()+
  geom_line(data = ICE$results$VehAge %>% filter(.id==2035),
            aes(x = .borders,y=.value),color = "blue")+
  ylab("Conditional Prediction")+
  ggtitle("ICE for a subset of 5000 IDs")

PDP = FeatureEffects$new(predictor = PR,
                         feature = "VehAge", 
                         method = "pdp", 
                         grid.size = 32)

plots$PDP = PDP$plot()+
  ggtitle("PDP for a subset of 5000 IDs")

ALE = FeatureEffects$new(predictor = PR,
                         feature = "VehAge",
                         method = "ale", # "pdp", "ice", "ale", "pdp+ice"
                         grid.size = 20)

plots$ALE = ALE$plot()+
  ggtitle("ALE for a subset of 5000 IDs")

##################################
############## SHAP ##############
##################################
visualiser = shapviz::shapviz(object = xgb,
                              X = data[viz$train,] %>% encode(F),
                              X_pred = data[viz$train,] %>% encode(T)) 

# obs level
plots$SHAP_obs_1 = sv_waterfall(visualiser, row_id = 4010) + 
  ggtitle("Observation level SHAP",
          subtitle = paste0("predicted frequency ",round(iml_predict_f(xgb,data[viz$train[4010],]),3)))

plots$SHAP_obs_2 = sv_waterfall(visualiser, row_id = 1000) + 
  ggtitle("Observation level SHAP",
          subtitle = paste0("predicted frequency ",round(iml_predict_f(xgb,data[viz$train[1000],]),3)))

# variable explanation
plots$SHAP_DrivAge = sv_dependence(visualiser,v = "DrivAge")+
  ggtitle("Variable level SHAP - continuous")
plots$SHAP_Region = shapviz::sv_dependence(visualiser,v = "Region")+ 
  scale_x_discrete(limits = data.frame(Region = data$Region[viz$train],
                                       sh = visualiser$S[,9]) %>% 
                     mutate(sh = as.numeric(sh)) %>% 
                     group_by(Region) %>% 
                     summarise(sh = mean(sh)) %>% 
                     arrange(-sh) %>% 
                     pull(Region))+
  ggtitle("Variable level SHAP - factor")+ 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# model level
plots$SHAP_model = sv_importance(visualiser,kind = "beeswarm",show_numbers = TRUE) + 
  ggtitle("Model Level SHAP")

##################################
############## save ##############
##################################

for(i in names(plots)[c(1:3,8)]){

  ggsave(paste0("plots/",i,".jpg"),
         plot = plots[[i]],
         scale = 1,
         width = 6.32,
         height = 4.17,
         units = "in", #"cm", "mm", "px"
         dpi = 300)
  
}

for(i in names(plots)[4:7]){
  
  ggsave(paste0("plots/",i,".jpg"),
         plot = plots[[i]],
         scale = 1,
         width = 5.36,
         height = 3.53,
         units = "in", #"cm", "mm", "px"
         dpi = 300)
  
}
