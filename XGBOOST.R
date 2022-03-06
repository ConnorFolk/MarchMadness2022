library(xgboost)
library(caret)
test= FullData%>%
  filter(Season>=2008 & Season<2019)
train_x <- model.matrix(Result ~ ., data = test%>% filter(Season<2019 & Season>=2008)%>%select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF, Result,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO))[,-1]

train_y <- as.numeric(as.character(test$Result))

# Build XGBoost model
#set.seed(25)
#xgb.ames <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 100)

#xgbcv.mm <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, nfold = 30, objective = "binary:logistic", eval_metric= 'error')

#xgbcv.mm$evaluation_log%>%arrange((test_error_mean))

set.seed(25)
xgbbest= xgboost(data = train_x, label = train_y, subsample = .45, nrounds = 7, eta = 0.215, max_depth = 12, gamma=0, min_child_weight=1, objective = "binary:logistic", eval_metric='error')


ML_trainphatsxgb=predict(xgbbest, train_x, type="response")
#typeof(ML_trainphatsxgb)

testyXGB2019= FullData%>%
  filter(Season==2019)#%>%
#select(-Team1_FTP, -Team2_FTP, -Team1_OPP_FTP, -Team2_OPP_FTP)

traintestyXGB2019= model.matrix(Result ~ ., data = testyXGB2019%>% select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF, Result,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO))[,-1]

xgbtesty2019=predict(xgbbest, newdata =  traintestyXGB2019, type="response")

testyXGB2021= FullData%>%
  filter(Season==2021)#%>%
#select(-Team1_FTP, -Team2_FTP, -Team1_OPP_FTP, -Team2_OPP_FTP)

traintestyXGB2021= model.matrix(Result ~ ., data = testyXGB2021%>% select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF, Result,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO))[,-1]

xgbtesty2021=predict(xgbbest, newdata =  traintestyXGB2021, type="response")