library(randomForest)

test= FullData%>%
  filter(Season>=2008 )

set.seed(25)
rf.ames2 <- randomForest(as.factor(Result) ~ ., data =  test%>% select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF, Result,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO),ntree = 200, mtry=5, importance = TRUE )



rfphat=as.double(predict(rf.ames2, type= "prob")[,2])




#testyrfdata2019= FullData%>%
#  filter(Season==2019)
#testyrf2019= as.double(predict(rf.ames2, newdata = testyrfdata2019, type= "prob")[,2])



#testyrfdata2021= FullData%>%
#  filter(Season==2021)
#testyrf2021= as.double(predict(rf.ames2, newdata = testyrfdata2021, type= "prob")[,2])
#plotROC(factor(testy$Result), testyrf)


#h=as.data.frame(cbind(testy$Result,testyrf, testy$Team1, testy$Team2))%>%
#  mutate(Predicted= ifelse(testyrf>.5, 1, 0),
#         Correct= ifelse(Predicted==V1, 1, 0))

#sum(h$Correct)/nrow(h)
