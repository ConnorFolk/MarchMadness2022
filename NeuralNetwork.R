library(nnet)
library(NeuralNetTools)

NN=test%>%
  filter(Season<2019 & Season>=2008)%>%
  select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO )


NN=as.data.frame(sapply(NN, scale))%>%
  cbind(test%>%filter(Season<2019 & Season>=2008)%>%select(Result))
NN$Result= as.factor(as.character(NN$Result))

set.seed(25)
bestneural=nnet(Result~., data = NN, size=4, decay=.675)

testyNN= FullData%>%
  filter(Season==2019)%>%
  select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO)%>%
  as.data.frame(sapply( scale))%>%
  cbind(FullData%>%filter(Season==2019)%>%select(Result))

#%>%
#select(-Team1_FTP, -Team2_FTP, -Team1_OPP_FTP, -Team2_OPP_FTP)

logtesty=predict(bestneural, newdata =  testyNN)

classrate2021=(confusionMatrix(factor(testyNN$Result), logtesty)[1,1]+confusionMatrix(factor(testyNN$Result), logtesty)[2,2])/sum(confusionMatrix(factor(testyNN$Result),logtesty))