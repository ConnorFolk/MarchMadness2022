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

testyNN2019= FullData%>%
  filter(Season==2019)%>%
  select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO)%>%
  as.data.frame(sapply( scale))%>%
  cbind(FullData%>%filter(Season==2019)%>%select(Result))


testyNN2021= FullData%>%
  filter(Season==2021)%>%
  select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO)%>%
  as.data.frame(sapply( scale))%>%
  cbind(FullData%>%filter(Season==2021)%>%select(Result))

#%>%
#select(-Team1_FTP, -Team2_FTP, -Team1_OPP_FTP, -Team2_OPP_FTP)

logtesty2019=predict(bestneural, newdata =  testyNN2019)
logtesty2021=predict(bestneural, newdata =  testyNN2021)


