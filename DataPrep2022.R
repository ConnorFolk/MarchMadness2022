possibilites2022= read.csv("MDataFiles_Stage2/MSampleSubmissionStage2.csv")

FinalPOMuse= FinalPOM%>%
  rename('FINALPOM'='FinalPOM')
all_ses=left_join(SeasonStats, FinalPOMuse, by=c("TeamID", "Season"))
head(all_ses)


head(possibilites2022)

possibilites2022one=possibilites2022%>%
  separate(ID, '_', into= c("Season", "Team1", "Team2"))%>%
  mutate(Season=as.numeric(Season),
         Team1=as.numeric(Team1),
         Team2=as.numeric(Team2))%>%
  select(-Pred)

possibilites2022two=possibilites2022%>%
  separate(ID, '_', into= c("Season", "Team2", "Team1"))%>%
  mutate(Season=as.numeric(Season),
         Team1=as.numeric(Team1),
         Team2=as.numeric(Team2))%>%
  select(-Pred)


data2022one=left_join(possibilites2022one, all_ses, by=c("Team1"="TeamID", "Season"))
colnames(data2022one)[4: ncol(data2022one)]=str_c("Team1_", colnames(data2022one)[4: ncol(data2022one)])
data2022one=left_join(data2022one, all_ses, by=c("Team2"="TeamID", "Season"))
colnames(data2022one)[55: ncol(data2022one)]=str_c("Team2_", colnames(data2022one)[55: ncol(data2022one)])
head(data2022one)

data2022=data2022one

data2022two=left_join(possibilites2022two, all_ses, by=c("Team1"="TeamID", "Season"))
colnames(data2022two)[4: ncol(data2022two)]=str_c("Team1_", colnames(data2022two)[4: ncol(data2022two)])
data2022two=left_join(data2022two, all_ses, by=c("Team2"="TeamID", "Season"))
colnames(data2022two)[55: ncol(data2022two)]=str_c("Team2_", colnames(data2022two)[55: ncol(data2022two)])
head(data2022two)


data2022one$Result= rep(1, nrow(data2022one))
data2022two$Result= rep(1, nrow(data2022two))
data2022xgbdataone= model.matrix(Result ~ ., data = data2022one%>% select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF, Result,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO))[,-1]
data2022xgbdatatwo= model.matrix(Result ~ ., data = data2022two%>% select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF, Result,Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP, Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO))[,-1]

data2022$xgbpredone=predict(xgbbest, newdata =  data2022xgbdataone, type="response")
data2022$xgbpredtwo=1-predict(xgbbest, newdata =  data2022xgbdatatwo, type="response")

data2022$rfpredone=as.double(predict(rf.ames2, newdata = data2022one, type= "prob")[,2])
data2022$rfpredtwo=1-as.double(predict(rf.ames2, newdata = data2022two, type= "prob")[,2])

dataNN2022one= data2022one%>%
  select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF,
         Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP,
         Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO)%>%
  sapply(scale)%>%
  as.data.frame()

dataNN2022two= data2022two%>%
  select(Team1_PPP, Team1_OPP_PPP,Team2_PPP, Team2_OPP_PPP, Team1_POINTDIFF, Team2_POINTDIFF,
         Team1_FINALPOM, Team2_FINALPOM, Team1_OPP_FGP3, Team2_OPP_FGP3, Team1_FTP, Team2_FTP,
         Team1_OR_DIFF, Team2_OR_DIFF, Team1_OPP_TO, Team2_OPP_TO)%>%
  sapply(scale)%>%
  as.data.frame()

data2022$NNpredone=predict(bestneural, newdata =  dataNN2022one)
data2022$NNpredtwo=1-predict(bestneural, newdata =  dataNN2022two)

Kagglescores=data2022%>%
  mutate(Pred= (xgbpredone + xgbpredtwo + rfpredone + rfpredtwo + NNpredone + NNpredtwo)/6,
         ID=paste0(Season,"_", Team1, "_", Team2 ))%>%
  select(ID, Pred)

bracketmaker=data2022%>%
  mutate(Pred= (xgbpredone + xgbpredtwo + rfpredone + rfpredtwo + NNpredone + NNpredtwo)/6)%>%
  select(Team1, Team2, Pred)

data2022%>%
  select(Team1, Team2, xgbpredone, xgbpredtwo, rfpredone, rfpredtwo,NNpredone, NNpredtwo)

teams= read.csv("MDataFiles_Stage2/MTeams.csv")


bracketmaker=bracketmaker%>%
  left_join(teams, by=c('Team1'='TeamID'))%>%
  left_join(teams, by=c('Team2'='TeamID'))%>%
  select(TeamName.x, TeamName.y, Pred)%>%
  rename('Team1'='TeamName.x', 'Team2'='TeamName.y')%>%
  mutate(Winner= ifelse(Pred>.5, Team1, Team2))

teambrack=function(team1, team2){
  
  return(bracketmaker%>%
    filter((Team1==team1 & Team2==team2) |(Team1==team2 & Team2==team1)))
}

#teambrack("Arizona", "Purdue")

#write.csv(Kagglescores, "Kagglescores.csv", row.names=F)

bracketmaker%>%
  mutate(Pred2=1-Pred)%>%
  select(Team2, Pred2)%>%
  rename('Team1'='Team2', 'Pred'='Pred2')%>%
  rbind(bracketmaker%>%select(Team1, Pred))%>%
  group_by(Team1)%>%
  summarise('AVGPrediction'=mean(Pred))%>%
  arrange(desc(AVGPrediction))
