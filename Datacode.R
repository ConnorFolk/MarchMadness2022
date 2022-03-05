library(tidyverse)
library(dplyr)



RegSeason=read_csv("MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")




Winners=RegSeason%>%
  select(Season, DayNum, NumOT, starts_with("W"), -WLoc)

colnames(Winners)[4:length(colnames(Winners))]=substring(colnames(Winners)[4:length(colnames(Winners))],2)
colnames(Winners)[5]= "Points"

WinnersOpponent=RegSeason%>%
  select(Season, DayNum, NumOT, WTeamID, starts_with("L"), -LTeamID)

colnames(WinnersOpponent)[4]= "TeamID"
colnames(WinnersOpponent)[5]= "LPoints"
colnames(WinnersOpponent)[5:ncol(WinnersOpponent)]=str_replace(colnames(WinnersOpponent)[5:ncol(WinnersOpponent)], "L", "OPP_")

Full_Winners=inner_join(Winners, WinnersOpponent, by= c("Season", "DayNum", "NumOT", "TeamID"))

Losers=RegSeason%>%
  select(Season, DayNum, NumOT, starts_with("L"))

colnames(Losers)[4:length(colnames(Losers))]=substring(colnames(Losers)[4:length(colnames(Losers))],2)
colnames(Losers)[5]= "Points"

LosersOpponent=RegSeason%>%
  select(Season, DayNum, NumOT, LTeamID, starts_with("W"), -WTeamID, -WLoc)

colnames(LosersOpponent)[4]= "TeamID"
colnames(LosersOpponent)[5]= "WPoints"
colnames(LosersOpponent)[5:ncol(WinnersOpponent)]=str_replace(colnames(WinnersOpponent)[5:ncol(WinnersOpponent)], "W", "OPP_")

Full_Losers=inner_join(Losers, LosersOpponent, by= c("Season", "DayNum", "NumOT", "TeamID"))


RegularSeason= rbind(Full_Winners, Full_Losers)


RegularSeason=RegularSeason%>%
  mutate(#FGP= FGM/FGA,
    #FGP3= FGM3/FGA3,
    #FTP= FTM/FTA,
    TR= OR + DR,
    #ASTtoTURN= Ast/TO,
    #OPP_FGP= OPP_FGM/OPP_FGA,
    #OPP_FGP3= OPP_FGM3/OPP_FGA3,
    #OPP_FTP= OPP_FTM/OPP_FTA,
    OPP_TR= OPP_OR + OPP_DR,
    #OPP_ASTtoTURN= OPP_Ast/OPP_TO,
    OR_DIFF= OR- OPP_OR,
    DR_DIFF= DR- OPP_DR,
    TR_DIFF= TR- OPP_TR,
    POSS= FGA - OR + TO + (.4 * FTA),
    OPP_POSS= OPP_FGA - OPP_OR + OPP_TO + (.4 * OPP_FTA),
    POINTDIFF= Points - OPP_Points, 
    POSS_DIFF= POSS - OPP_POSS)

head(RegularSeason) 

SeasonStats=RegularSeason%>%
  group_by(TeamID, Season)%>%
  summarise(across(c(3:39), mean), FGP= sum(FGM)/sum(FGA), FGP3= sum(FGM3)/sum(FGA3), FTP= sum(FTM)/sum(FTA), OPP_FGP= sum(OPP_FGM)/sum(OPP_FGA), OPP_FGP3= sum(OPP_FGM3)/sum(OPP_FGA3), OPP_FTP= sum(FTM)/sum(FTA), ASStoTURN= sum(Ast)/sum(TO), OPP_ASStoTURN= sum(OPP_Ast)/sum(OPP_TO), PPP= sum(Points)/ sum(POSS), OPP_PPP= sum(OPP_Points)/sum(OPP_POSS), EFFGP= (sum(FGM) + .5*sum(FGM3))/FGA, OPP_EFFGP= (sum(OPP_FGM) + .5*sum(OPP_FGM3))/OPP_FGA)




NCAATourneySched=read_csv("MDataFiles_Stage1/MNCAATourneyCompactResults.csv")
NCAASEEDS= read.csv("MDataFiles_Stage1/MNCAATourneySeeds.csv")
Massey= read.csv("MDataFiles_Stage1/MMasseyOrdinals.csv")

#If team 1 wins then it is a one
NCAASCHED=NCAATourneySched%>%
  rowid_to_column('ID')%>%
  mutate(Team1= ifelse(ID %% 2 != 0, WTeamID, LTeamID),
         Team2= ifelse(ID %% 2 != 0, LTeamID, WTeamID),
         Result= ifelse(ID %% 2 != 0, 1, 0))%>%
  select(Season, Team1,Team2, Result)%>%
  filter(Season >=2003)

NCAASCHED

Final=Massey%>%
  group_by(Season, TeamID)%>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)%>%
  dplyr::slice(n())%>%
  select(POM)%>%
  replace(is.na(.),999)

midpoint=Massey%>%
  group_by(Season, TeamID)%>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)%>%
  dplyr::slice(which(!is.na(POM) & (RankingDayNum <=110 & RankingDayNum>=101)))%>%
  #filter_at(Season==2007, RankingDayNum==100)
  select(POM, RankingDayNum)%>%
  replace(is.na(.),999)

nrow(midpoint)
nrow(Final)

FinalPOM=left_join(Final, midpoint, by= c("Season", "TeamID"))%>%
  select(-RankingDayNum)%>%
  rename('FinalPOM'= 'POM.x', 'LASTMONTHPOM'='POM.y')%>%
  mutate(CHINPOM= FinalPOM- LASTMONTHPOM)%>%
  select(-LASTMONTHPOM)

FullData=left_join(NCAASCHED, SeasonStats, by= c("Season", "Team1" = "TeamID"))
colnames(FullData)[5: ncol(FullData)]=str_c("Team1_", colnames(FullData)[5: ncol(FullData)])
FullData

FullData=left_join(FullData, SeasonStats, by= c("Season", "Team2" = "TeamID"))

colnames(FullData)[54: ncol(FullData)]=str_c("Team2_", colnames(FullData)[54: ncol(FullData)])
FullData

FullData=left_join(FullData, NCAASEEDS, by= c( "Season","Team1"="TeamID"))
FullData=left_join(FullData, NCAASEEDS, by= c( "Season","Team2"="TeamID"))
colnames(FullData)[c(ncol(FullData)-1 , ncol(FullData))]= c("Team1_Seed", "Team2_Seed")

FullData=FullData%>%
  separate(Team1_Seed, 1, into = c("Team1_Region", "Team1_Seed"))%>%
  separate(Team2_Seed, 1, into = c("Team2_Region", "Team2_Seed"))%>%
  separate(Team1_Seed, 2, into = c("Team1_Seed", "Team1_Playin"))%>%
  separate(Team2_Seed, 2, into = c("Team2_Seed", "Team2_Playin"))%>%
  mutate(Team1_Playin= ifelse(Team1_Playin=='', 0, 1))%>%
  mutate(Team2_Playin= ifelse(Team2_Playin=='', 0, 1))

FullData$Team1_Region= as.factor(FullData$Team1_Region)
FullData$Team2_Region= as.factor(FullData$Team2_Region)
FullData$Team1_Seed= as.factor(FullData$Team1_Seed)
FullData$Team2_Seed= as.factor(FullData$Team2_Seed)
FullData$Team1_Playin= as.factor(FullData$Team1_Playin)
FullData$Team2_Playin= as.factor(FullData$Team2_Playin)

FullData=left_join(FullData, FinalPOM, by= c( "Season","Team1"="TeamID"))
colnames(FullData)[c(ncol(FullData)-1, ncol(FullData))]= c("Team1_FINALPOM", "Team1_CHINPOM")
FullData=left_join(FullData, FinalPOM, by= c( "Season","Team2"="TeamID"))
colnames(FullData)[c(ncol(FullData)-1, ncol(FullData))]= c("Team2_FINALPOM", "Team2_CHINPOM")

test=FullData#%>%
#select(-Team1_FTP, -Team2_FTP, -Team1_OPP_FTP, -Team2_OPP_FTP)

test[rowSums(is.na(test)) > 0,]
