SeedingOrdinal=Massey%>%
  group_by(Season, TeamID)%>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)%>%
  dplyr::slice(n())%>%
  select(Season, TeamID, SEL)

Chalk2019= FullData%>%
  select(Team1, Team2, Season, Result)%>%
  filter(Season==2019)%>%
  left_join( SeedingOrdinal, by=c('Team1'= 'TeamID', 'Season'))%>%
  rename('SELTeam1'='SEL')%>%
  left_join( SeedingOrdinal, by=c('Team2'= 'TeamID', 'Season'))%>%
  rename('SELTeam2'='SEL')%>%
  mutate(ChalkPred= ifelse(SELTeam1<SELTeam2, 1, 0),
         ChalkCorrect= ifelse(ChalkPred==Result, 1,0))

chalkaccuracy2019=sum(Chalk2019$ChalkCorrect)/nrow(Chalk2019)

Chalk2021= FullData%>%
  select(Team1, Team2, Season, Result)%>%
  filter(Season==2021)%>%
  left_join( SeedingOrdinal, by=c('Team1'= 'TeamID', 'Season'))%>%
  rename('SELTeam1'='SEL')%>%
  left_join( SeedingOrdinal, by=c('Team2'= 'TeamID', 'Season'))%>%
  rename('SELTeam2'='SEL')%>%
  mutate(ChalkPred= ifelse(SELTeam1<SELTeam2, 1, 0),
         ChalkCorrect= ifelse(ChalkPred==Result, 1,0))

chalkaccuracy2021=sum(Chalk2021$ChalkCorrect)/nrow(Chalk2021)



