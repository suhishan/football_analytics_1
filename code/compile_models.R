packages <- c("tidyverse","cmdstanr","bayesplot","loo","posterior")
lapply(packages, library, character.only = TRUE)


#--- Load the data and make it usable ---#

data = read.csv("data/premfull23_24.csv")

data = data %>% select(HomeTeam, HomeGoals, AwayGoals, AwayTeam) %>% 
  rename(
    Home = HomeTeam,
    s1 = HomeGoals,
    s2 = AwayGoals,
    Away = AwayTeam
  )

# Recode team names.
lookup <- c(
     "Brighton and Hove Albion" = "Brighton",
     "Manchester Utd" = "Manchester United",
     "Newcastle Utd" = "Newcastle United",
     "Nott'ham Forest" = "Nottingham Forest",
     "Sheffield Utd" = "Sheffield United",
     "Tottenham Hotspur" = "Tottenham",
     "West Ham United" = "West Ham",
     "Wolverhampton Wanderers" = "Wolves"
)

data = data %>% mutate(
  Home = recode(Home, !!!lookup),
  Away = recode(Away, !!!lookup)
)


#--- Model fit (complete pooling) ---#

teams = unique(data$Home) # Team names
ng = nrow(data) # number of games
nt = length(unique(data$Home)) # number of teams
ht = unlist(sapply(1:ng, function(g) which(teams == data$Home[g]))) # Home team index
at = unlist(sapply(1:ng, function(g) which(teams == data$Away[g]))) # Away team index

np = 10 # Number of games to be predicted i.e. test sample length
ngd = ng - np # Number of games used to train i.e. train sample length.


dat <-  list(
  nt = nt,
  ng = ngd,
  ht = ht[1:ngd],
  at = at[1:ngd],
  s1 = data$s1[1:ngd],
  s2 = data$s2[1:ngd],
  
  # for predictions
  np = np,
  htp = ht[(ngd+1):ng],
  atp = at[(ngd+1):ng]
)


m1 <- cmdstan_model()

