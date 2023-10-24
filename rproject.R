library(readxl)
Data <- read_excel("Premier League 2011-12 Match by Match.xls")
print(head(Data))
library(dplyr)
unique_values <- unique(Data$Team)


#1.Get all the teams that were present in Premier League 2011-2012
print("All the Teams That Were Present In Premier League 2011-2012")
j=1
for(i in unique_values ){
  cat(j,i,"\n")
  j=j+1
}
###################################################################################
#2.Get teams with their total score in that season
print("Teams And Their Total Scores ")
result <- aggregate(Goals ~ Team, Data, sum)
print(result)
###############################################################################
#3.Get Teams with their scores on average per match
print("Teams and their scores on average per match")

result2 <- aggregate(Goals ~ Team, Data, mean)
print(result2)
########################################################################################################
#4.Bar graph teams and their total score in the season
library(dplyr)
library(readxl)
Data <- read_excel("Premier League 2011-12 Match by Match.xls")
aggregated_data <- Data %>% 
  group_by(Team) %>% 
  summarise(Goals = sum(Goals))
p <- ggplot(aggregated_data, aes(x = Team, y = Goals)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = Team, y = Goals/2), size = 4, angle = 90,vjust=0.3, color = "white") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(x = "Team", y = "Goals")
print(p)
####################################################
#5.Enter the date to check which match was held

check_d=function(user_date){
filtered_data <- subset(Data, Date == user_date)
if (nrow(filtered_data) == 0) {
  cat("No data available for the entered date.\n")
} else {
  unique_matchups <- unique(paste(pmin(filtered_data$Team, filtered_data$Opposition), pmax(filtered_data$Team, filtered_data$Opposition), sep = "-"))
  unique_matchups_df <- data.frame(Team = character(0), Opposition = character(0), Goals = character(0), stringsAsFactors = FALSE)
  for (matchup in unique_matchups) {
    teams <- unlist(strsplit(matchup, "-"))
    team1_goals <- sum(filtered_data$Goals[filtered_data$Team == teams[1]])
    team2_goals <- sum(filtered_data$Goals[filtered_data$Team == teams[2]])
    goals_string <- paste(team1_goals, team2_goals, sep = "-")
    unique_matchups_df <- rbind(unique_matchups_df, data.frame(Team = teams[1], Opposition = teams[2], Goals = goals_string))
  }
  print(unique_matchups_df)
}
}

user_date <- as.Date(readline("Enter a date (yyyy-mm-dd): "))
check_d(user_date)





