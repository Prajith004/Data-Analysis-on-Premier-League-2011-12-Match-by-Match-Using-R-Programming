library(readxl)
Data <- read_excel("Premier League 2011-12 Match by Match.xls")
library(dplyr)
unique_values <- unique(Data$Team)



print("All the Teams That Were Present In Premier League 2011-2012")
j=1
for(i in unique_values ){
  cat(j,i,"\n")
  j=j+1
}
print("Teams And Their Total Scores ")
result <- aggregate(Goals ~ Team, Data, sum)
print(result)
print("Teams and their scores on average per match")
j=1
for(i in unique_values ){
  team_to_calculate <- i
  team_data <- df[Data$Team == team_to_calculate, ]
  mean_score <- mean(team_data$Goals)
  cat(j,i,mean_score)
  j=j+1
}
result <- aggregate(Goals ~ Team, Data, mean)
print(result)

final_match=max(Data$Date)
final_match
