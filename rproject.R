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

result2 <- aggregate(Goals ~ Team, Data, mean)
print(result2)

#final_match=max(Data$Date)
#final_match
library(plotrix)
x=result$Goals
lbl=result$Team
png(file="football.png")
pie3D(x,labels=lbl,explode=0.1,main="Pie chart of football teams and their total score",col=rainbow(length(x)))


dev.off()




png(file="barfootball.png")
barplot(x,names.arg = lbl,xlab = "Teams",ylab = "Goals",col = "blue",border = "red",main ="Bar plot of football teams and their total score")
dev.off()



library(dplyr)
library(readxl)
Data <- read_excel("Premier League 2011-12 Match by Match.xls")
aggregated_data <- Data %>% group_by(Team) %>% summarise(Goals = sum(Goals))
p <- ggplot(aggregated_data, aes(x = Team, y = Goals)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = lbl, y = x/2), size = 4, angle = 90,vjust=0.3, color = "white") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(x = "Team", y = "Goals")
print(p)

