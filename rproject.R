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


