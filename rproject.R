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

result <- aggregate(Goals ~ Team, Data, sum)
print("Teams And Their Total Scores ")
print(result)
###############################################################################
#3.Get Teams with their scores on average per match


result2 <- aggregate(Goals ~ Team, Data, mean)
print("Teams and their scores on average per match")
print(result2)
########################################################################################################
#4.Bar graph teams and their total score in the season
library(dplyr)
library(readxl)
library(ggplot2)
Data <- read_excel("Premier League 2011-12 Match by Match.xls")
aggregated_data <- Data %>% 
  group_by(Team) %>% 
  summarise(Goals = sum(Goals))
p <- ggplot(aggregated_data, aes(x = Team, y = Goals)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = Team, y = Goals/2), size = 4, angle = 90,vjust=1, color = "white") +
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

#6.Team Which Won Fairplay Award

df <- data.frame(
  Team = Data$Team,
  Yellow_Cards = Data$`Yellow Cards`,
  Red_Cards = Data$`Red Cards`

)
df <- df %>%
  mutate(Total_Cards = Yellow_Cards + Red_Cards)
fair_play_winner <- df %>%
  group_by(Team) %>%
  summarise(Total_Cards = sum(Total_Cards)) %>%
  arrange(Total_Cards) %>%
  slice(1)
print("The Team Which Won Fairplay Award")
print(fair_play_winner)

#7.Clean Sheet Prediction for goal keepers
# Load necessary libraries
library(readxl)
library(dplyr)
library(caret)  # For modeling

# Read data from Excel file
file_path <- "Premier League 2011-12 Match by Match.xls"
data <- read_excel(file_path)  # Replace "Sheet1" with the correct sheet name

# Select relevant columns for goalkeeper analysis and filter rows
goalkeeper_data <- data %>%
  select(
    'Player Surname', 'Player Forename', 'Saves Made', 'Goals Conceded',
    Catches, Punches, 'Clean Sheets'
  ) %>%
  filter(`Saves Made` != 0)  # Filtering out rows where Saves Made is not equal to zero

# Split the data into features and target variable
features <- goalkeeper_data %>%
  select(-`Clean Sheets`)  # Excluding the target variable
target <- goalkeeper_data$`Clean Sheets`


# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(target, p = 0.8, list = FALSE)
train_data <- features[train_index, ]
test_data <- features[-train_index, ]
train_target <- target[train_index]
test_target <- target[-train_index]

# Train a linear regression model
model <- train(
  x = train_data,
  y = train_target,
  method = "lm"  # Using linear regression
)

# Predict on the test set
predictions <- predict(model, test_data)
goalkeeper_data$`Player Surname` <- as.character(goalkeeper_data$`Player Surname`)

# New goalkeeper data for prediction
new_goalkeeper_data <- data.frame(
  'Saves Made' = 6,
  'Goals Conceded' = 0,
  Catches = 1,
  Punches = 2,
  'Player Surname' = 'Hart',
  `Player Forename` = "Joe" 
)
# Rename the column 'Player.Surname' to 'Player Surname'
# Rename the columns to match the model's training columns
names(new_goalkeeper_data)[names(new_goalkeeper_data) == "Player.Surname"] <- "Player Surname"
names(new_goalkeeper_data)[names(new_goalkeeper_data) == "Player.Forename"] <- "Player Forename"
names(new_goalkeeper_data)[names(new_goalkeeper_data) == "Saves.Made"] <- "Saves Made"
names(new_goalkeeper_data)[names(new_goalkeeper_data) == "Goals.Conceded"] <- "Goals Conceded"

# Predict using the trained model
predicted_clean_sheets <- predict(model, newdata = new_goalkeeper_data)
print(predicted_clean_sheets)

str(new_goalkeeper_data)

