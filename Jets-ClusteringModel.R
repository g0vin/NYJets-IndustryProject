# Libraries needed to read in and manipulate the data 
library(readxl)
library(dplyr)

# Read in the Game_Clustering_Model file we created
game_clustering_jets <- read_excel("~/Downloads/Game_Clustering_Jets (2).xlsx")

# Dummy variables for `Time of Day`
game_clustering_jets$TODNoon <- ifelse(game_clustering_jets$'Time of the day' == "Noon", 1, 0)
game_clustering_jets$TODAfternoon <- ifelse(game_clustering_jets$'Time of the day' == "Afternoon", 1, 0)
game_clustering_jets$TODEvening <- ifelse(game_clustering_jets$'Time of the day' == "Evening", 1, 0)

# Dummy variables for Event_Day
game_clustering_jets$DayThursday <- ifelse(game_clustering_jets$'Event_Day' == "Thursday", 1, 0)
game_clustering_jets$DayFriday <- ifelse(game_clustering_jets$'Event_Day' == "Friday", 1, 0)
game_clustering_jets$DaySaturday <- ifelse(game_clustering_jets$'Event_Day' == "Saturday", 1, 0)
game_clustering_jets$DaySunday <- ifelse(game_clustering_jets$'Event_Day' == "Sunday", 1, 0)
game_clustering_jets$DayMonday <- ifelse(game_clustering_jets$'Event_Day' == "Monday", 1, 0)

# Dummy variables for Month
game_clustering_jets$MonthAugust <- ifelse(game_clustering_jets$'Month' == "August", 1, 0)
game_clustering_jets$MonthSeptember <- ifelse(game_clustering_jets$'Month' == "September", 1, 0)
game_clustering_jets$MonthOctober <- ifelse(game_clustering_jets$'Month' == "October", 1, 0)
game_clustering_jets$MonthNovember <- ifelse(game_clustering_jets$'Month' == "November", 1, 0)
game_clustering_jets$MonthDecember <- ifelse(game_clustering_jets$'Month' == "December", 1, 0)


# Rename the ID variable in game_clustering_jets to event_id
game_clustering_jets <- game_clustering_jets %>% 
  rename(
    event_id = ID
  )

###########################################################################################################################

# Read the Ticket Exchange data in 
Ticket_Exchange_raw <- read.csv("~/Downloads/Ticket Exchange.csv")

# Create a new variable called Time_Before that find the difference between the event day and the day the ticket was purchases
Ticket_Exchange_raw$Time_Before <- difftime(Ticket_Exchange_raw$event_date, Ticket_Exchange_raw$add_datetime, units= "days")

# Convert negative values in Time_before to 0.
Ticket_Exchange_raw$Time_Before <- ifelse(Ticket_Exchange_raw$Time_Before < 0, 0, Ticket_Exchange_raw$Time_Before)


# Create buckets for different purchase times
Ticket_Exchange_raw$purchase_date <- ifelse(Ticket_Exchange_raw$Time_Before == 0, "Day_of", ifelse(Ticket_Exchange_raw$Time_Before<=7 & Ticket_Exchange_raw$Time_Before>0, "Week_of", ifelse(Ticket_Exchange_raw$Time_Before<=28 & Ticket_Exchange_raw$Time_Before>7, "4_Weeks", "months_before")))
Ticket_Exchange_raw$purchase_day_of <- ifelse(Ticket_Exchange_raw$purchase_date == "Day_of", 1, 0)
Ticket_Exchange_raw$purchase_week_of <- ifelse(Ticket_Exchange_raw$purchase_date == "Week_of", 1, 0)
Ticket_Exchange_raw$purchase_month_of <- ifelse(Ticket_Exchange_raw$purchase_date == "4_Weeks", 1, 0)
Ticket_Exchange_raw$purchase_over_month_out <- ifelse(Ticket_Exchange_raw$purchase_date == "months_before", 1, 0)


# Remove the missing purchase_price data from the Ticket Exchange file
Ticket_Exchange <- Ticket_Exchange_raw[!Ticket_Exchange_raw$te_purchase_price == "NULL", ] 
Ticket_Exchange <- na.omit(Ticket_Exchange)


# Change the variable type of posting price from character to numeric 
Ticket_Exchange$te_posting_price <- as.numeric(Ticket_Exchange$te_purchase_price)

# Rename variable names in Ticket_Exchange
library(tidyverse)
Ticket_Exchange <- Ticket_Exchange %>% 
  rename(
    Section = section_name,
    Row = row_name,
    Seat = seat_num
  )

###########################################################################################################################

# Read the Price Level Section Mapping data in
Price_Level_Section_Mapping_raw <- read.csv("~/Downloads/Price Level Section Mapping.csv")

# Uncomment the below line to install the 'fastDummies' package
#install.packages('fastDummies')
library('fastDummies')

# Merge the Ticket_Exchange and the Price_Level_Section_Mapping data sets
tickets <- merge(Price_Level_Section_Mapping_raw, Ticket_Exchange, by = c("Section", "Row", "Seat"))

# Merge the already merged ticket data from above with the Excel spreadsheet data we created 
tickets_all <- merge(tickets, game_clustering_jets,  by="event_id")

# Change the type of Rows from character to integer
tickets_all <- tickets_all[!tickets_all$te_purchase_price == "NULL", ]
tickets_all$Row <- as.numeric(tickets_all$Row)
tickets_all$te_purchase_price <- as.numeric(tickets_all$te_purchase_price)
tickets_all$te_purchase_price <- na.omit(tickets_all$te_purchase_price)

# Remove leading whitespace from Opponent names
tickets_all$Opponent <- str_trim(tickets_all$Opponent, side ="right")

# Create training and test data
training_data <- tickets_all[tickets_all$season_year != 2019,]
test_data <- tickets_all[tickets_all$season_year == 2019,]

#########################################################################################################################

# Create the model used to classify the game type tiers
model3 <- lm(te_purchase_price~season_year+PC1+Opponent+
               Percentage_W_previous_season+TODNoon+TODAfternoon+
               DayThursday+DaySaturday+DaySunday+MonthDecember+
               MonthSeptember+MonthOctober+MonthNovember+GameType, 
             data=training_data)
#summary(model3)

test_data$season_year <- mean(tickets_all$season_year)

# Select the relevant variables from the test data based on the model
new_data <- test_data %>% 
  dplyr::select(season_year,Time_Before,Opponent,PC1,GameType,
                Percentage_W_previous_season,TODNoon,TODAfternoon,DayThursday,DayFriday,
                DaySaturday,DaySunday,MonthDecember,MonthSeptember,MonthOctober,MonthNovember) %>%
  filter(Opponent == "Oakland Raiders")

# Predict the purchase price on the test data based on the model we created
new_data$predicted_purchase <- predict(model3, newdata = new_data)
new_data %>% dplyr::select(PC1, predicted_purchase) ############
test_data %>% dplyr::select(Opponent, PC1, te_purchase_price)%>%  ############
  filter(Opponent == "Oakland Raiders") ############

# Omit any NA values on the predicted purchase price
new_data$predicted_purchase <- na.omit(new_data$predicted_purchase) ############


new_data %>% dplyr::select(PC1, predicted_purchase) %>% ############
  filter(PC1=="M") %>% summarise(mean(predicted_purchase)) ############

test_data$te_purchase_price <- na.omit(test_data$te_purchase_price) ############
test_data$te_purchase_price <- as.numeric(test_data$te_purchase_price) ############
test_data %>% dplyr::select(Opponent, PC1, te_purchase_price)%>% ############
  filter(Opponent == "Oakland Raiders") %>% ############
  filter(PC1=="M") %>% summarise(mean(te_purchase_price)) ############

# GAVIN WANTS TO WORK MORE

new_data$predicted_purchase <- na.omit(new_data$predicted_purchase) ############
new_data %>% dplyr::select(PC1, predicted_purchase) %>% ############
  summarise(mean(predicted_purchase)) ############

test_data$te_purchase_price <- na.omit(test_data$te_purchase_price) ############
test_data$te_purchase_price <- as.numeric(test_data$te_purchase_price) ############
test_data %>% dplyr::select(Opponent, PC1, te_purchase_price)%>% ############
  filter(Opponent == "Oakland Raiders") %>% ############
  summarise(mean(te_purchase_price)) ############


