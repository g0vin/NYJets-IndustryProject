
# References the Jets-ScheduleFunction.R file where the Schedule function is located
source("Jets-ScheduleFunction.R", local = T)

# References the Jets-ClusteringModel.R file where the Clustering Model is located
source("Jets-ClusteringModel.R", local = T)

# Create a function called simulation that runs the generate_schedule
simulation <- function(){
  
  # Initialize a variable x
  x<-1
  
  # While loop
  while(x<2){
    run_schedule <- generate_schedule()
    run_schedule<-rbind(run_schedule, run_schedule[rep(1:8,23),])
    run_schedule<- run_schedule %>% arrange(week)
    run_schedule$PC1 <- rep(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", 
                              "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X"),8)
    
    # Assign the Facebook follower values for the 2021 opponents
    # run_schedule$Facebook_Followers <- 
    #   ifelse(run_schedule$opponent=="Dolphins",2200000,
    #          ifelse(run_schedule$opponent=="Jaguars",690000,
    #                 ifelse(run_schedule$opponent=="Bills", 940000,
    #                        ifelse(run_schedule$opponent=="Patriots", 6870000,
    #                               ifelse(run_schedule$opponent=="Buccaneers", 1000000,
    #                                      ifelse(run_schedule$opponent=="Titans", 960000,
    #                                             ifelse(run_schedule$opponent=="Bengals", 1100000, 3980000)))))))
    
    # Create the schedule for the 2021 season
    run_schedule$season_year <- 2021
    
    # Assign the winning percentage for the 2021 opponents
    run_schedule$Percentage_W_previous_season <-
      ifelse(run_schedule$opponent=="Dolphins",0.625,
             ifelse(run_schedule$opponent=="Jaguars",0.063,
                    ifelse(run_schedule$opponent=="Bills", 0.813,
                           ifelse(run_schedule$opponent=="Patriots", 0.438,
                                  ifelse(run_schedule$opponent=="Buccaneers", 0.688,
                                         ifelse(run_schedule$opponent=="Titans", 0.688,
                                                ifelse(run_schedule$opponent=="Bengals", 0.281, 0.750)))))))
    #run_schedule$ELO <-
    #  ifelse(run_schedule$opponent=="Dolphins",1546,
    #        ifelse(run_schedule$opponent=="Jaguars",1256,
    #              ifelse(run_schedule$opponent=="Bills", 1699,
    #                    ifelse(run_schedule$opponent=="Patriots", 1499,
    #                          ifelse(run_schedule$opponent=="Buccaneers", 1732,
    #                                ifelse(run_schedule$opponent=="Titans", 1578,
    #                                      ifelse(run_schedule$opponent=="Bengals", 1366, 1670)))))))
    
    # Create dummy variables for time of day 
    run_schedule$TODNoon <-
      ifelse(run_schedule$time == "Sunday_100", 1,0)
    
    run_schedule$TODAfternoon <-
      ifelse(run_schedule$time == "Saturday_430", 1, 
             ifelse(run_schedule$time == "Sunday_405", 1,
                    ifelse(run_schedule$time == "Sunday_425", 1, 0)))
    
    # Create dummy variables for day of the week
    run_schedule$DayThursday <- 
      ifelse(run_schedule$time == "Thursday_820", 1,0) 
    
    run_schedule$DaySaturday <- 
      ifelse(run_schedule$time == "Saturday_430", 1, 
             ifelse(run_schedule$time == "Saturday_820", 1,0))
    
    run_schedule$DaySunday <-
      ifelse(run_schedule$time == "Sunday_430", 1, 
             ifelse(run_schedule$time == "Sunday_405", 1,
                    ifelse(run_schedule$time == "Sunday_425", 1, 
                           ifelse(run_schedule$time=="Sunday_100",1,
                                  ifelse(run_schedule$time=="Sunday_820",1,0)))))
    
    # Create dummy variables for Month
    run_schedule$MonthDecember <- 
      ifelse(run_schedule$week < 16.5 & run_schedule$week > 12.5 , 1, 0)
    run_schedule$MonthSeptember <-
      ifelse(run_schedule$week < 3.5 & run_schedule$week > 0.5, 1, 0)
    run_schedule$MonthOctober <-
      ifelse(run_schedule$week < 8.5 & run_schedule$week > 3.5, 1, 0)
    run_schedule$MonthNovember <-
      ifelse(run_schedule$week < 12.5 & run_schedule$week > 8.5, 1, 0)
    
    # Transform team names into the full proper names with the city 
    run_schedule$Opponent <-
      ifelse(run_schedule$opponent=="Dolphins", "Miami Dolphins",
             ifelse(run_schedule$opponent=="Jaguars", "Jacksonville Jaguars",
                    ifelse(run_schedule$opponent=="Bills", "Buffalo Bills",
                           ifelse(run_schedule$opponent=="Patriots", "New England Patriots",
                                  ifelse(run_schedule$opponent=="Buccaneers", "Tampa Bay Buccaneers",
                                         ifelse(run_schedule$opponent=="Titans", "Tennessee Titans",
                                                ifelse(run_schedule$opponent=="Bengals", "Cincinnati Bengals", "New Orleans Saints")))))))
    
    # Mean season year
    run_schedule$season_year <- mean(tickets_all$season_year)
    
    # Initialize the game type to regular season for the 2021 teams
    run_schedule$GameType <- "Regular Season"
    
    # Select the appropriate variables and create the data
    new_data <- run_schedule %>%
      dplyr::select(season_year,Opponent,Percentage_W_previous_season,PC1,GameType,
                    TODNoon,TODAfternoon,DayThursday,DaySaturday,DaySunday,MonthDecember,
                    MonthSeptember,MonthOctober,MonthNovember)
    
    # Select the appropriate variables and create the model
    model3 <- lm(te_purchase_price~season_year+Opponent+PC1+Percentage_W_previous_season+GameType+
                   TODNoon+TODAfternoon+DayThursday+DaySaturday+DaySunday+MonthDecember+
                   MonthSeptember+MonthOctober+MonthNovember, data=tickets_all)
    
    # Predict the purchase prices
    run_schedule$predicted_purchase <- predict(model3, newdata = new_data)
    x <- x+1
    run_schedule <- run_schedule %>% 
      dplyr::select(Opponent, predicted_purchase, PC1)
    
  } # End of while() loop
  
  # Return the schedule that was created
  return(run_schedule)
  
} # End of simulation() function

# Run the simulation
simulation()

# Create a list containing all the simulations
totallist = vector('list', 2) 
for (i in 1:2) {
  ref_data <- simulation()
  totallist[[i]] <- ref_data
}

# Viewing the previous lise
totallist

# Function that transform the list created above to a csv file that contains all simulations
df_total<-data.frame()
for (i in 1:32){
  df <- data.frame(totallist[[i]])
  df_total <- rbind(df_total, df)
  write.csv(df_total, "NJETS.csv")
}


