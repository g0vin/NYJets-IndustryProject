#Import libraries
library(tibble)
library(purrr)
library(dplyr)
library(MASS)

# Create a function called generate_schedule that will generate schedules 
generate_schedule <- function(){
  success <- FALSE
  
  # While loop that generates unique schedules subject to hard and soft constraints
  while (!success) {
    
    # Possible days and times for games
    possible_days <- c("Thursday_820", "Saturday_430","Saturday_820","Sunday_100",
                       "Sunday_405","Sunday_425","Sunday_820", "Monday_815")

    # Possible opponents the Jets will play for the given season
    possible_opponents <- c("Bills", "Dolphins", "Patriots", "Jaguars",
                            "Titans", "Saints","Buccaneers", "Bengals") 
    
    # Possible weeks for the season up to 16 weeks
    possible_week <- c(1:16)
    
    # Create tibble on the possible game day/time and weeks played
    sched = tibble(
      time = sample(possible_days, 8, replace=TRUE),
      week = sample(possible_week, 8, replace = FALSE)
    )
    
    # Primetime constraints
    sched$prime_time <- ifelse(sched$time=="Sunday_100", 0, 
                               ifelse(sched$time=="Sunday_425",0, 
                                      ifelse(sched$time=="Sunday_405",0,1)))
    
    # Calculated rivalry and quality scores for each opponent
    rivalry_score <- c(2,3,3,1,1,1,2,2)
    quality_score <- c(3,2,1,1,2,2,3,1)
    total_score = rivalry_score + quality_score
    
    # Create a tibble on opponents and their associated score
    opponents_score <- tibble(
      possible_opponents,
      total_score
    )
    
    # Calculate the probability of games being primetime given the total_score
    prob_prime <- ifelse(total_score==5, 0.20, ifelse(total_score==4, 0.15,
                                                      ifelse(total_score==3, 0.10, 0.05)))
    prob_not_prime <- ifelse(total_score==5, 0.06, ifelse(total_score==4, 0.12,
                                                          ifelse(total_score==3, 0.15, 0.25)))
    
    sched$opponent[1] = ifelse(sched$prime_time[1] == 1, 
                               sample(possible_opponents, 1, prob=prob_prime, 
                                      replace=FALSE), 
                               sample(possible_opponents, 1, prob=prob_not_prime, 
                                      replace=FALSE))
    
    # Match the primetime scores with the opponents
    x_1<-match(sched$opponent[1],possible_opponents)
    possible_opponents <- possible_opponents[-c(x_1)]
    total_score <- total_score[-c(x_1)]
    
    
    prob_prime <- ifelse(total_score==5, 0.20, ifelse(total_score==4, 0.15,
                                                      ifelse(total_score==3, 0.10, 0.05)))
    prob_not_prime <- ifelse(total_score==5, 0.06, ifelse(total_score==4, 0.12,
                                                          ifelse(total_score==3, 0.15, 0.25)))
    
    
    sched$opponent[2] = ifelse(sched$prime_time[2] == 1, 
                               sample(possible_opponents, 1, prob=prob_prime, 
                                      replace=FALSE), 
                               sample(possible_opponents, 1, prob=prob_not_prime, 
                                      replace=FALSE))
    
    x_1<-match(sched$opponent[2],possible_opponents)
    possible_opponents <- possible_opponents[-c(x_1)]
    total_score <- total_score[-c(x_1)]
    
    prob_prime <- ifelse(total_score==5, 0.20, ifelse(total_score==4, 0.15,
                                                      ifelse(total_score==3, 0.10, 0.05)))
    prob_not_prime <- ifelse(total_score==5, 0.06, ifelse(total_score==4, 0.12,
                                                          ifelse(total_score==3, 0.15, 0.25)))
    
    
    sched$opponent[3] = ifelse(sched$prime_time[3] == 1, 
                               sample(possible_opponents, 1, prob=prob_prime, 
                                      replace=FALSE), 
                               sample(possible_opponents, 1, prob=prob_not_prime, 
                                      replace=FALSE))
    
    x_1<-match(sched$opponent[3],possible_opponents)
    possible_opponents <- possible_opponents[-c(x_1)]
    total_score <- total_score[-c(x_1)]
    
    prob_prime <- ifelse(total_score==5, 0.20, ifelse(total_score==4, 0.15,
                                                      ifelse(total_score==3, 0.10, 0.05)))
    prob_not_prime <- ifelse(total_score==5, 0.06, ifelse(total_score==4, 0.12,
                                                          ifelse(total_score==3, 0.15, 0.25)))
    
    
    sched$opponent[4] = ifelse(sched$prime_time[4] == 1, 
                               sample(possible_opponents, 1, prob=prob_prime, 
                                      replace=FALSE), 
                               sample(possible_opponents, 1, prob=prob_not_prime, 
                                      replace=FALSE))
    
    x_1<-match(sched$opponent[4],possible_opponents)
    possible_opponents <- possible_opponents[-c(x_1)]
    total_score <- total_score[-c(x_1)]
    
    prob_prime <- ifelse(total_score==5, 0.20, ifelse(total_score==4, 0.15,
                                                      ifelse(total_score==3, 0.10, 0.05)))
    prob_not_prime <- ifelse(total_score==5, 0.06, ifelse(total_score==4, 0.12,
                                                          ifelse(total_score==3, 0.15, 0.25)))
    
    
    sched$opponent[5] = ifelse(sched$prime_time[5] == 1, 
                               sample(possible_opponents, 1, prob=prob_prime, 
                                      replace=FALSE), 
                               sample(possible_opponents, 1, prob=prob_not_prime, 
                                      replace=FALSE))
    
    x_1<-match(sched$opponent[5],possible_opponents)
    possible_opponents <- possible_opponents[-c(x_1)]
    total_score <- total_score[-c(x_1)]
    
    prob_prime <- ifelse(total_score==5, 0.20, ifelse(total_score==4, 0.15,
                                                      ifelse(total_score==3, 0.10, 0.05)))
    prob_not_prime <- ifelse(total_score==5, 0.06, ifelse(total_score==4, 0.12,
                                                          ifelse(total_score==3, 0.15, 0.25)))
    
    
    sched$opponent[6] = ifelse(sched$prime_time[6] == 1, 
                               sample(possible_opponents, 1, prob=prob_prime, 
                                      replace=FALSE), 
                               sample(possible_opponents, 1, prob=prob_not_prime, 
                                      replace=FALSE))
    
    x_1<-match(sched$opponent[6],possible_opponents)
    possible_opponents <- possible_opponents[-c(x_1)]
    total_score <- total_score[-c(x_1)]
    
    prob_prime <- ifelse(total_score==5, 0.20, ifelse(total_score==4, 0.15,
                                                      ifelse(total_score==3, 0.10, 0.05)))
    prob_not_prime <- ifelse(total_score==5, 0.06, ifelse(total_score==4, 0.12,
                                                          ifelse(total_score==3, 0.15, 0.25)))
    
    
    sched$opponent[7] = ifelse(sched$prime_time[7] == 1, 
                               sample(possible_opponents, 1, prob=prob_prime, 
                                      replace=FALSE), 
                               sample(possible_opponents, 1, prob=prob_not_prime, 
                                      replace=FALSE))
    
    x_1<-match(sched$opponent[7],possible_opponents)
    possible_opponents <- possible_opponents[-c(x_1)]
    total_score <- total_score[-c(x_1)]
    
    prob_prime <- ifelse(total_score==5, 0.20, ifelse(total_score==4, 0.15,
                                                      ifelse(total_score==3, 0.10, 0.05)))
    prob_not_prime <- ifelse(total_score==5, 0.06, ifelse(total_score==4, 0.12,
                                                          ifelse(total_score==3, 0.15, 0.25)))
    
    
    sched$opponent[8] = ifelse(sched$prime_time[8] == 1, 
                               sample(possible_opponents, 1, prob=prob_prime, 
                                      replace=FALSE), 
                               sample(possible_opponents, 1, prob=prob_not_prime, 
                                      replace=FALSE))
    sched <- as.data.frame(sched)
    sched <- sched %>% arrange(week)
  
    
    # Hard Constraints
    x<-ifelse(
      # Can't play Thursday if played Monday the week before
      sched$time[1]=="Monday_815" & sched$time[2]=="Thursday_820" |
        sched$time[2]=="Monday_815" & sched$time[3]=="Thursday_820" |
        sched$time[3]=="Monday_815" & sched$time[4]=="Thursday_820"|
        sched$time[4]=="Monday_815" & sched$time[5]=="Thursday_820"|
        sched$time[5]=="Monday_815" & sched$time[6]=="Thursday_820"|
        sched$time[6]=="Monday_815" & sched$time[7]=="Thursday_820"|
        sched$time[7]=="Monday_815" & sched$time[8]=="Thursday_820"|
      # Saturday games on weeks 15 and 16 only
        sched$time[1] == "Saturday_430"| sched$time[2] == "Saturday_430"
      | sched$time[3] == "Saturday_430"| sched$time[4] == "Saturday_430"
      | sched$time[5] == "Saturday_430"| sched$time[6] == "Saturday_430"
      | sched$time[1] == "Saturday_820"| sched$time[2] == "Saturday_820"
      | sched$time[3] == "Saturday_820"| sched$time[4] == "Saturday_820"
      | sched$time[5] == "Saturday_820"| sched$time[6] == "Saturday_820"
      | sched$time[7] == "Saturday_820" & sched$week[7]<15
      | sched$time[8] == "Saturday_820" & sched$week[8]<15
      | sched$time[7] == "Saturday_430" & sched$week[7]<15
      | sched$time[8] == "Saturday_430" & sched$week[8]<15
      # No more than 3 homes games in a row
      | sched$week[4] - sched$week[1] < 4
      | sched$week[5] - sched$week[2] < 4
      | sched$week[6] - sched$week[3] < 4
      | sched$week[7] - sched$week[4] < 4
      | sched$week[8] - sched$week[5] < 4
      # No more than 3 away games in a row
      | sched$week[2] - sched$week[1] > 3
      | sched$week[3] - sched$week[2] > 3
      | sched$week[4] - sched$week[3] > 3
      | sched$week[5] - sched$week[4] > 3
      | sched$week[6] - sched$week[5] > 3
      | sched$week[7] - sched$week[6] > 3
      | sched$week[8] - sched$week[7] > 3
      # Can't play same day every week
      | ("Sunday" %in% sched$week[1] & "Sunday" %in% sched$week[2] &
           "Sunday" %in% sched$week[3] & "Sunday" %in% sched$week[4] &
           "Sunday" %in% sched$week[5] & "Sunday" %in% sched$week[6] &
           "Sunday" %in% sched$week[7] & "Sunday" %in% sched$week[8])
      | ("Monday" %in% sched$week[1] & "Monday" %in% sched$week[2] &
           "Monday" %in% sched$week[3] & "Monday" %in% sched$week[4] &
           "Monday" %in% sched$week[5] & "Monday" %in% sched$week[6] &
           "Monday" %in% sched$week[7] & "Monday" %in% sched$week[8])
      | ("Thursday" %in% sched$week[1] & "Thursday" %in% sched$week[2] &
           "Thursday" %in% sched$week[3] & "Thursday" %in% sched$week[4] &
           "Thursday" %in% sched$week[5] & "Thursday" %in% sched$week[6] &
           "Thursday" %in% sched$week[7] & "Thursday" %in% sched$week[8])
      
      
      ,0,1) 
    # Updates the success variable
    success <- x > 0
  } # End of while() loop
  
  # Return the schedule using week, time, and opponent
  var <- c("week", "time", "opponent")
  sched <- sched[var]
  return(sched)
} # End of generate_schedule() function

# Run and view the generate_schedule() function
generate_schedule()
view(generate_schedule())



