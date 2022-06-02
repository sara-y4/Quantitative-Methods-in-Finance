#####Problem 1#####

ResultVector <- NULL
for(i in 1:1000){
  Budget <- 100
  Bet <- 1
  BetNumber <- 0
  while (Budget > 0) {
    BetNumber <- BetNumber + 1
    if (Bet > Budget){
      Bet <- Budget
    }
    cointoss <- sample (c("win", "lose"), 1, prob = c(0.486, 0.514))
    if(cointoss == "win"){
      Budget <- Budget + Bet
      Bet <- 1
    } else {
      Budget <- Budget - Bet
      Bet <- Bet * 2
    }
  }
  ResultVector <- c(ResultVector, BetNumber)
}

library(nycflights13)
library(tidyverse)
flights <- flights

####Exercises 5.2.4#####
#Question 1
number_one <- dplyr::filter(flights, arr_delay >= 120)
number_two <- dplyr::filter(flights, dest == "HOU" | dest == "IAH")
number_three <- dplyr::filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL")
number_four <- dplyr::filter(flights, month == 7 | month == 8 | month == 9)
number_five <- dplyr::filter(flights, arr_delay > 120 & dep_delay <= 0)
number_six <- dplyr::filter(flights, arr_delay >= 60 & (dep_delay - arr_delay) <= 30)
number_seven <- dplyr::filter(flights, hour == 6 | hour == 24)

#Question 2
example <- dplyr::filter(flights, dplyr::between(dep_time, 0, 600))
##between() can be used in order to simplify inequalities.

#Question 3
miss_time <- dplyr::count(dplyr::filter(flights, is.na(dep_time)))
print(miss_time)
#8255 flights have a missing dep. time. 
#Other varibales that are missing include dep_delay and arr_time. 
#This means the flight was either canceled or mischeduled. 

#Question 4 
#If you can't search for something that doesn’t exist, 
#an NA doesn’t exist in the data base rather a blank exists and an NA is returned.

####Exercises 5.3.1####
#Question 1
flights %>% 
  dplyr::arrange(desc(is.na(dep_time)),
                 desc(is.na(dep_delay)),
                 desc(is.na(arr_time)), 
                 desc(is.na(arr_delay)),
                 desc(is.na(tailnum)),
                 desc(is.na(air_time)))

#Question 2
earliest <- dplyr::arrange(flights, dep_delay)
most_delayed <- dplyr::arrange(flights, desc(dep_delay))

#Question 3
fastest <- dplyr::arrange(flights, air_time)

#Question 4
shortest <- dplyr::arrange(flights, distance)
farthest<- dplyr::arrange(flights, desc(distance))

####Exercises 5.4.1####
#Question 1
first_select <- dplyr::select(flights, dep_time, dep_delay, arr_time, arr_delay)

#Question 2
second_select <- dplyr::select(flights, year, month, day, month, day)
#When we use View() it shows each variable only once (not multiple times).

#Question 3
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
dplyr::select(flights, one_of(vars))
#one_of() function allows you to select from a character vector.

#Question 4
code_test <- dplyr::select(flights, contains("TIME"))
empty <- dplyr::select(flights, contains("TIME",ignore.case = FALSE))

####Exercises 5.5.2####
#Question 1
convert_1 <- transmute(flights,
                       dep_time,
                       dep_time_minutes = (dep_time %/% 100)*60 + (dep_time %% 100) 
)

convert_2 <- transmute(flights,
                       sched_dep_time,
                       sched_dep_time_minutes = (sched_dep_time %/% 100)*60 + (sched_dep_time %% 100) 
)
#Question 2
flights_airtime <-
  dplyr::mutate(flights,
                dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
                air_time_diff = air_time - arr_time + dep_time
  )

#Question 3
flights_deptime <-
  dplyr::mutate(flights,
                dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                sched_dep_time_min = (sched_dep_time %/% 100 * 60 +
                                        sched_dep_time %% 100) %% 1440,
                dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
  )

#Question 4
head(arrange(flights,desc(row_number(flights$dep_delay))), 10)
#TIES WILL BE BROKEN WHILE FIRST WILL GET SMALLER RANK

head(arrange(flights,desc(min_rank(flights$dep_delay))), 10)
# TIES WILL ALL GET THE SAME SMALLEST RANK

#Question 5
1:3 + 1:10
#The code also produces a warning that the shorter vector is not a multiple of the longer vector. 
#This indicates a bug in the code. That is why a warning is provided.

#Question 6
#All trigonometric functions are all described in a single help page, named Trig.
#All trigonometric functions that are available in R are the sine, cosine, and tangent method and their inverse methods.
