#####Problem 1#####

FactorialFunction <- function(x) {
  if (x == 0) {
    result <- 1
  } else {
    result <- x
    while(x > 1){
      result <- (x - 3) * result
      x <- x - 3
    }
  }
  return(result)
}

FactorialFunction(10)
FactorialFunction(0)


#####Problem 2#####
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
# SDFunction <- function(inputVector){
#   ???
#     return(Result)
# }
# ??? is not Result <- sd(inputVector)
#####Problem 2#####

a <- c(5, 1, 9, 6, 11, 14, 13, 19)

SDFunction <- function(inputvector){
  avg = sum(inputvector)/length(inputvector)
  SumDiff <- sum((inputvector - avg)^2) 
  FinalResult <- sqrt(sum((inputvector - avg)^2) / (length(inputvector) - 1))
  return(FinalResult)
}

SDFunction(a)

library(tidyverse)
library(nycflights13)

####Exercises 5.6.7####
###Question 1
delay_char <-
  flights %>%
  group_by(flight) %>%
  dplyr::summarise(n = n(),
                   fifteen_early = mean(arr_delay == -15, na.rm = TRUE),
                   fifteen_late = mean(arr_delay == 15, na.rm = TRUE),
                   ten_always = mean(arr_delay == 10, na.rm = TRUE),
                   thirty_early = mean(arr_delay == -30, na.rm = TRUE),
                   thirty_late = mean(arr_delay == 30, na.rm = TRUE),
                   percentage_on_time = mean(arr_delay == 0, na.rm = TRUE),
                   twohours = mean(arr_delay > 120, na.rm = TRUE)) %>%
  map_if(is_double, round, 2) %>%
  as_tibble()

#A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
delay_char %>%
  dplyr::filter(fifteen_early == 0.5, fifteen_late == 0.5)

#A flight is always 10 minutes late.
delay_char %>%
  filter(ten_always == 1)

#A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
delay_char %>%
  filter(thirty_early == 0.5 & thirty_late == 0.5)

#99% of the time a flight is on time. 1% of the time it’s 2 hours late.
delay_char %>%
  filter(percentage_on_time == 0.99 & twohours == 0.01)

#Which is more important: arrival delay or departure delay? - It depends.

###Question 2
not_cancelled <-
  flights %>% 
  dplyr::filter(!is.na(dep_delay), !is.na(arr_delay))


not_cancelled %>%
  group_by(dest) %>%
  summarise(n = n())

#and

not_cancelled %>%
  group_by(tailnum) %>%
  tally(wt = distance)

###Question 3
#Because if a flight didn’t leave then it was cancelled. 
#If the condition is.na(dep_delay) is met, then the flight was cancelled.

###Question 4
flights %>%
  group_by(day) %>%
  dplyr::summarise(cancelled = mean(is.na(dep_delay)),
                   mean_dep = mean(dep_delay, na.rm = T),
                   mean_arr = mean(arr_delay, na.rm = T)) %>%
  ggplot(aes(y = cancelled)) +
  geom_point(aes(x = mean_dep), colour = "red") +
  geom_point(aes(x = mean_arr), colour = "blue") +
  labs(x = "Avg delay per day", y = "Cancelled flights p day")

###Question 5
flights %>%
  summarise(n_car = n_distinct(carrier),
            n_air = n_distinct(dest),
            n_or = n_distinct(origin))

flights %>%
  group_by(carrier) %>%
  mutate(avg_carrier = mean(dep_delay, na.rm = T)) %>%
  group_by(carrier, origin) %>%
  mutate(origin_mean = mean(dep_delay, na.rm = T),
         deviations = origin_mean - avg_carrier) %>%
  summarise(deviations = mean(deviations), mean = mean(avg_carrier)) %>%
  ggplot(aes(origin, deviations)) + geom_col() + facet_wrap(~ carrier)

flights %>%
  group_by(carrier, dest) %>%
  summarise(mean_departure = mean(dep_delay, na.rm = T),
            mean_arrival = mean(arr_delay, na.rm = T))

###Question 6
flights %>%
  count(flight, sort = T)


####Exercises 5.7.1####
###Question 1
#It is applied to groups. 
#For example you can use mutate to create new variables by using group statistics specificly 
#or you can filter out an entire group.

###Question 2
flights %>%
  dplyr::filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(prop_time = sum(arr_delay <= 30)/n(),
            mean_arr = mean(arr_delay, na.rm = TRUE),
            fl = n()) %>%
  arrange(desc(prop_time))

###Question 3
flights %>%
  group_by(hour) %>%
  dplyr::filter(!is.na(dep_delay)) %>%
  dplyr::summarise( delay = mean( dep_delay > 0 , na.rm = T)) %>%
  ggplot(aes(hour, delay, fill = delay)) + geom_col() 

###Question 4
flights %>%
  group_by(dest) %>%
  dplyr::filter(!is.na(dep_delay)) %>%
  dplyr::summarise(tot_mins = sum(dep_delay[dep_delay > 0]))

flights %>%
  dplyr::filter(!is.na(dep_delay)) %>%
  group_by(tailnum, dest) %>%
  dplyr::summarise(m = mean(dep_delay > 0), n = n()) %>%
  arrange(desc(m))

###Question 5
flights %>%
  select(year, month, day, hour, dest, dep_delay) %>%
  group_by(dest) %>%
  dplyr::mutate(lag_delay = lag(dep_delay)) %>%
  arrange(dest) %>%
  dplyr::filter(!is.na(lag_delay)) %>%
  dplyr::summarize(cor = cor(dep_delay, lag_delay, use = "complete.obs"),
                   n = n()) %>%
  arrange(desc(cor)) %>%
  dplyr::filter(row_number(desc(cor)) %in% 1:10)

###Question 6
# (1)
flights %>%
  group_by(dest) %>%
  arrange(air_time) %>%
  slice(1:5) %>%
  select(tailnum, sched_dep_time, sched_arr_time, air_time) %>%
  arrange(air_time)

# (2)
flights %>%
  group_by(dest) %>%
  dplyr::mutate(shortest = air_time - min(air_time, na.rm = T)) %>%
  top_n(1, air_time) %>%
  arrange(-air_time) %>%
  select(tailnum, sched_dep_time, sched_arr_time, shortest)

###Question 7
flights %>%
  group_by(dest) %>%
  dplyr::filter(n_distinct(carrier) > 2) %>%
  group_by(carrier) %>%
  dplyr::summarise(n = n_distinct(dest)) %>%
  arrange(-n)

###Question 8
not_cancelled %>%
  group_by(origin, tailnum) %>%
  dplyr::summarise(
    count = n(),
    agg_dep_delay = sum(cumsum(dep_delay > 60) < 1)
  )


#####Problem 4 - Solution#####
library(tidyverse)
library(tidyquant)
library(nycflights13)
View(flights)

#4.1
common_dest <- flights %>% 
  group_by(carrier, dest) %>%
  select(carrier, dest) %>%
  summarise(NrOfFlights = n()) %>%
  arrange(desc(NrOfFlights)) %>%
  slice_head()

#4.2
biggest_delay <- flights %>%
  arrange(desc(arr_delay)) %>%
  group_by(carrier) %>%
  slice_head()

#4.3
most/least_miles <- not_cancelled %>%
  dplyr::group_by(tailnum) %>%
  dplyr::summarise(TotalDistance = base::sum(distance))  %>%
  dplyr::arrange(dplyr::desc(TotalDistance))%>%
  dplyr::slice(1:3, (n()-2):n())%>%
  dplyr::ungroup()

#4.4
sss <- flights %>%
  filter(year == 2013,
         month == 2,
         !is.na(dep_time)) %>%
  arrange(day, dep_time) %>%
  group_by(day) %>%
  filter(row_number() == 1 |
           row_number() == n()) %>%
  ungroup()

#4.5
most/least_miles_company <- not_cancelled %>%
  dplyr::filter(month == 3) %>%
  dplyr::group_by(carrier)%>%
  dplyr::mutate(TotalMiles = base::sum(distance))%>%
  dplyr::ungroup()%>%
  dplyr::arrange(dplyr::desc(TotalMiles))%>%
  dplyr::slice(1, n())

#4.6
most_delays_month <-not_cancelled %>%
  dplyr::group_by(month)%>%
  dplyr::summarise(TotalDelay = base::sum(arr_delay>60))%>%
  dplyr::ungroup()%>%
  dplyr::slice_max(TotalDelay)

#4.7
average_time <- not_cancelled %>%
  dplyr::summarise(TimebetweenFlights = base::mean(dep_time - dplyr::lag(dep_time), na.rm = TRUE))%>%
  base::round(digits = 3)

#4.8
SDFunction <- function(inputVector){
  Denominator = sum(inputVector)/length(inputVector)
  Nominator = sum((inputVector - Denominator)^2)
  Result <- sqrt(sum((inputVector - Denominator)^2)/(length(inputVector)-1))
  return(Result)
}  

n <- not_cancelled %>%
  dplyr::group_by(month,dest) %>%
  dplyr::summarise(sdDelay = SDFunction(arr_delay))%>%
  dplyr::ungroup() 