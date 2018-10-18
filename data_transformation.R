# Chap 5: Data Transformation
#
#
########

# We will use nycflights13 data for this. 

# Load libraries
library(tidyverse)
library(nycflights13)

attach(flights)

# Pick observations by their value filter() and view first 6 values of the result

head(firstmonth <- filter(flights, month==1, day == 1))

# To filter data based on one value or another
head(dec_or_june <- filter(flights, month == 6 | month ==12))

# To filter data based on values in different columns
head(carrier4 <-filter(flights, month > 7 & carrier=='B6'))

# For a list of evaluations instead of using or,you can use in
head(half_year <- filter(flights, month %in% c(2,4,6,8)))

## 5.2.4 EXERCISE ##

# 1. Flights with an arrival delay of two or more hours
head(flights1 <- filter(flights, arr_delay >= 120))

# 2. Flights to Houston (IAH or HOU)
head(flights2 <- filter(flights, dest == 'IAH' | dest =='HOU'))

# 3. Were operated by United, American or Delta
head(flights3 <- filter(flights, carrier %in% c('UA','AA','DL')))

# 4. Departed in summer (July, August and September)
head(flights4 <- filter(flights, month %in% c(7,8,9)))

# 5. Arrived more than two hours late but did not leave late
head(flights5 <- filter(flights, dep_delay == 0 & arr_delay>120))

# 6. Were delayed by atleast an hour, but made up over 30 minutes in flight
head(flight6 <- filter(flights, dep_delay>=60 & arr_delay<=30))

# 7. Departed between midnight and 6AM (inclusive). sing between()
head(flight7 <- filter(flights, between(dep_time, 0, 600)))

# How many flights have a missing dep_time
count(filter(flights, is.na(dep_time)))

# What other variables are missing? What might these rows represent?
sapply(flights, function(x) sum(is.na(x)))
# These rows probably represent cancelled flights


## 5.3.1 EXERCISE ##

# 1. Use arrange to sort all missing values at the start
z <- arrange(flights, !is.na(dep_time), dep_time)

d <- arrange(flights, desc(is.na(dep_time)))

# 2. Sort flights to find the most delayed flights. Find the flights that left the earliest
head(arrange(flights, desc(dep_delay)))
head(arrange(flights, dep_delay))

# 3. Sort flights to find the fastest flights.
# speed is distance over time. Should we create a new column of speed?
head(fastest <- arrange(flights, desc(distance/air_time)))  # For now, use the least air time/distance.

# 4. Which flights travelled the longest? Which travelled the shortest?
head(arrange(flights, air_time)) #  Shortest
tail(arrange(flights, air_time))# Longest

##  5.4.1 EXERCISE ##
select(flights,tailnum)

select(flights, starts_with('d'))

select(flights, contains('time'))

# 1. Ways to select dep_time, dep_delay, arr_time, and arr_delay
select(flights, dep_time, dep_delay, arr_time, arr_delay)

select(flights, starts_with('dep'), starts_with('arr'))

select(flights, 4, 6:7, 9)

# 2. What happens if you include the name of a variable multiple times in a select() call
select(flights, dep_time, dep_time, dep_time, dep_time) # Outputs only one instance of the column

# 3. What does the one_of() function do? Why might it be helpful in conjuction with the vector below
var <- c('year', 'month','day','dep_delay','arr_delay')
select(flights, one_of(var))

# 4. Does the result of runing the following code surprise you?
select(flights, contains('TIME')) # Outputs all variables contaning 'time'. Select is case insensitive.
select(flights, contains('TIME', ignore.case=FALSE)) # To set case sensitivity

### mutate() Create new variables with functions of existing variables ###
flights_sml <- select(flights,
                      year:day,
                      ends_with('delay'),
                      distance,
                      air_time
                      )
mutate(flights_sml,
       gain = dep_delay-arr_delay,
       speed = distance/air_time*60,
       hours = air_time/60,
       gain_per_hour = gain/hours
       )

# To only keep the new variables, use transmute()
transmute(flights,
          gain = dep_delay-arr_delay,
          hours = air_time/60,
          gain_per_hour = gain/hours)

transmute(flights,
          dep_delay,
          hours = dep_delay/60,
          hour = dep_delay%/%60,
          minute = dep_delay%%60
          )
 
## 5.5.2 EXERCISE ##
# 1. Curently dep_time and sched_dep_time are conventinent to look at, but hard
# to compute with because they're not really continuous numbers. Convert them to
# a more convenient representation of number of minutes since midnight.
times <- select(flights,
                   flight,
                   dep_time,
                   sched_dep_time,
                   dep_delay,
                   hour,
                minute,
                   arr_time,
                   sched_arr_time,
                   arr_delay,
                air_time
                   )
times_2 <- mutate(times, 
       dep_time_prop = dep_time*0.6,
       sched_dep_time_prop = sched_dep_time*0.6)

# 2. Compare air_time with arr_time + dep_time. What do you expect to see? What do you
# see? What do you need to do to fix it?


# 3. Compare dep_time, sched_dep_time and dep_delay. How would you expect those three
# numbers to be related?


# 4. Find the 10 most delayed flights using a ranking function. How do you want to handle
# ties? Carefully read the documentation for nin_rank()


# 5. What does 1:3 + 1:10 return? Why?


# 6. What trigonometric functions does R provide?



### summarise()  Collapses a data frame into a single row.

summarise(flights, delay= mean(dep_delay, na.rm=TRUE))

by_day <- group_by(flights, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


#(group_by((select(flights, 1:3, 'dest')), dest))
#(group_by(flights, dest))

# Exploring the relationship between the distance and average
# delay for each location. 
# Method 1

by_dest <- group_by(flights,dest)
delay_by_dest <- summarise(by_dest,
                           count = n(),
                           dist=mean(distance, na.rm = TRUE),
                           delay = mean(arr_delay, na.rm = TRUE)
                           )
delay <- filter(delay_by_dest, count>20, dest !='HNL')

# Plot relationship between average distance and average delay

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# It looks as though delays increase with distance up to ~750 miles
# and then decrease. Maybe as flights get longer there's more
# ability to make up delays in the air?

# Method 2 using pipes

delays <- flights %>%
  group_by(dest) %>%
  summarise( 
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != 'HNL')


# To count distinct number of carriers
flights %>%
  select(carrier) %>%
  n_distinct(carrier)

# To count number of each unique carrier
flights %>%
  count(carrier)
# Using a bar chart
ggplot(data = flights, mapping = aes(carrier)) +
  geom_bar()




# # Create a list of the data files to be imported
#nycflights <- c("data/airlines.csv", "data/airports.csv", "data/planes.csv", "data/weather.csv")
# 
# # Apply the function read_csv() to the data files in the list
#nycdata <-  lapply(nycflights, read_csv)
# 
# # Observe the data structure of the list of data files.
# str(nycdata, give.attr = FALSE)
# 
# # Extract individual items from the list of read in data
# airlines <-  nycdata[[1]]
# airports <- nycdata[[2]]
# planes <- nycdata[[3]]
# weather <-  nycdata[[4]]
# 
# head(airports)
# head(airlines)
# head(planes)
# head(weather)


