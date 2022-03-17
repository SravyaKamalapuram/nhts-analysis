## Basic analysis of NHTS(2017) data to understand the following: 
#1. Number of trips in the sample by state 
#2. Average trip duration and distance by mode by state 
#3. Percentage of households by number of vehicles owned 
#4. Number of trips by trip purpose in each state

##Citation 
##U.S. Department of Transportation, Federal Highway Administration, 2017 National Household Travel Survey. URL: http://nhts.ornl.gov.

##Author: Sravya Kamalapuram


##Load libraries 
library(tidyverse)
library(janitor)

#Read files 
files  <- list.files(path="./csv",
                      pattern="\\.csv$"
                     ,full.names=T)    

hh_df = read.csv(files[1])
person_df = read.csv(files[2])
trip_df = read.csv(files[3])
veh_df = read.csv(files[4])

#Required variables from trip file 
#travel duration in minutes - TRVELCMIN
#Trip distance in  miles - TRPMILES
#Trip mode - TRPTRANS
#State code - HHSTATE
#trippurp - Trip purpose 

#Clean column names
hh_df = clean_names(hh_df)
trip_df = clean_names(trip_df)

#Total number of trips taken by 1-walk, 2-bike, 3-car,11-transit and 17-taxi
num_trips = trip_df %>% 
  filter(trptrans %in% c(1,2,3,11,17)) %>%
  group_by(hhstate) %>%
  summarize(number = n())

write.csv(num_trips, "Number_of_trips.csv")

#Trip duration and trip duration by transport mode by hhstate by urban/rural
mode_state <- trip_df %>% 
              filter(trptrans %in% c(1,2,3,11, 17)) %>% 
              mutate(trptrans = recode(as.factor(trptrans),"1"="walk", "2"="bike", "3"="car","11"="transit", "17" = "Taxi(including Uber/Lyft)"), 
                     urbrur = recode(as.factor(urbrur), "1"= "Urban", "2" = "Rural")) %>% #recode values
              group_by(hhstate,trptrans, urbrur) %>% 
              summarize(avg_trip_duration = round(mean(trvlcmin),2), #calculate mean trip duration 
                        avg_trip_distance = round(mean(trpmiles),2), #calculate mean trip distance
                        number = n())  
names(mode_state) = c("State", "Mode", "Urban/Rural", "Average_trip_duration", "Average_trip_distance", "Number_of_trips")

write.csv(mode_state, "NHTS_1.csv")

#Number of trip by trip purpose in each state
trp_purpose <- trip_df %>% 
                filter(trptrans %in% c(1,2,3,11,17), trippurp != -9) %>% #Filter transport modes (walk, bike, car,transit and taxi) and trip purpose = "-9"i.e "not ascertained" trips 
                group_by(hhstate, trippurp) %>% 
                summarize(number = n()) 
write.csv(trp_purpose, "trip_purpose.csv")

#Count percentage of households by vehicle ownership in each state 
hh_df$hhvehcnt = ifelse(hh_df$hhvehcnt %in% c(5,6,7,8,9,10,11,12), "5 or greater", hh_df$hhvehcnt) #Recode vehicle count >=5

#summarize number of households in each state
number_of_households = hh_df %>% 
        group_by(hhstate) %>% 
        summarise(number_of_households = length(unique(houseid)))

#Calculate percentage of households by number of vehicles owned 
household_veh_own = hh_df %>%
                    group_by(hhstate,hhvehcnt) %>% 
                    summarise(number_hh = length(unique(houseid))) %>% 
                    left_join(y = number_of_households, by = "hhstate") %>% 
                    mutate(per_hh = round(number_hh/number_of_households*100,2)) %>% 
                    arrange(hhstate, hhvehcnt) %>% 
                    select(hhstate,hhvehcnt, per_hh)

write.csv(household_veh_own, "household_veh_own.csv")        
