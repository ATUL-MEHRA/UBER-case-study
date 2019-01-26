########################################################UBER###############################################################################

# Loading the Uber Dataset

library(readr)
library(stringr)
library(tidyr)

Uber <- read_csv("Uber Request Data.csv ")

str(Uber)

########################################Data Cleaning & Preparation###################################################

sum(is.na(Uber))

# I didn't find any missing value & unnecessary NA in the dataset.


# Convert the date into a single format.

Uber$`Request timestamp` <- str_replace_all(Uber$`Request timestamp`,"/","-")

Uber$`Drop timestamp` <- str_replace_all(Uber$`Drop timestamp`,"/","-")


Uber_separated <- separate(Uber,'Request timestamp', into = c("req_date", "req_time"), sep = " ")

Uber_separated <- separate(Uber_separated,'Drop timestamp', into = c("drop_date", "drop_time"), sep = " ")

Uber_separated$req_date  <- as.Date(Uber_separated$req_date, format = "%d-%m-%Y")

Uber_separated$drop_date <- as.Date(Uber_separated$drop_date, format = "%d-%m-%Y")


# Extracting Hour from Request time column


Uber_separated$Req_date_time <- with(Uber_separated, as.POSIXct(paste(req_date,req_time), format = "%Y-%m-%d %H"))

Uber_separated$Hour <- substr(Uber_separated$Req_date_time, 12,13)

Uber_separated$Hour <- as.numeric(Uber_separated$Hour) 


########################################################################################################################
# 1.  Visually identify the most pressing problems for Uber. 
########################################################################################################################


#####################frequency of requests that get cancelled or show 'no cars available'###############################


library(ggplot2)


ggplot(Uber_separated,aes(x=Status,fill=`Pickup point`)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle(" Frequency of requests that get \n 'cancelled' or show 'no cars available'")


# From plot it can be observed that"No Cars Available" status from Airport to city having maximum frquency.



########################################################################################################################
## Identify the most problematic types of requests (city to airport / airport to city etc.) 
## and the time slots (early mornings, late evenings etc.) using plots.
########################################################################################################################


# Morning            9 am to 10 am
# Early morning      5 to 8 am
# Late morning      11 am to 12pm
# Early afternoon   1 to 3pm
# Late afternoon    4 to 5pm
# Early evening     5 to 7 pm
# Late Evening      8 pm to 9 pm
# Night             10 pm to 12 am
# Late Night        12 am to 4 am

Hour <- c(0:4,5:8,9:10,11:12,13:15,16:17,18:19,20:21,22:23)

Time_Slot <- c("Late Night","Early Morning","morning","Late morning","Early Afternoon",
               "Late afternoon","Early Evening","Late evening","Night")

Time_rep <- rep(Time_Slot, c(5,4,2,2,3,2,2,2,2))

Hour_df <- data.frame(Hour,Time_rep)


# Merge the Hour_df with Uber_separated data frame.

Time_merge <- merge(x= Uber_separated, y= Hour_df, by = "Hour", all.x = T)


# Plot describing request made from Airport & City in various days.

ggplot(Time_merge,aes(x=Req_date_time,fill=`Pickup point`)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Plot describing request made from \n Airport & City")


# Plot describing request made from Airport & City in different Time Slots.


ggplot(Time_merge,aes(x=Time_rep,fill=`Pickup point`)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Request made from \n Airport & City in different time of the days")


# From graph most of the request are made on early evening (6-7 PM) from Airport to City, 
# early morning (5-8 AM) from city to Airport.


#################################### Creating subset for request made from Airport#######################################

Air_req <- subset(Time_merge, Time_merge$`Pickup point` == "Airport")

# Plot Request made from Airport datewise.

ggplot(Air_req, aes(x=Req_date_time, fill= Status)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90))

# while checking status it mostly showing 'No Cars Available' from "Airport".

ggplot(Air_req, aes(x=Time_rep, fill= Status)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90))+ ggtitle("Request made from Airport to city")

# From graph above majority of 'No Cars Available' status appears
# in "Early Evening" & "Late Evening" timmings (6-9 PM)


##################################### Creating subset for request made from City#################################################


City_req <- subset(Time_merge, Time_merge$`Pickup point` == "City")


# Plot Request made from City datewise

ggplot(City_req, aes(x=Req_date_time, fill= Status)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90))

# while checking status it mostly showing 'Cancelled' from "City".

 ggplot(City_req, aes(x=Time_rep, fill= Status)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Request made from city to Airport")

# From graph above majority of 'Cancelled' status appears
# in "Early Morning" & "Morning" timmings (5-10 AM)



#########################################################################################################################
# 2.Find out the gap between supply and demand and show the same using plots.
#########################################################################################################################
 
 
# 1. Percentage of total number of requests raised vs trip completed is 41.97%

trip_completed <- length(which(Time_merge$Status == "Trip Completed"))

request_raised <- length(Time_merge$Status)

perc_complete <- (trip_completed/request_raised)*100

perc_complete

# 2. Percentage of total number of requests raised vs cancelled by drivers is 18.74%.

trip_cancell <- length(which(Time_merge$Status == "Cancelled"))

request_raised <- length(Time_merge$Status)

perc_cancell <- (trip_cancell/request_raised)*100

perc_cancell

# 3.Percentage of total number of requests raised vs cars not available is 39.29%.

trip_nocar <- length(which(Time_merge$Status == "No Cars Available"))

request_raised <- length(Time_merge$Status)

perc_nocar <- (trip_nocar/request_raised)*100

perc_nocar

####################################################################################################
# Plot Analysis : 
# Find the types of requests (city-airport or airport-city) 
# for which the gap is the most severe in the identified time slots
####################################################################################################

# Creating a Subset having timing "Early Morning" & "Late morning"

Morning_cancell <- subset(Time_merge, Time_merge$Time_rep == "Early Morning" | Time_merge$Time_rep == "Late morning")

ggplot(Morning_cancell, aes(x=Status, fill= Morning_cancell$`Pickup point`)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90))

# Most of request cancelled during Morning hours made from city to Airport.




# Creating a Subset having timing "Early Evening" & "Late Evening".

Evening_no_car <- subset(Time_merge, Time_merge$Time_rep == "Early Evening" | Time_merge$Time_rep == "Late Evening")

ggplot(Evening_no_car, aes(x=Status, fill= Evening_no_car$`Pickup point`)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90))


# Most of request Showed "No Cars Available" during Evening hours made from Airport to City.

#########################################################################################################################
############################################### Quantative Analysis######################################################
#########################################################################################################################



#############################################Cancellation Analysis########################################################


Tot_can <- length(which(Morning_cancell$Status == "Cancelled"))

City_canc <- length(which(Morning_cancell$Status == "Cancelled" & Morning_cancell$`Pickup point` == "City"))

Air_canc <- length(which(Morning_cancell$Status == "Cancelled" & Morning_cancell$`Pickup point` == "Airport"))

# percentage of request cancelled from city in problematic Morning Time slot.

Perc_City_canc <- City_canc/Tot_can * 100


# 95.72 % of request got cancelled from city in Morning Timeslot.

Perc_City_canc

# percentage of request cancelled from Airport in problematic Morning Time slot.

Perc_Air_canc  <- Air_canc/Tot_can * 100

# 4.27 % of request got cancelled from Airport in Morning Timeslot.

Perc_Air_canc

##########################################"No Cars Available Analysis"###################################################


Tot_No_car <- length(which(Evening_no_car$Status == "No Cars Available"))

City_No_car <- length(which(Evening_no_car$Status == "No Cars Available" & Evening_no_car$`Pickup point` == "City"))

Air_No_car <- length(which(Evening_no_car$Status == "No Cars Available" & Evening_no_car$`Pickup point` == "Airport"))

# percentage of request cancelled from city in problematic Evening Time slot.

Perc_City_No_car <- City_No_car/Tot_No_car * 100


# 4.63 % of request show "No Cars Available" from city in Evening Timeslot.

Perc_City_No_car

# percentage of request show "No Cars Available" from Airport in problematic Evening Time slot.

Perc_Air_No_car <- Air_No_car/Tot_No_car * 100

# 95.37 % of request show "No Cars Available" from Airport in problematic Evening Time slot.

Perc_Air_No_car

#######################################Gap Analysis using Plot##################################


# new column "Demand" has ben created which is actually "total No. of request made"

Time_group <- group_by(Time_merge, Hour)

Time_summarise <- summarize(Time_group, demand = sum(!is.na(Hour)))


# subset of request which have been completed.

Trp_comp <- subset(Time_group, Time_group$Status == "Trip Completed")

Supp_group <- group_by(Trp_comp,Hour)

# column name "supp" created which show the requests that have been completed.

Supp_summary <- summarize(Supp_group, supp = sum(!is.na(Hour)))

Time_bind <- cbind(Hour_df,Time_summarise[,"demand"],Supp_summary[,"supp"])

# column "Gap" created which is "demand" - "supp".

Time_bind[,"Gap"] <- Time_bind[,"demand"]- Time_bind[,"supp"]

# Plot the graph depicting the demand- supply Gap in various Time Slots of a day.

ggplot(Time_bind,aes(x=Time_rep, y= Gap)) + geom_point()+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Plot describing the gap between Demand & Supply")


## This plot also show that the demand - Supply Gap is majorly 
##in Early Evening, Late Evening, Early Morning & Morning.


ggplot(Time_bind,aes(x=Hour, y= Gap)) + geom_point()+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Plot describing the gap between Demand & Supply")


## This plot also show that the demand - Supply Gap is majorly 
## in 5-9 AM & 6 - 9 PM.


#######################################################################################################
# 3.  What do you think is the reason for this issue for the supply-demand gap? 
# Write the answer in less than 100 words. You may accompany the write-up with plot(s).
#####################################################################################################

# From the above analysis it can be observed that most of the request got cancelled in morning hours made from the city


# Plot No. of request made from City.

ggplot(City_req, aes(x= Time_rep)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90))  + ggtitle("No. of request made from City")


# PLot No. Request made from the Airport.

ggplot(Air_req, aes(x= Time_rep)) + geom_histogram(stat="count", position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("No. of request made from Airport")


# Most of the requests were made in early Morning (5-8 AM) from city & from graph above 
# one can see the same time slot is most problematic in terms of cancellations. this can be due to 
# the reason that the number of request made in morning & late morning (9AM - 12 PM) from "Airport" are least 
# because of which driver need to wait to about 4 Hours. This could be the most reasonable reason
# that the drivers were cancelling the request made from City in early morning hours.




# From the above analysis it can be observed that most of the request showed "No cars Available"
# in "Early Evening" & "Late Evening" hours made from the Airport.



# Most of the requests were made in "Early Evening" & "Late Evening" (5-9 PM) from Airport & from 
# graph above one can see the same time slot is most problematic in terms of "No cars Available". 
# this can be due to the reason that the number of request made in nighr & late night (10PM - 4 PM)
# from "city" are least because of which driver need to wait to about 4 Hours. This could be the
# reason that the drivers were not available from Airport to city in "Early Evening" & "Late Evening" hours


# ANOTHER REASON can be that it is the sleeeping time and most of the drivers had off their services
# and head toward their homes. this is the reason that the driver this time are not cancelling the
# request instead made themselves unavailable.


####################################################################################################
######################## 4. Recommend some ways to resolve the supply-demand gap.######################
####################################################################################################

# The problem of "No cars Available" Airport to City can be rectified providing good incentive to 
# the drivers which encourages them to work in night hours or to incentivize them for their idle time 
# in Airport.

# Similarly Incentive to driver could help to fill the gap between supply & demand of cars in 
# morning hours.





