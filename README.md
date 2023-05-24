# RWFM

## Overview
RWFM is a comprehensive R package designed and developed to cater to the workforce management and call center analytics requirements. It offers a range of powerful tools and functions that can be utilized for a wide array of tasks related to managing call centers, staffing optimization.

## Installation
```
# install and load devtools
install.packages("devtools")
library(devtools)

# install and load RWFM from Github
devtools::install_github("tesfahunboshe/RWFM")
library("RWFM")
```
## Usage

### For a point data calculations
```
# ReqFTE(volume, aht, sla, slatime, intervallength)
ReqFTE(0.00278,2092.2,0.8,60,1) # calculate required FTEs

# Occupancy(volume, aht,intervallength, agentscount)
Occupancy(0.00278, 2092.2, 1,9) # calculate occupancy

# CAI(agents, volume,aht, intervallength)
CAI(9,0.00278, 2092.2, 1) # calculate  % calls answered immediately

# ASA(agents, volume,aht, intervallength)
ASA(9, 0.00278, 2092.2, 1) # calculate average speed of answering

# SL(agents, volume,aht, slatime, intervallength)
SL(9, 0.00278, 2092.2, 60,1) # calculate service level
```
### For dataframes with multiple data points
```
# read the data from a local file
data = read.csv("capacityPlanInput.csv")
head(data)

# calculate the metrics and add additional columns
(data$agents <- mapply(ReqFTE,data$volume,data$AHT,data$slaTarget,data$slaTime,data$interval))
(data$Occupancy <- mapply(Occupancy,data$volume,data$AHT,data$interval,data$agents))
(data$CAI <- mapply(CAI,data$agents,data$volume,data$AHT,data$interval))
(data$ASA <- mapply(CAI,data$agents,data$volume,data$AHT,data$interval))
(data$SL <- mapply(CAI,data$agents,data$volume,data$AHT,data$slaTarget,data$interval))

# save to a csv output
write.csv(data,"Output.csv",row.names = F)
```
### Schedule Generation
```
# read from a sample capacity plan
Cap = read.csv("capacityPlanOutput.csv")

## generate schedule for contract based staff with only full timers
answer = OptimumSchedule(Cap, shiftLength = 8,weeklyWorkingDays = 5,minEfficiency = 0.8,intervals = 24,partTime = F)
answer$agents # number of agents needed
answer$coverage # coverage per interval
answer$plot # coverage vs requirement plot
answer$schedule # schedule format per agent
answer$efficiency # scheduling efficiency


## generate schedule for contract based staff with full timers and part-timers
answer2 = OptimumSchedule(Cap, shiftLength = 8,weeklyWorkingDays = 5,minEfficiency = 1,intervals = 24,partTime = T,minShiftLength = 5)
answer2$agents # number of agents needed
answer2$coverage # coverage per interval
answer2$plot # coverage vs requirement plot
answer2$schedule # schedule format per agent
answer2$efficiency # scheduling efficiency


## generate schedule for gigeconomy workforce
answer3 = gigSchedule(Cap, shiftLength = 8,minEfficiency = 1,intervals = 24,minShiftLength = 7)
answer3$agents # number of agents needed
answer3$coverage # coverage per interval
answer3$plot # coverage vs requirement plot
answer3$schedule # schedule format per agent
answer3$efficiency # scheduling efficiency
```
### Checking coverage and efficiency
```
# capacity plan
Cap = read.csv("capacityPlanOutput.csv")

# schedule
schedule = read.csv("schedule.csv")
head(schedule)

cvrg = coverage(schedule,nDays = 7,intervals=24)
cvrg

## Check the scheduling efficiency
accuracy_F(cvrg,Cap)
```

## Getting help
This blog post and other future posts are main places to get help with ggplot2. Find the author 
