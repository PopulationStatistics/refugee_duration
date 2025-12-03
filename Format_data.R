######################################################################################
######################################################################################
#
#   FORMAT V4 DATASETS TO CALCULATE REFUGEE DURATION AND RUN DATA CHECKS
#
######################################################################################
######################################################################################
######################################################################################
#
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(survival)
library(survminer)
library(scales)
library(gganimate)
library(unhcrthemes)
library(stringr)

######################################################################################
######################################################################################
#
#         FIRST PART: FORMAT DATASET
#
#               1. Read in data and run some data checks
#               2. Calculate refugee duration by using current date
#               3. Create some plots to check distribution of registrations
#
######################################################################################
######################################################################################

################################################
################################################
#   Read in data
################################################
################################################
#
#     CHOOSE WHICH DATASET YOU WANT TO READ IN 
name <- "Syrians_in_Egypt.csv"
name <- "Syrians_in_Jordan.csv"
name <- "Syrians_in_Iraq.csv"
name <- "South_Sudanese_in_Sudan.csv"
name <- "South_Sudanese_in_Kenya.csv"
name <- "South_Sudanese_in_Ethiopia.csv"
name <- "South_Sudanese_in_Uganda.csv"
name <- "Sudanese_in_Chad.csv"
name <- "burundis_in_tanzania.csv"

dataset <- read.csv(paste0("Raw Data/", name))

table(dataset$ProcessStatusName)

table(as.factor(dataset$COAName))

################################################
################################################
#   Run some data checks
################################################
################################################

# 1. Check if there are missing registration or arrival dates
dataset[which(is.na(dataset$RegistrationDate)),]
dataset[which(is.na(dataset$ArrivalDate)),]

# 2. Check that all closed and inactive cases have a processstatusdate which will be used as the end date
dataset %>%
  filter(ProcessStatusName %in% c("Closed", "Inactive") & is.na(ProcessStatusDate))

# Check if process date is ever before registration date or reg date before arrival date
check <- dataset %>%
  filter(ProcessStatusDate < RegistrationDate & ProcessStatusName %in% c("Closed", "Inactive"))

check2 <- dataset %>%
  filter(RegistrationDate < ArrivalDate)

min(dataset$RegistrationDate)
min(dataset$ArrivalDate)

dataset1 <- dataset %>%
  filter(ProcessStatusName != "Erroneous" & !((RegistrationDate>as.Date(Sys.Date())|RegistrationDate>ProcessStatusDate|ProcessStatusDate>as.Date(Sys.Date())) & ProcessStatusName %in% c("Closed", "Inactive")) & 
           RegistrationReasonName != "New Birth") %>%
  mutate( registration_year = year(RegistrationDate),
          duration_registration = ifelse(ProcessStatusName %in% c("Closed", "Inactive") & ProcessStatusDate<as.Date("2021-12-31"),
                                         round(difftime(as.Date(ProcessStatusDate), as.Date(RegistrationDate), units = "weeks")/52.25, digits=2),
                                         round(difftime(as.Date(Sys.Date()), as.Date(RegistrationDate), units = "weeks")/52.25, digits=2)),
          duration_registration = ifelse(duration_registration<0, 0, duration_registration),
          duration_arrival = ifelse(ProcessStatusName %in% c("Closed", "Inactive") & ProcessStatusDate<as.Date("2021-12-31"),
                                    round(difftime(as.Date(ProcessStatusDate), as.Date(ArrivalDate), units = "weeks")/52.25, digits=2),
                                    round(difftime(as.Date(Sys.Date()), as.Date(ArrivalDate), units = "weeks")/52.25, digits=2)),
          event = ifelse(ProcessStatusName %in% c("Closed", "Inactive"), 1, 0))

new_name <- str_trim(tolower(gsub(".csv", "", name)))
assign(new_name, dataset1)
save(dataset1, file=paste0("Formatted Data/", new_name, ".RData"))

processStatus <- table(dataset1$ProcessStatusName)
round(processStatus/sum(processStatus)*100, digits=2)
#
#   Calculate duration by using registration date or arrival date as start date
#
duration <- dataset1 %>%
  mutate( year = year(RegistrationDate)) %>%
  group_by(processStatusName, year) %>%
  summarise( avg_duration = mean(duration_registration),
             median_duration = median(duration_registration),
             n = n())

duration_arrival <- dataset1 %>%
  mutate( year = year(ArrivalDate)) %>%
  group_by(processStatusName, year) %>%
  summarise( avg_duration = mean(duration_arrival),
             median_duration = median(duration_arrival),
             n = n())
#
#   Number of newly registered cases by year
#
ggplot() +
  geom_bar(data=duration, aes(year, n, colour=processStatusName), stat="identity") +
  ggtitle(" Number of newly registered cases by year ") +
  xlab("Registration Year")

ggplot() +
  geom_bar(data=filter(duration_arrival, year>=2010), aes(year, n, colour=processStatusName), stat="identity") +
  ggtitle(" Number of newly registered cases by year ") +
  xlab("Arrival Year")

all_data <- rbind(syrians_in_egypt,
                  syrians_in_jordan)

ggplot(data = filter(all_data, registration_year>2010),
       aes(x=registration_year)) +
  geom_bar() +
  ggtitle(" Number of newly registered cases by year ") +
  xlab("Registration Year") +
  ylab("Registered individuals") +
  facet_wrap(~CountryName) +
  scale_x_continuous(breaks= pretty_breaks())

