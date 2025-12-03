######################################################################################
######################################################################################
#
#   LOAD FORMATTED DATASETS AND RUN EXPLORATORY DATA ANALYSIS
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

file_names = as.list(paste0("Formatted data/", dir(path="Formatted data/")))

i=2

load(file_names[[i]])
######################################################################################
######################################################################################
#
#   SYRIA - READ FORMATTED DATA AND PLOT BARCHART OF REGISTERED CASES AND KM PLOTS
#
######################################################################################
######################################################################################
######################################################################################

load("Formatted Data/syrians_in_iraq.RData")
syrians_in_iraq <- dataset1
load("Formatted Data/syrians_in_jordan.RData")
syrians_in_jordan <- dataset1
load("Formatted Data/syrians_in_egypt.RData")
syrians_in_egypt <- dataset1

# Extract name
extract_name <- gsub("/|.R", "", str_extract(file_names[[i]], "/.*.R"))
assign(paste(extract_name), dataset1)

options(scipen = 999)

all_data_syria <- rbind(syrians_in_egypt, 
                        syrians_in_iraq, 
                        syrians_in_jordan)

reasons_syria <- filter(all_data_syria, ProcessStatusName %in% c("Inactive", "Closed")) %>%
  group_by(ProcessDetail) %>%
  summarise(n = n())
######################################################################################
#       Bar chart of registered cases by year since 2011
######################################################################################
registered_syrians_year <- ggplot(data = filter(all_data_syria, registration_year>2012),
                                  aes(x=registration_year),
                                  fill = unhcr_pal(n = 3, "pal_blue")) +
  geom_bar( stat="count") +
  ggtitle(" Number of newly registered Syrian cases by year ") +
  xlab("Registration Year") +
  ylab("Registered individuals") +
  facet_wrap(~COAName) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                     expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks= pretty_breaks(10))  +
  theme_unhcr(grid = "Y", axis_title = "y") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        panel.spacing = unit(1, "lines"))

save(registered_syrians_year, file="Plots/barchart_registered_syrians_year.RData")

######################################################################################
#       KM plots for registered cases from Syrians since 2013
######################################################################################

data_2 <- syrians_in_jordan[year(syrians_in_jordan$RegistrationDate) >="2013",]

fit2 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_2)

# Visualize with survminer
plot <- ggsurvplot(data = data_2,
                   fit2,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC') +
  labs(title = paste0("In ", unique(data_2$COAName) ," | registered from 2013"),
       y = "Refugee duration probability")

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_2$RegistrationDate[which(year(data_2$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)

plot_syrians_in_jordan <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.75, label=paste(round(fit2$surv[which(fit2$time==5)]*100), "%"),
           color="black")
  # geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_2$RegistrationDate[which(year(data_2$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
  # annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit2$surv[which(fit2$time==end_tp)]*100), "%"),
  #          color="black")

ggsave(file="Plots/plot_syrians_in_jordan.svg", plot=plot_syrians_in_jordan, width=10, height=8)
save(plot_syrians_in_jordan, file="Plots/plot_syrians_in_jordan.RData")

data_3 <- syrians_in_egypt[year(syrians_in_egypt$RegistrationDate) >="2013",]

fit3 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_3)

# Visualize with survminer
plot <- ggsurvplot(data = data_3,
                   fit3,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC') +
  labs(title = paste0("In ", unique(data_3$COAName) ," | registered from 2013"),
       y = "Refugee duration probability",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency")

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_3$RegistrationDate[which(year(data_3$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)

plot_syrians_in_egypt <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.75, label=paste(round(fit3$surv[which(fit3$time==5)]*100), "%"),
           color="black")
  # geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_3$RegistrationDate[which(year(data_3$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
  # annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit3$surv[which(fit3$time==end_tp)]*100), "%"),
  #          color="black")

table(filter(data_3, ProcessStatusName %in% c("Inactive", "Closed")))

reasons_egypt <- filter(data_3, ProcessStatusName %in% c("Inactive", "Closed")) %>%
  group_by(ProcessDetail) %>%
  summarise(n = n())

ggsave(file="Plots/plot_syrians_in_egypt.svg", plot=plot_syrians_in_egypt, width=10, height=8)
save(plot_syrians_in_egypt, file="Plots/plot_syrians_in_egypt.RData")

data_4 <- syrians_in_iraq[year(syrians_in_iraq$RegistrationDate) >="2013",]

fit4 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_4)

# Visualize with survminer
plot <- ggsurvplot(data = data_4,
                   fit4,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC',
                   risk.table=TRUE) +
  labs(title = paste0("In ", unique(data_4$COAName) ," | registered from 2013"),
       y = "Refugee duration probability")

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_4$RegistrationDate[which(year(data_4$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)

plot_syrians_in_iraq <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.75, label=paste(round(fit4$surv[which(fit4$time==5)]*100), "%"),
           color="black")
  # geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_4$RegistrationDate[which(year(data_4$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
  # annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit4$surv[which(fit4$time==end_tp)]*100), "%"),
  #          color="black")

ggsave(file="Plots/plot_syrians_in_iraq.svg", plot=plot_syrians_in_iraq, width=10, height=8)
save(plot_syrians_in_iraq, file="Plots/plot_syrians_in_iraq.RData")


######################################################################################
######################################################################################
#
#   SOUTH SUDAN - READ FORMATTED DATA AND PLOT BARCHART OF REGISTERED CASES AND KM PLOTS
#
######################################################################################
######################################################################################
######################################################################################

load("Formatted Data/south_sudanese_in_sudan.RData")
south_sudanese_in_sudan <- dataset1
load("Formatted Data/south_sudanese_in_kenya.RData")
south_sudanese_in_kenya <- dataset1
load("Formatted Data/south_sudanese_in_ethiopia.RData")
south_sudanese_in_ethiopia <- dataset1
load("Formatted Data/south_sudanese_in_uganda.RData")
south_sudanese_in_uganda <- dataset1

options(scipen = 999)

all_data_south_sudan <- rbind(south_sudanese_in_sudan,
                              south_sudanese_in_kenya, 
                              south_sudanese_in_ethiopia, 
                              south_sudanese_in_uganda)

reasons_south_sudan <- filter(all_data_south_sudan, ProcessStatusName %in% c("Inactive", "Closed")) %>%
  group_by(ProcessDetail) %>%
  summarise(n = n())
######################################################################################
#       Bar chart of registered cases by year since 2011
######################################################################################
registered_south_sudanese_year <- ggplot(data = filter(all_data_south_sudan, registration_year>2010),
                                  aes(x=registration_year),
                                  fill = unhcr_pal(n = 3, "pal_blue")) +
  geom_bar( stat="count") +
  ggtitle(" Number of newly registered South Sudanese cases by year ") +
  xlab("Registration Year") +
  ylab("Registered individuals") +
  facet_wrap(~COAName) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                     expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks= pretty_breaks(10),
                     limits      = c(2012, max(all_data_south_sudan$registration_year)))  +
  theme_unhcr(grid = "Y", axis_title = "y") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        panel.spacing = unit(1, "lines"))

save(registered_south_sudanese_year, file="Plots/barchart_registered_south_sudanese_year.RData")

######################################################################################
#       KM plots for registered cases from South Sudanese since 2014 or 2015
######################################################################################

data_2 <- south_sudanese_in_uganda[year(south_sudanese_in_uganda$RegistrationDate) >="2014",]

fit2 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_2)

# Visualize with survminer
plot <- ggsurvplot(data = data_2,
                   fit2,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC') +
  labs(title = paste0("In ", unique(data_2$COAName) ," | registered from 2014"),
       y = "Refugee duration probability")

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_2$RegistrationDate[which(year(data_2$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)

plot_south_sudanese_in_uganda <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
#  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.7, label=paste(round(fit2$surv[which(fit2$time==5)]*100), "%"),
           color="black")
# geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_2$RegistrationDate[which(year(data_2$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
# annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit2$surv[which(fit2$time==end_tp)]*100), "%"),
#          color="black")

ggsave(file="Plots/plot_south_sudanese_in_uganda.svg", plot=plot_south_sudanese_in_uganda, width=10, height=8)
save(plot_south_sudanese_in_uganda, file="Plots/plot_south_sudanese_in_uganda.RData")

data_3 <- south_sudanese_in_kenya[year(south_sudanese_in_kenya$RegistrationDate) >="2014",]

fit3 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_3)

# Visualize with survminer
plot <- ggsurvplot(data = data_3,
                   fit3,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC') +
  labs(title = paste0("In ", unique(data_3$COAName) ," | registered from 2014"),
       y = "Refugee duration probability",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency")

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_3$RegistrationDate[which(year(data_3$RegistrationDate)=="2014")])), units = "weeks")/52.25, digits=2)

plot_south_sudanese_in_kenya <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
#  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.85, label=paste(round(fit3$surv[which(fit3$time==5)]*100), "%"),
           color="black")
# geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_3$RegistrationDate[which(year(data_3$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
# annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit3$surv[which(fit3$time==end_tp)]*100), "%"),
#          color="black")

ggsave(file="Plots/plot_south_sudanese_in_kenya.svg", plot=plot_south_sudanese_in_kenya, width=10, height=8)
save(plot_south_sudanese_in_kenya, file="Plots/plot_south_sudanese_in_kenya.RData")

data_4 <- south_sudanese_in_ethiopia[year(south_sudanese_in_ethiopia$RegistrationDate) >="2013",]

fit4 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_4)

# Visualize with survminer
plot <- ggsurvplot(data = data_4,
                   fit4,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC',
                   risk.table=TRUE) +
  labs(title = paste0("In ", unique(data_4$COAName) ," | registered from 2014"),
       y = "Refugee duration probability")

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_4$RegistrationDate[which(year(data_4$RegistrationDate)=="2014")])), units = "weeks")/52.25, digits=2)

plot_south_sudanese_in_ethiopia <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
#  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.65, label=paste(round(fit4$surv[which(fit4$time==5)]*100), "%"),
           color="black")
# geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_4$RegistrationDate[which(year(data_4$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
# annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit4$surv[which(fit4$time==end_tp)]*100), "%"),
#          color="black")

ggsave(file="Plots/plot_south_sudanese_in_ethiopia.svg", plot=plot_south_sudanese_in_ethiopia, width=10, height=8)
save(plot_south_sudanese_in_ethiopia, file="Plots/plot_south_sudanese_in_ethiopia.RData")

data_3 <- south_sudanese_in_sudan[year(south_sudanese_in_sudan$RegistrationDate) >="2015",]

fit3 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_3)

# Visualize with survminer
plot <- ggsurvplot(data = data_3,
                   fit3,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC',
                   risk.table=TRUE) +
  labs(title = paste0("In ", unique(data_3$COAName) ," | registered from 2015"),
       y = "Refugee duration probability") 

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_3$RegistrationDate[which(year(data_3$RegistrationDate)=="2015")])), units = "weeks")/52.25, digits=2)

plot_south_sudanese_in_sudan <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
#  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.87, label=paste(round(fit3$surv[which(fit3$time==5)]*100), "%"),
           color="black")
# geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_4$RegistrationDate[which(year(data_4$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
# annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit4$surv[which(fit4$time==end_tp)]*100), "%"),
#          color="black")

ggsave(file="Plots/plot_south_sudanese_in_sudan.svg", plot=plot_south_sudanese_in_sudan, width=10, height=8)
save(plot_south_sudanese_in_sudan, file="Plots/plot_south_sudanese_in_sudan.RData")
######################################################################################
######################################################################################
#
#   CAR IN CAMEROON - READ FORMATTED DATA AND PLOT BARCHART OF REGISTERED CASES AND KM PLOTS
#
######################################################################################
######################################################################################
######################################################################################

load("Formatted Data/sudanese_in_chad.RData")
sudanese_in_chad <- dataset1

options(scipen = 999)

reasons_sudanese_in_chad <- filter(sudanese_in_chad, ProcessStatusName %in% c("Inactive", "Closed")) %>%
  group_by(ProcessDetail) %>%
  summarise(n = n())

######################################################################################
#       Bar chart of registered cases by year since 2011
######################################################################################
registered_sudanese_in_chad_year <- ggplot(data = filter(sudanese_in_chad, registration_year>2000),
                                  aes(x=registration_year),
                                  fill = unhcr_pal(n = 3, "pal_blue")) +
  geom_bar( stat="count") +
  ggtitle(" Number of newly registered Syrian cases by year ") +
  xlab("Registration Year") +
  ylab("Registered individuals") +
  facet_wrap(~COAName) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                     expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks= pretty_breaks(10))  +
  theme_unhcr(grid = "Y", axis_title = "y") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        panel.spacing = unit(1, "lines"))

save(registered_sudanese_in_chad_year, file="Plots/registered_sudanese_in_chad_year.RData")

######################################################################################
#       KM plots for registered cases from Sudanese in Chad in 2005
######################################################################################

data_2 <- sudanese_in_chad[year(sudanese_in_chad$RegistrationDate) =="2005",]

fit2 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_2)

# Visualize with survminer
plot <- ggsurvplot(data = data_2,
                   fit2,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC') +
  labs(title = paste0("Sudanese in ", unique(data_2$COAName) ," | registered in 2005"),
       y = "Refugee duration probability",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency")

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_2$RegistrationDate[which(year(data_2$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)

plot_sudanese_in_chad <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.75, label=paste(round(fit2$surv[which(fit2$time==5)]*100), "%"),
           color="black")
# geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_2$RegistrationDate[which(year(data_2$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
# annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit2$surv[which(fit2$time==end_tp)]*100), "%"),
#          color="black")

ggsave(file="Plots/plot_sudanese_in_chad.svg", plot=plot_sudanese_in_chad, width=10, height=8)
save(plot_sudanese_in_chad, file="Plots/plot_sudanese_in_chad.RData")

######################################################################################
######################################################################################
#
#   Burundians IN Tanzania - READ FORMATTED DATA AND PLOT BARCHART OF REGISTERED CASES AND KM PLOTS
#
######################################################################################
######################################################################################
######################################################################################

load("Formatted Data/burundis_in_tanzania.RData")
burundis_in_tanzania <- dataset1

options(scipen = 999)

reasons_burundis_in_tanzania <- filter(burundis_in_tanzania, ProcessStatusName %in% c("Inactive", "Closed")) %>%
  group_by(ProcessDetail, registration_year) %>%
  summarise(n = n())

######################################################################################
#       KM plots for registered cases from Sudanese in Chad in 2005
######################################################################################

data_2 <- burundis_in_tanzania[year(burundis_in_tanzania$RegistrationDate) >="1996" & year(burundis_in_tanzania$RegistrationDate) <="1997",]

fit2 <- survfit(Surv(duration_registration, event) ~ 1,
                data = data_2)

# Visualize with survminer
plot <- ggsurvplot(data = data_2,
                   fit2,
                   legend="none",
                   ggtheme = theme_unhcr(grid = "Y", axis_title = "y"),
                   palette= '#0072BC') +
  labs(title = paste0("Burundis in ", unique(data_2$COAName) ," | registered between 1996 and 1997"),
       y = "Refugee duration probability",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency")

end_tp <- round(difftime(as.Date(Sys.Date()), as.Date(min(data_2$RegistrationDate[which(year(data_2$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)

plot_burundis_in_tanzania <- plot$plot +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_vline(xintercept = 5) +
  geom_vline(xintercept = end_tp) +
  annotate(geom="text", x=6, y=0.75, label=paste(round(fit2$surv[which(fit2$time==5)]*100), "%"),
           color="black")
# geom_vline(xintercept = round(difftime(as.Date(Sys.Date()), as.Date(min(data_2$RegistrationDate[which(year(data_2$RegistrationDate)=="2013")])), units = "weeks")/52.25, digits=2)) +
# annotate(geom="text", x = end_tp+0.3, y=0.5, label=paste(round(fit2$surv[which(fit2$time==end_tp)]*100), "%"),
#          color="black")

ggsave(file="Plots/plot_burundis_in_tanzania.svg", plot=plot_burundis_in_tanzania, width=10, height=8)
save(plot_burundis_in_tanzania, file="Plots/plot_burundis_in_tanzania.RData")
