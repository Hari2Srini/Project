library(psych)
library(tidyverse)
library(ggplot2)
library(RSocrata)
library(readxl)
library(dplyr)
library(lubridate)
library(kableExtra)
library(extrafont)
library(lemon)
library(ggstatsplot)
loadfonts(device = "win")


d <- single_lion
view(d)

skimr::skim(d)                                                             

##Number of traffic accidents per month
at = d %>% select(date,fatal,injury)
view(at)
skimr::skim(at)
variable.names(at)


#Expanding the frequency
at <- data.frame(at)

data <-  at

#work_data <- data[rep(1:nrow(data), data[["total_cases"]]), ] 
work_data <- data[rep(1:nrow(data), data[["fatal"]]), ] 
work_data <- data[rep(1:nrow(data), data[["injury"]]), ]
view(work_data)

#Assigning value 1 
#work_data$total_cases[work_data$total_cases>0] <- 1
work_data$fatal[work_data$fatal>0]<-1
work_data$injury[work_data$injury>0]<-1

work_data = work_data %>%
  select(date,fatal,injury)
variable.names(work_data)


view(work_data)
work_data = work_data %>%  pivot_longer(-date, 
                                        names_to = "cases", 
                                        values_to = "injury")
#write.csv(work_data,'D:\\pivot.csv')

library("dplyr")
##---------------------

library(extrafont)
library(ggplot2)
library(viridis)

library(hrbrthemes)

loadfonts(device = "win")
variable.names(work_data)

#write.csv(work_data,'E:\\time.csv')
##Import the dataset 

##Load and convert through r 
m <- pivoted_data_fullycleaned
view(m)

wd <- m
view(wd)

rownames(wd) <- NULL
wd <- wd %>%select(date,cases)

variable.names(wd)
dim(wd)

##Trend Analysis male & female injuries and fatals

wd=data.frame(wd)
wd %>%
  mutate(date = floor_date(date, unit = "year")) %>%
  count(cases,date) %>%
  mutate(percent=n/sum(n)) %>% 
  ggplot(aes(date,percent, color = cases)) + geom_line(size = 1.5, alpha = 0.8) +
  geom_point() + 
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = "Year",
    y = "Percentage of Fatals and Injuries",
    title = "Number of people involved in the accident over the past 10 years",) + 
  theme_minimal()   +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12)) + 
  geom_point(shape = 19,colour = "#0081C8", fill = "#00A651", size = 2)





##Trend Analysis of accidents
wd=data.frame(wd)
wd %>%
  mutate(date = floor_date(date, unit = "year")) %>%
  mutate(cases = fct_collapse(cases,fatal = c("male_fatal","female_fatal"),
                                    injury = c("male_injury","female_injury"))) %>% count(cases,date) %>%
  mutate(percent=n/sum(n)) %>% 
  ggplot(aes(date,percent, color = cases)) + geom_line(size = 1.5, alpha = 0.8) +
  geom_point() + 
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = "Year",
    y = "Percentage of Fatals and Injuries",
    title = "Number of people involved in the accident over the past 10 years",) + 
  theme_minimal()   +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12)) + 
  geom_point(shape = 19,colour = "#0081C8", fill = "#00A651", size = 2)



dim(wd)
variable.names(wd)
head(wd)












