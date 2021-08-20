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

t = d %>% select(total_cases,head,date)
view(t)


##Accidents by days of the week (Injury and Fatal)

tt %>%
  mutate(date = wday(date, label = TRUE)) %>%
  mutate(head = fct_collapse(head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),
                             Injury = c("Simple","simple","Grevious","Grives","RA"))) %>%
  
  count(date, head) %>%
  group_by(head) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(percent, head, fill = date )) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "% of Cases", y = NULL, fill = "Cases") + ggtitle("Accident by days of the week") +
  
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))


##Number of traffic accidents per month
at = d %>% select(total_cases,date,fatal,injury)
view(at)
skimr::skim(at)
variable.names(at)


#Expanding the frequency
at <- data.frame(at)

data <-  at

work_data <- data[rep(1:nrow(data), data[["total_cases"]]), ] 
work_data <- data[rep(1:nrow(data), data[["fatal"]]), ] 
work_data <- data[rep(1:nrow(data), data[["injury"]]), ]
view(work_data)

#Assigning value 1 
work_data$total_cases[work_data$total_cases>0] <- 1
work_data$fatal[work_data$fatal>0]<-1
work_data$injury[work_data$injury>0]<-1

work_data = work_data %>%
  select(date,fatal,injury)
variable.names(work_data)


view(work_data)
work_data = work_data %>%  pivot_longer(-date, 
                              names_to = "cases", 
                              values_to = "injury")
write.csv(work_data,'D:\\pivot.csv')

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
m <- pivot
view(m)

wd <- m
view(wd)

rownames(wd) <- NULL
wd <- wd %>%select(date,cases)

variable.names(wd)
dim(wd)
# load fonts - every session
loadfonts(device = "win", quiet = TRUE)
str(work_data)
wd %>%
  mutate(date = floor_date(date, unit = "year")) %>% 
  count(date,cases) %>% 
  filter(
    date != last(date),
    date != first(date)
  )  %>%
  mutate(percent= n / sum(n)) %>%
  ungroup() %>% 
  ggplot(aes(date,percent)) +
  geom_line(size = 1.5, alpha = 0.7, color= "red" ) +
  geom_point() + 
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = "Year",
    y = "Percentage of Fatals and Injuries",
    title = "Number of people involved in the accident over the past 10 years",
  ) + theme_minimal()   + theme(legend.position = "none") + 
  geom_point(shape = 21,colour = "#0081C8", fill = "#00A651", size = 4)





