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


##Facet graph

b <- single_lion
view(b)
str(b)

at = b %>% select(date,total_cases,fatal,injury,age_1,age_2,age_3,age_4,age_5,age_6,age_7,age_8,age_9,age_10)
view(at)
skimr::skim(at)
variable.names(at)


#Expanding the frequency
at <- data.frame(at)

data <-  at %>% 
  select(date,total_cases,fatal,age_1,age_2,age_3,age_4,age_5,age_6,age_7,age_8,age_9,age_10)
view(data)
data = data %>% pivot_longer(cols = starts_with("age"),names_to = "age_1",values_to ="age",
                             names_prefix = "age",
                             names_ptypes = list(age = integer()),
                             values_drop_na = T)

view(data)
#write.csv(data,'D:\\facet_total_cases.csv')
g <- facet_total_cases %>% 
  select(age,date,fatal,total_cases)
g = g %>% na.omit(b,cols = c("age"))
view(g)
##Import the age data
#skimr::skim(tp)

## Facet of Number of Fatals
tp = g %>% 
  select(fatal,age,date,total_cases)
tp %>% 
  mutate(Date = floor_date(Date, unit = "year")) %>%
  apply(c, 2, function(x) any(is.na(x)))

ttr <- tp %>% 
  mutate(age_group = dplyr::case_when(
      age < 5                ~ "0-5",
      age >= 5   & age <= 9  ~ "5-9",
      age >= 10  & age <= 15 ~ "10-15",
      age >= 16  & age <= 20 ~ "16-20",
      age >= 21  & age <= 24 ~ "21-24",
      age >= 25  & age <= 34 ~ "25-34",
      age >= 35  & age <= 44 ~ "35-44",
      age >= 45  & age <= 54 ~ "45-54",
      age >= 55  & age <= 64 ~ "55-64",
      age >= 65  & age <= 74 ~ "65-74",
      age >= 75  & age <= 84 ~ "75-84",
      age > 84               ~ ">84"  
      ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-5","5-9","10-15","16-20","21-24","25-34","35-44","45-54","55-64","65-74","75-84",">84")
    )
  ) 


view(ttr)
ttr = ttr %>% na.omit(b,cols = c("age"))

ag <- ttr %>% count(age_group,fatal,date)
ag
sum(ag$n)

ggplot(data = ag, aes(x = date , y = n, group = age_group, colour = age_group)) +
  geom_line() +facet_wrap(~ age_group)+theme_minimal()+ 
  labs(x = "Year", y = "Number of Accidents",title = "Fatalities by Age (2010 - 2020) ")+
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))


##facet of total cases

ft <- ttr %>% count(age_group,total_cases,date)
ft
sum(ft$n)
view(ft)
ggplot(data = ft, aes(x = date , y = total_cases, group = age_group, colour = age_group)) +
  geom_line() +facet_wrap(~ age_group)+theme_minimal()+ 
  labs(x = "Year", y = "Number of Accidents",title = "Accidents by Age (2010 - 2020) ")+
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))

##Bar chart number of fatalities by age

ft = na.omit(ag, cols = c ("age_group"))
ggplot(ft, aes(x= age_group, y=n ,color = age_group)) + 
  geom_bar(stat = "identity", width=0.8, alpha = 0.4) +
  theme_minimal() + labs(x = "Age", y = "Number of Fatalities",title = "Accidents by Age (2010 - 2020) ") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 14, family = "Open Sans"))


ggplot(aggregate(n~age_group,ft,"sum")) + 
  geom_bar(aes(x = as.factor(age_group), y = n,color = age_group), stat="identity")+ theme_minimal()+
  labs(x = "Age", y = "Number of Fatalities",title = "Accidents by Age (2010 - 2020) ") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 14, family = "Open Sans"))


