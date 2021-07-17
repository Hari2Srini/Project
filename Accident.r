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
loadfonts(device = "win")


## Number of traffic Accidents per month

a <- excel_sheets("D:/thoppurproject/acci.xlsx") %>% map_df(~read_xlsx("D:/thoppurproject/acci.xlsx",.))
view(a)

skimr::skim(a)

tp = a %>% select(Head,Date)
view(tp)
skimr::skim(tp)



tp %>%
  mutate(Date = floor_date(Date, unit = "month")) %>%
  mutate(Head = fct_collapse(Head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),
                              
                              Injury = c("Simple","simple","Grevious","Grives","RA"))) %>% count(Date,Head) %>% 
  filter(
    Date != last(Date),
    Date != first(Date)
  )  %>%
    ggplot(aes(Date,n, color = Head)) + geom_line(size = 1.5, alpha = 0.7) + scale_y_continuous(limits = (c(0, NA))) +
    labs(
      x = NULL, y = "Number of Traffic Accidents per Month",
      color = "CASES"
    ) + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))
  
  
## Number of Fatals


tp %>%
  mutate(Date = floor_date(Date, unit = "month")) %>%
  mutate(Head = fct_collapse(Head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),
                             
                             Injury = c("Simple","simple","Grevious","Grives","RA"))) %>% 
  count(Date,Head) %>% 
  filter(
    Date != last(Date),
    Date != first(Date)
  )  %>%
  group_by(Date) %>% 
  mutate(percent_injury = n / sum(n)) %>%
  ungroup() %>%
  filter(Head == "Fatal") %>%
  ggplot(aes(Date, percent_injury)) + geom_line(size = 1.5, alpha = 0.7, color = "Red") +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent_format()) +
  labs(x = NULL, y = "% of Accidents that Involve Death") + 
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))

##Number of Injuries


tp %>%
  mutate(Date = floor_date(Date, unit = "month")) %>%
  mutate(Head = fct_collapse(Head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),
                             
                             Injury = c("Simple","simple","Grevious","Grives","RA"))) %>% 
  count(Date,Head) %>% 
  filter(
    Date != last(Date),
    Date != first(Date)
  )  %>%
  group_by(Date) %>% 
  mutate(percent_injury = n / sum(n)) %>%
  ungroup() %>%
  filter(Head == "Injury") %>%
  ggplot(aes(Date, percent_injury)) + geom_line(size = 1.5, alpha = 0.7, color = "Midnightblue") +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent_format()) +
  labs(x = NULL, y = "% of Accidents that Involve Injury") + 
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))
  
  
##Accident Fatal and Injuries 
tpp<-tp%>%
  mutate(Date = wday(Date, label = TRUE)) %>%
  mutate(Head = fct_collapse(Head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),Injury = c("Simple","simple","Grevious","Grives","RA"))) %>%
  count(Head)
ggplot(tpp, aes(x= Head, y=n )) + 
  geom_bar(stat = "identity", width=0.2, alpha = 0.4, fill="red") +
  theme_minimal() + labs(x = "Cases", y = "Number of Accidents") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))




##How does the cases rate change through the week?

tp %>%
  mutate(Date = wday(Date, label = TRUE)) %>%
  mutate(Head = fct_collapse(Head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),
                             Injury = c("Simple","simple","Grevious","Grives","RA"))) %>%
  
  count(Date, Head) %>%
  group_by(Head) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(percent, Date, fill = Head )) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "% of Cases", y = NULL, fill = "Cases") + ggtitle("Case Rate") +
  
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))


##Accident Rate through Week
tp %>%
  mutate(Date = wday(Date, label = TRUE)) %>%
  mutate(Head = fct_collapse(Head,Fatal = c("Fatal","fatal","Fatal.","Fatals"))) %>% 
  count(Date,Head) %>% filter(Head == "Fatal") %>% group_by(Head) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  kable(
    col.names = c("Day", "Cases", "Number of people", "percentage"),
    align = "llrr"
  )
  
##Male & Female age group

###load acci excel sheet

t = acci %>% select(Head,Age,Date,Gender)
view(t)

tt <- t %>% 
  filter(!is.na(Age),!is.na(Gender))

tt %>%
  mutate(Date = floor_date(Date, unit = "month")) %>%
  mutate(Head = fct_collapse(Head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),
                             Injury = c("Simple","simple","Grevious","Grives","RA")))


ttr <- tt %>% 
  mutate(
    age_group = dplyr::case_when(
      Age <= 18            ~ "0-18",
      Age >= 19 & Age <= 40 ~ "19-40",
      Age > 40 & Age <= 62 ~ "40-62",
      Age > 62             ~ "> 62"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-18","19-40","40-62", "> 62")
    )
  ) 



ag <- ttr %>%  
  filter(Gender != "N")  %>% 
  count(age_group,Gender)



g = na.omit(ag, cols = c("Gender", "age_group"))

ggplot(data = g,mapping = aes(x = ifelse(test = Gender == "M", yes = -n, no = n),
                               y = age_group, fill = Gender,
                               label=paste(round(n*100/sum(n), 0), "%", sep=""))) +
  geom_col() +
  geom_text(hjust=ifelse(test = g$Gender == "M",  yes = 1.1, no = -0.1), size= 3, colour="#505050") + 
  labs(x = "Population") +
  # Remove the axis labels and the fill label from the legend - these are unnecessary for a Population Pyramid
  labs(
    x = "",
    y = "",
    fill=""
  ) + theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)) +
    
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 14, family = "Open Sans"))




###Bar chart Age and head

tt %>%
  mutate(Date = floor_date(Date, unit = "month")) %>%
  mutate(Head = fct_collapse(Head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),
                             Injury = c("Simple","simple","Grevious","Grives","RA")))


ttr <- tt %>% 
  mutate(
    age_group = dplyr::case_when(
      Age < 5                ~ "0-5",
      Age >= 5   & Age <= 9  ~ "5-9",
      Age >= 10  & Age <= 15 ~ "10-15",
      Age >= 16  & Age <= 20 ~ "16-20",
      Age >= 21  & Age <= 24 ~ "21-24",
      Age >= 25  & Age <= 34 ~ "25-34",
      Age >= 35  & Age <= 44 ~ "35-44",
      Age >= 45  & Age <= 54 ~ "45-54",
      Age >= 55  & Age <= 74 ~ "55-74",
      Age > 74               ~ " > 74"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-5","5-9","10-15","16-20","21-24","25-34","35-44","45-54","55-74"," > 74")
    )
  ) 
ag <- ttr %>%  
  filter(Head == "Fatal")  %>% 
  count(age_group,Head)
ag
g = na.omit(ag, cols = c ("age_group"))
ggplot(g, aes(x= age_group, y=n )) + 
  geom_bar(stat = "identity", width=0.7, alpha = 0.4) +
  theme_minimal() + labs(x = "Age", y = "Number of Fatalities") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 10, family = "Open Sans"))


## Accident & Gender
library(DataExplorer)
l = t %>%  na.omit(t,cols = c("Gender"))

plot_bar(l, ggtheme = theme_minimal(base_size = 20))



## comparing categorical using chi square
l$Head=factor(l$Head)
l$Gender=factor(l$Gender)
str(l)
ggbarstats(data = l, x = Head, y = Gender, label = "both")
library(SmartEDA)
library(ISLR)


## Box Plot

plot4 <- ExpNumViz(l,target="Head",col=c("darkgreen","springgreen3","springgreen1"))

plot4[[1]]












