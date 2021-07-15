# Project

library(psych)
library(tidyverse)
library(ggplot2)
library(RSocrata)
library(readxl)
library(lubridate)


## Number of traffic Accidents per month

a <- excel_sheets("D:/thoppurproject/Road.xlsx") %>% map_df(~read_xlsx("D:/thoppurproject/Road.xlsx",.))
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








