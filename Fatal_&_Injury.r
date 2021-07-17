##Accident Fatal and Injuries 
tpp<-tp%>%
  mutate(Date = wday(Date, label = TRUE)) %>%
  mutate(Head = fct_collapse(Head, Fatal = c("Fatal","fatal","Fatal.","Fatals"),Injury = c("Simple","simple","Grevious","Grives","RA"))) %>%
  count(Head)
ggplot(tpp, aes(x= Head, y=n )) + 
  geom_bar(stat = "identity", width=0.2, alpha = 0.4, fill="red") +
  theme_minimal() + labs(x = "Cases", y = "Number of Accidents") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))
