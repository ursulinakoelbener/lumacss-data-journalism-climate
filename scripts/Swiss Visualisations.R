library(tidyverse)
library(plotly)

swiss_data <- read_csv("Enriched Swiss protest data.csv")

# 1. Cantons with the most protests ---------------------------------------

swiss_cities <- swiss_data %>% 
  group_by(admin1) %>% 
  summarise(Protests=n()) %>% 
  ungroup()

# 2. Identify most common protest topics by region ------------------------

theme_df <- swiss_data %>% 
  select(Topic,event_date) %>% 
  group_by(Topic) %>% 
  summarise(Protests=n()) %>% 
  ungroup()

protest_type_plot <- plot_ly(data=theme_df,
                             x=~Protests,
                             y=~fct_reorder(Topic, Protests),
                             type="bar",
                             orientation="h") %>% 
                              layout(title = "Most common protest topics (2020-2025)",
                                yaxis = list(title = ""),
                                xaxis = list(title = "Number of Protests"))

regional_df <- swiss_data %>% 
  group_by(admin1,Topic) %>% 
  summarise(Protests=n()) %>% 
  ungroup() %>% 
  group_by(admin1) %>% 
  mutate(ProtestsCanton=sum(Protests)) %>% 
  ungroup() %>% 
  mutate(Percentage=round((Protests/ProtestsCanton)*100,2))

regional_plot <- plot_ly(data=regional_df,
                         x=~Percentage,
                         y=~fct_reorder(admin1 ,ProtestsCanton),
                         color=~Topic,
                         type="bar",
                         orientation="h") %>% 
  layout(barmode="stack",
         title = "Distribution of protest topic by canton (2020-2025)",
         yaxis = list(title = ""),
         xaxis = list(title = "Percentage of Canton Protests (%)"))


# 3. Identify which cantons and topics have highest number of prot --------

cantons_protesters <- swiss_data %>% 
  select(admin1, final_estimate,Topic) %>% 
  drop_na() %>% 
  group_by(admin1,Topic) %>% 
  summarise(totalProtesters=sum(final_estimate)) %>% 
  ungroup() %>% 
  group_by(admin1) %>% 
  mutate(cantonProtesters=sum(totalProtesters)) %>% 
  ungroup()

protester_plot <- plot_ly(data=cantons_protesters,
                          x=~totalProtesters,
                          y=~fct_reorder(admin1 ,cantonProtesters),
                          color=~Topic,
                          type="bar",
                          orientation="h") %>% 
  layout(barmode="stack",
         title="Number of protesters by topic and canton",
         yaxis = list(title = ""),
         xaxis = list(title = "Number of protesters"))

# 4. Investigate distribution of protests over time -----------------------

protests_time <- swiss_data%>% 
  group_by(event_date,Topic) %>% 
  summarise(Protests=n()) %>% 
  ungroup()

protest_time_plot <- plot_ly(data=protests_time,
                             x=~event_date,
                             y=~Protests,
                             color=~Topic,
                             type="scatter",
                             mode="lines+markers") %>% 
  layout(title = "Distribution of Protests (2020-2025)",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Number of Protests"))
