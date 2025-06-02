library(tidyverse)
library(plotly)

# 1.Load data and select key columns  -------------------------------------

protest-data <- read_csv("https://raw.githubusercontent.com/ursulinakoelbener/lumacss-data-journalism-climate/refs/heads/main/data/Enriched%20Swiss%20protest%20data.csv")

key_data <- protest_data %>% 
  select(event_date,year,sub_event_type,assoc_actor_1,actor2,country:longitude,
         notes,fatalities, tags, population_best) %>% 
  mutate(event_date=as.Date(event_date, format = "%d %B %Y")) %>% 
  filter(year>=2020)
# Swiss data only availabel for 2020 onwards

#Focus on Switzerland
switzerland <- key_data %>% 
  filter(country=="Switzerland")

# 2. Add protest type using keywords in notes field -----------------------

#Define keywords for each theme
israeli_keywords <- str_c(c("jewish","hostages","unrwa"),collapse = "|")
palestinian_keywords <- str_c(c("palestine","palestinian","israel","gaza","pro-palestine"),collapse = "|")
lgbtq_keywords <- str_c("\\b",c("lgbtq+","lesbian","trans","homophobia","lgbt+","transphobia","transphobic",
                                "lgbtqia+","homosexuals"),"\\b",collapse = "|")
climate_keywords <- str_c(c("climate","greenpeace","critical mass","fossil fuels","biodiversity","co2 emissions","energy renovation",
                            "environmental","fracking","extinction rebellion","global warming","save the planet","oil","trees"),collapse = "|")
feminism_keywords <- str_c("\\b",c("feminist","femicide","gender-based violence","rape","harassment",
                                   "sexism","patriarchy","femicides","sexist","greve feministe","feminicides",
                                   "sexual abuse","rapist's"),"\\b",collapse = "|")
ukraine_keywords <- str_c(c("ukraine","ukrainian"),collapse = "|")
farmer_keywords <- str_c(c("farmer","farming","uniterre"),collapse = "|")
asylum_keywords <- str_c(c("asylum","migswissrant","refugee","frontex"),collapse = "|")
police_keywords <- str_c(c("police custody","police violence","police shooting","black lives matter",
                           "police brutality","puplinge","police repression","brenaz","suspension of 6 police officers",
                           "black man"),collapse = "|")
corona_keywords <- str_c(c("corona","coronavirus","mass-voll","pandemic"),collapse = "|")
christian_keywords <- str_c(c("christian","anti-abortion","right to life","catholic"),collapse = "|")
workers_keywords <- str_c("\\b",c("better working conditions","higher wages","union","labor agreement",
                                  "strike","striking","unia","syna","uss","pay rise","salary","salaries",
                                  "wage","wages","better work conditions","uber","labor rights","better pay",
                                  "job cuts","famco","reform of consultation times and fees",
                                  "taxi drivers"),"\\b",collapse = "|")
serbia_keywords <- str_c(c("serbia","serbian"),collapse = "|")
international_keywords <- str_c(c("iran","afghanistan","algeria","kurd","kurdish",
                                  "sudan","congolese","peru","uganda","thailand",
                                  "tibet","cuba","yezidi","uyghur","taliban","future russia","turkish",
                                  "sahrawi","cameroonian","armenian","lebanese","trump","tesla","eritrean","navalny"),collapse = "|")
animal_keywords <- str_c("\\b",c("fur","antispecists","anti-speciesist","vivisection","wolves"),"\\b",collapse = "|")
farright_keywords <- str_c("\\b",c("junge tat","drag queen"),"\\b",collapse = "|")
healthInsurance_keywords <- str_c("\\b",c("health insurance","swica","high costs of healthcare services"),"\\b",collapse = "|")
cycling_keywords <- str_c("\\b",c("bike","cycle","bikes"),"\\b",collapse = "|")
housing_keywords <- str_c("\\b",c("real estate speculation","homeless","housing"),"\\b",collapse = "|")

#Add themes to dataframe 
swiss_themes <- switzerland %>% 
  mutate(notes=tolower(notes)) %>% 
  mutate(Topic=case_when(str_detect(notes,feminism_keywords)~"Feminist",
                         str_detect(notes,israeli_keywords)~"Israel",
                         str_detect(notes,palestinian_keywords)~"Palestine",
                         str_detect(notes,christian_keywords)~"Christian",
                         str_detect(notes,lgbtq_keywords)~"LGBTQ+",
                         str_detect(notes,climate_keywords)~"Climate",
                         str_detect(notes,ukraine_keywords)~"Ukraine",
                         str_detect(notes,farmer_keywords)~"Farming",
                         str_detect(notes,asylum_keywords)~"Migrants",
                         str_detect(notes,police_keywords)~"Police",
                         str_detect(notes,workers_keywords)~"Workers",
                         str_detect(notes,corona_keywords)~"Corona",
                         str_detect(notes,serbia_keywords)~"Serbian",
                         str_detect(notes,international_keywords)~"International",
                         str_detect(notes,animal_keywords)~"Animal",
                         str_detect(notes,farright_keywords)~"FarRight",
                         str_detect(notes,healthInsurance_keywords)~"HealthInsurance",
                         str_detect(notes,cycling_keywords)~"Cycling",
                         str_detect(notes,housing_keywords)~"Housing",
                         TRUE~"Other"))


# Clean data on number of protesters ------------------------------

protester_number <- swiss_themes %>% 
  mutate(tags=if_else(tags=="crowd size=no report",NA,tags)) %>% 
  mutate(tags=str_replace_all(tags,",","")) %>% 
  mutate(tags=str_replace_all(tags,"hundreds","500")) %>% 
  mutate(tags=str_replace_all(tags,"several hundred","300")) %>% 
  mutate(tags=str_replace_all(tags,"several hundreds","300")) %>% 
  mutate(tags=str_replace_all(tags,"a few hundred","300")) %>% 
  mutate(tags=str_replace_all(tags,"thousands","5000")) %>% 
  mutate(tags=str_replace_all(tags,"several thousand","3000")) %>% 
  mutate(tags=str_replace_all(tags,"several","7")) %>% 
  mutate(tags=str_replace_all(tags,"a handful","7")) %>% 
  mutate(tags=str_replace_all(tags,"dozens","74")) %>% 
  mutate(tags=str_replace_all(tags,"several dozen","48")) %>% 
  mutate(tags=str_replace_all(tags,"a dozen","12")) %>% 
  mutate(tags=str_replace_all(tags,"around a dozen","12")) %>% 
  mutate(tags=str_replace_all(tags,"thousands or tens of thousands","10000")) %>% 
  mutate(tags=str_replace_all(tags,"a good thousand","1000")) %>% 
  mutate(tags=str_replace_all(tags,"tens of thousand","30000")) %>% 
  mutate(tags=str_replace_all(tags,"fifty","50")) %>% 
  mutate(tags=str_replace_all(tags,"six","6")) %>% 
  mutate(tags=str_replace_all(tags,"seven","7")) %>% 
  mutate(tags=str_replace_all(tags,"eight","8")) %>% 
  mutate(all_numbers=str_extract_all(tags, "\\d+")) %>% 
  rowwise() %>% 
  # Extract first and last numbers of estimates
  mutate(first_number=if (length(all_numbers)>0) all_numbers[[1]] else NA) %>% 
  mutate(last_number=if (length(all_numbers)>1) all_numbers[[2]] else NA) %>% 
  mutate(first_number=as.numeric(first_number)) %>% 
  mutate(last_number=as.numeric(last_number)) %>% 
  # Average upper and lower numbers to get average
  mutate(final_estimate=case_when(is.na(last_number)~first_number,
                                  !(is.na(last_number)&is.na(last_number))~(first_number+last_number)/2)) %>% 
  #Some rows contain protesters across several locations, in this case divide protesters by locations
  mutate(Location=if_else(str_detect(tags,"locations"),"Location",NA)) %>% 
  mutate(final_estimate=case_when(Location=="Location"~ (first_number/last_number),
                                  TRUE~final_estimate)) %>% 
  select(-first_number,-last_number,-all_numbers,-Location) %>% 
  rename(crowdSize=final_estimate)

# Check how much data on crowd size is missing
missing_values <- protester_number %>% 
  filter(is.na(final_estimate)) %>% 
  nrow()

total_size <- nrow(protester_number)

missing_fraction <- missing_values/total_size
#There are 229 mssing values in crowd size estimates (about 33% of data)

write_csv(protester_number,"Enriched Swiss protest data.csv")

