### Supplemental Materials (Data and scripts)
###  Distance Learning Solutions with COVID-19 Pandemic

## ---- load
library(tidyverse)
library (gtrendsR)
library(RColorBrewer)
library(ggpubr)
library(viridis)
library(fpp3)
library(countrycode)
library(coronavirus)
library(patchwork)
#set the time window
# Consider 2020 
time <- ("2019-12-01 2021-08-15")

## ---- covidImpactWorld
## COVID-19 Impact on Education (global data)
## Data source: https://en.unesco.org/covid19/educationresponse
## Data download: 16-08-2021
## Write (download from the above link)
world_impact <- read.csv(
  here::here("data", "covid_impact_education_full.csv"))
wimpact <- world_impact %>%
  mutate(Date = as.Date(Date, "%d/%m/%Y"),
         Status = as.factor(Status)) 

wimpact <- wimpact %>% select(-ISO) %>%
  group_by(Date, Status) %>%
  summarize(Count = n())%>%
  as_tsibble(index = Date, key = Status) 

p <- wimpact %>% autoplot(Count) +
  geom_line(size=1) +
  xlab ("Time") +
  # viridis::scale_colour_viridis(discrete = TRUE)+
  scale_color_brewer(palette="Dark2")+
  ylab("Number of Countries")+
  theme_bw() + 
  theme( text = element_text(size = 14), legend.position = "bottom")+
  guides(colour = guide_legend(nrow = 2, title="Status"))

print(p)


## ---- covidImpactContinent
## COVID-19 Impact on Education - continent wise 
## Data source: https://en.unesco.org/covid19/educationresponse
## Data download: 16-08-2021
world_impact <- read.csv(
  here::here("data", "covid_impact_education_full.csv"))
wimpact <- world_impact %>%
  mutate(Date = as.Date(Date, "%d/%m/%Y"),
         Status = as.factor(Status)) 

wimpact$Continent <- countrycode(sourcevar = wimpact[, "ISO"],
                                 origin = "iso3c",
                                 destination = "continent")

wimpact <- wimpact %>% select(-ISO) %>%
  group_by(Date, Status, Continent) %>%
  summarize(Count = n())

wimpact1<- wimpact%>%pivot_wider(names_from = Continent, values_from = Count )

wimpact1$Asia <- (wimpact1$Asia - min(wimpact1$Asia, na.rm = TRUE))/(max(wimpact1$Asia, na.rm = TRUE) - min(wimpact1$Asia, na.rm = TRUE))

wimpact1$Africa <- (wimpact1$Africa - min(wimpact1$Africa, na.rm = TRUE))/(max(wimpact1$Africa, na.rm = TRUE) - min(wimpact1$Africa, na.rm = TRUE))

wimpact1$Africa <- (wimpact1$Americas - min(wimpact1$Americas, na.rm = TRUE))/(max(wimpact1$Americas, na.rm = TRUE) - min(wimpact1$Americas, na.rm = TRUE))

wimpact1$Europe <- (wimpact1$Europe - min(wimpact1$Europe, na.rm = TRUE))/(max(wimpact1$Europe, na.rm = TRUE) - min(wimpact1$Europe, na.rm = TRUE))

wimpact1$Oceania <- (wimpact1$Oceania - min(wimpact1$Oceania, na.rm = TRUE))/(max(wimpact1$Oceania, na.rm = TRUE) - min(wimpact1$Oceania, na.rm = TRUE))


wimpact2<- wimpact1 %>% pivot_longer(
  cols = Asia:Oceania,
  names_to = c("Continent"),
  values_to = "count"
)

p <- wimpact %>%
  ggplot(aes(x = Date, y = Count, color = Status)) +
  geom_line(size = 1) +
  facet_grid(vars(Continent), scales = "free_y") +   # only assigns the row variable
  xlab("Time)") +
  scale_color_brewer(palette="Dark2")+
  #viridis::scale_colour_viridis(discrete = TRUE, begin = 0, end = 1)+
  theme_bw() + 
  theme( text = element_text(size = 14), legend.position = "bottom")+
  guides(colour = guide_legend(nrow = 2))+
  ylab("Number of Countries")

print(p)


## ---- distance_learning_world
## Offline data retrieval
keywords= c("Online Learning", "Online teaching",
            "Distance learning","Distance education",
            "online proctoring")
#data_world <- purrr::map_df(keywords, get_info_world)
category <- categories[
  categories$name=="Education",2] %>% 
  as.numeric()
#set channels 
channel <- 'web'
trends <-  gtrends(keywords,
                   gprop =channel,
                   time = "2017-12-01 2021-08-15" )
trends <- trends$interest_over_time
write.csv(trends, here::here(
  "data", "education_search_world.csv"))



## ---- covid19_trend_world
## Offline data retrieval
keywords= c("covid 19", "corona", "covid")
#set channels 
channel <- 'web'
trends <-  gtrends(keywords, gprop =channel,
                   time = "2017-12-01 2021-08-15" )
trends <- trends$interest_over_time
write.csv(trends, here::here( "data",
  "covid_search_world.csv"))


## ---- covid19_vaccine_trend_world
## Offline data retrieval
keywords= c("COVID-19 vaccine", "covid vaccine")
#set channels 
channel <- 'web'
trends <-  gtrends(keywords, gprop =channel,
                   time = "2017-12-01 2021-08-15" )
trends <- trends$interest_over_time
write.csv(trends, here::here("data",
  "covid_vaccine_search_world.csv"))


## ---- covid_epidemic_datasave
# Data retrieved from coronavirus R package on 19-01-2021
library(coronavirus)
data("coronavirus")

covid_cases <- coronavirus %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, 
              values_from = total_cases) %>%
  arrange(date) 
  #mutate(active = confirmed - death - recovered)

covid_cases <- covid_cases %>% as_tsibble(index = date)
write.csv(covid_cases, here::here(
  "data", "covid_cases.csv"))

## ---- vaccine_datasave
# Data retrieved from coronavirus R package on 19-01-2021
data("covid19_vaccine")

covid_vaccine <- covid19_vaccine %>% 
  select(date, people_partially_vaccinated, 
         people_fully_vaccinated) %>%
  pivot_longer(
    cols = people_partially_vaccinated: people_fully_vaccinated,
    names_to = "type",
    values_to = "count"
  ) %>% 
  group_by(type, date) %>%
  summarise(total_vaccine = sum(count)) %>%
  pivot_wider(names_from = type, 
              values_from = total_vaccine) %>%
  arrange(date) 
#mutate(active = confirmed - death - recovered)

covid_vaccine <- covid_vaccine %>% as_tsibble(index = date)
write.csv(covid_vaccine, here::here(
  "data", "covid_vaccine.csv"))



## ---- distanceLearningWorldAnalysis
# covid actual cases
covidcasesW <- read.csv(here::here( "data",
  "covid_cases.csv"))
covidcasesW <- covidcasesW %>% select(-X)

covidcasesW1 <- covidcasesW %>% pivot_longer(
  cols = confirmed : recovered,
  names_to = "Type",
  values_to = "Counts") %>%
  mutate(Type = factor(Type, levels = c("confirmed", "death", "recovered")))%>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(index = date, key = Type) %>%
  mutate(Counts = ifelse(Counts <0, NA, Counts))


p1 <- covidcasesW1 %>%  autoplot() +
  xlab("date") + 
  scale_color_brewer(palette="Dark2") +
  scale_x_date(limits = as.Date(c('2017-12-03','2021-08-08'))) +
  theme_bw()+
  theme(legend.position = "bottom", text = element_text(size = 14)) +
  ggtitle("(a)")+
  xlab("")


# covid vaccine cases
covidVaccine <- read.csv(here::here( "data",
  "covid_vaccine.csv"))
covidVaccine <- covidVaccine %>% select(-X)

covidVaccine1 <- covidVaccine %>% pivot_longer(
  cols = people_fully_vaccinated : people_partially_vaccinated,
  names_to = "Type",
  values_to = "Counts") %>%
  mutate(Type = factor(Type, 
                       levels = c("people_fully_vaccinated", 
                                  "people_partially_vaccinated")))%>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(index = date, key = Type) %>%
  mutate(Counts = ifelse(Counts <0, NA, Counts))


p2 <- covidVaccine1 %>%  autoplot() +
  scale_color_brewer(palette="Dark2")  +
  scale_x_date(limits = as.Date(c('2017-12-03','2021-08-08'))) +
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))

## Covid search
data_covid <- read.csv( here::here( "data",
  "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)

keyword_ord<- data_covid %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_covid %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")

p3<- dataW %>%  ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) + 
  scale_color_brewer(palette="Dark2")  +
  scale_x_date(limits = as.Date(c('2017-12-03','2021-08-08'))) +
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14)) +
  ggtitle("(b)")+
  xlab("")


## Covid vacicne search trend

data_covidvaccine <- read.csv( here::here( "data",
  "covid_vaccine_search_world.csv"))
hits1<- ifelse(data_covidvaccine$hits ==  "<1", 0, data_covidvaccine$hits)
data_covidvaccine$hits <- as.numeric(hits1)

keyword_ord<- data_covidvaccine %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataV <- data_covidvaccine %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataV$date = as.Date(dataV$date, "%Y-%m-%d")


p4<- dataV %>%  ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) + 
  scale_color_brewer(palette="Dark2")  +
  scale_x_date(limits = as.Date(c('2017-12-03','2021-08-08'))) +
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))


## covid google trends
data_world <- read.csv( here::here( "data", 
  "education_search_world.csv"))

# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")


p5<- dataW%>% ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) +
  scale_color_brewer(palette="Dark2")  +
  scale_x_date(limits = as.Date(c('2017-12-03','2021-08-08'))) +
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))+
  guides(colour = guide_legend(nrow = 2, title="keywords"))+
  ggtitle("(c)")

p <- p1/p3/p5

print(p)



## ---- dtw

# Covid cases
covidcasesW <- read.csv(here::here( "data", "covid_cases.csv"))
covidcasesW <- covidcasesW %>% select(-X)

## covid search google trends
data_covid <- read.csv( here::here(
  "data", "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)
# covid search google trends - arrange in wide format
covid_search <- data_covid %>%
  select(date, hits, keyword) %>%
  pivot_wider(names_from = keyword, values_from = hits)

## online learning google trends
data_world <- read.csv( here::here(
  "data", "education_search_world.csv"))
# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)
# arrange in wide format
edu_search <- data_world %>%
  select(date, hits, keyword) %>%
  pivot_wider(names_from = keyword, values_from = hits)

## Time Lagged Cross Correlation 
library(fpp3)
data <- full_join(covid_search, 
                  edu_search,  by = "date" ) %>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(index = date)

library(dtw)
x<- scale(data$`Online Learning`)
y<- scale(data$corona)
z<- scale(data$covid)

xyz <- cbind(x,y,z)
xyz <- na.omit(xyz)

align1 <- dtw(xyz[,1], xyz[,1]*0, keep = T)
align1 <- dtw(xyz[,1], xyz[,2], keep = T)
align2 <- dtw(xyz[,1], xyz[,3], keep = T)


## ---- dtw1
# par(mfrow=c(2,2), mar=c(0,1,0.75,0))
p1 <- dtwPlotTwoWay(align1,
          main = "(a) Pointwise comparison between 'Online Learning' and 'corona'")
p2 <- dtwPlotDensity(align1, normalize = TRUE, main = "(b) Cumulative cost density 
                     with the warping path between 'Online Learning' and 'corona' ")


## ---- dtw2
p3 <- dtwPlotTwoWay(align2, main = "(a) Pointwise comparison between 'Online Learning' and 'covid'")
p4 <-dtwPlotDensity(align2, normalize = TRUE, main = "(b) Cumulative cost density 
                    with the warping path between 'Online Learning' and 'covid'")


## ---- ccfAnalysis
# Covid cases
covidcasesW <- read.csv(here::here("data", "covid_cases.csv"))
covidcasesW <- covidcasesW %>% select(-X)

## covid search google trends
data_covid <- read.csv( here::here( "data", "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)
# covid search google trends - arrange in wide format
covid_search <- data_covid %>%
  select(date, hits, keyword) %>%
  pivot_wider(names_from = keyword, values_from = hits)

## online learning google trends
data_world <- read.csv( here::here( "data", "education_search_world.csv"))
# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)
# arrange in wide format
edu_search <- data_world %>%
  select(date, hits, keyword) %>%
  pivot_wider(names_from = keyword, values_from = hits)

## Time Lagged Cross Correlation 
library(fpp3)
data <- full_join(covid_search, edu_search,  by = "date" ) %>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(index = date)


p1 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_online = difference(`Online Learning`)) %>%
  CCF(diff_corona, diff_online ) %>%
  autoplot()+
  labs(title= "(a) 'Corona' with 'Online learning'")

p2 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_dist = difference(`Distance learning`)) %>%
  CCF(diff_corona, diff_dist ) %>%
  autoplot()+
  labs(title= "(b) 'Corona' with 'Distance learning'")

p3 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_teach = difference(`Online teaching`)) %>%
  CCF(diff_corona, diff_teach ) %>%
  autoplot()+
  labs(title= "(c) 'Corona' with 'Online teaching'")

p4 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_edu = difference(`Distance education`)) %>%
  CCF(diff_corona, diff_edu ) %>%
  autoplot()+
  labs(title= "(d) 'Corona' with 'Distance education'")

p5 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_proc = difference(`online proctoring`)) %>%
  CCF(diff_corona, diff_proc ) %>%
  autoplot()+
  labs(title= "(e) 'Corona' with 'Online proctoring'")



pa <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_online = difference(`Online Learning`)) %>%
  CCF(diff_corona, diff_online ) %>%
  autoplot()+
  labs(title= "(f) 'Covid 19' with 'Online learning")

pb <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_dist = difference(`Distance learning`)) %>%
  CCF(diff_corona, diff_dist ) %>%
  autoplot()+
  labs(title= "(g) 'Covid 19' with 'Distance learning'")

pc <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_teach = difference(`Online teaching`)) %>%
  CCF(diff_corona, diff_teach ) %>%
  autoplot()+
  labs(title= "(h) 'Covid 19' with 'Online teaching'")

pd <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_edu = difference(`Distance education`)) %>%
  CCF(diff_corona, diff_edu ) %>%
  autoplot()+
  labs(title= "(i) 'Covid 19' with 'Distance education'")

pe <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_proc = difference(`online proctoring`)) %>%
  CCF(diff_corona, diff_proc ) %>%
  autoplot()+
  labs(title= "(j) 'Covid 19' with 'Online proctoring'")


(p1 | pa ) / 
  (p2 | pb )/
  (p3 | pc )/
  (p4 | pd )/
  (p5 | pe )






## ---- download_education_data

#Google Trends categories.
# set category- use 'data(categories)' to retreive valid codes. 74- Education
category <- categories[
  categories$name=="Education",2] %>%
  as.numeric()
#set channels 
channel <- 'web'
edu_world <- gtrends( gprop =channel,
                      time = time ,
                      category = category)
write.csv(edu_world$interest_over_time, 
          here::here("data", 
                     "trends_edu_world.csv"))
write.csv(edu_world$related_topics,
          here::here("data", 
                     "related_topics_edu_world.csv"))
write.csv(edu_world$related_queries, 
          here::here( "data",
                     "related_queries_edu_world.csv"))



# set category- use 'data(categories)' to retreive valid codes. 74- Education
category <- categories[
  categories$name=="Distance Learning",2] %>% 
  as.numeric()
#set channels 
channel <- 'web'
dist_edu_world <- gtrends( gprop =channel, 
                           time = time ,
                           category = category)
write.csv(dist_edu_world$interest_over_time,
          here::here("data",
                     "trends_dist_edu_world.csv"))
write.csv(dist_edu_world$related_topics,
          here::here("data", 
                     "related_topics_dist_edu_world.csv"))
write.csv(dist_edu_world$related_queries, 
          here::here("data", 
                     "related_queries_dist_edu_world.csv"))



# set category- use 'data(categories)' to retreive valid codes. 74- Education
category <- categories[
  categories$name=="Education",2] %>%
  as.numeric()
#set the geographic area: DE = Germany
# use this to find country code https://en.wikipedia.org/wiki/ISO_3166-2
country <- c('LK') 
#set channels 
channel <- 'web'
edu_SL <- gtrends( gprop =channel, 
                   geo=country, time = time ,
                   category = category)
write.csv(edu_SL$interest_over_time, 
          here::here("data", "trends_edu_SL.csv"))
write.csv(edu_SL$related_topics, 
          here::here("data", 
                     "related_topics_edu_SL.csv"))
write.csv(edu_SL$related_queries, 
          here::here("data", 
                     "related_queries_edu_SL.csv"))



# set category- use 'data(categories)' to retreive valid codes. 74- Education
category <- categories[
  categories$name=="Distance Learning",2] %>%
  as.numeric()
#set the geographic area: DE = Germany
# use this to find country code https://en.wikipedia.org/wiki/ISO_3166-2
country <- c('LK') 
#set channels 
channel <- 'web'
dist_edu_SL<- gtrends( gprop =channel, 
                       geo=country, 
                       time = time , 
                       category = category)
write.csv(dist_edu_SL$interest_over_time, 
          here::here("data", 
                     "trends_dist_edu_SL.csv"))
write.csv(dist_edu_SL$related_topics,
          here::here("data", 
                     "related_topics_dist_edu_SL.csv"))
write.csv(dist_edu_SL$related_queries,
          here::here("data", 
                     "related_queries_dist_edu_SL.csv"))





## ---- Digital_lms_data_download

keywords= c("CenturyTech", "ClassDojo", "Edmodo", "Edraak",
            "EkStep", "Google Classroom", "Moodle", "Nafham",
            "Paper Airplanes", "Schoology", "Seesaw", "Skooler")
#data_world <- purrr::map_df(keywords, get_info_world)

# Google Classroom is the maximum category by 15-08-2021
keywords1 <- c("Google Classroom",
               "CenturyTech", "ClassDojo", 
               "Edmodo", "Edraak" )
trends1 <-  gtrends(keywords1, 
                    gprop =channel, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("Google Classroom", "EkStep",
               "Moodle", "Nafham", 
               "Paper Airplanes")
trends2 <-  gtrends(keywords2, 
                    gprop =channel, time = time )
trends2 <- trends2$interest_over_time
keywords3 <- c("Google Classroom", "Schoology", 
               "Seesaw", "Skooler")
trends3 <-  gtrends(keywords3, gprop =channel,
                    time = time )
trends3 <- trends3$interest_over_time

trends21 <-trends2 %>% 
  filter(keyword != "Google Classroom")
trends31 <-trends3 %>% 
  filter(keyword != "Google Classroom")

data_world <- bind_rows(trends1, trends21, 
                        trends31)
write.csv(data_world,
          here::here("data", 
                     "Digital_lms_world.csv"))


## ---- DigitalLmsAnalysis
fontsize = 12

keywords= c("CenturyTech", "ClassDojo", "Edmodo", "Edraak",
            "EkStep", "Google Classroom", "Moodle", "Nafham",
            "Paper Airplanes", "Schoology", "Seesaw", "Skooler")
#data_world <- purrr::map_df(keywords, get_info_world)

data_world <- read.csv( here::here("data", "Digital_lms_world.csv"))

hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")


p1 <- dataW %>%
  as_tsibble(index= date, key = keyword) %>%
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom", 
        text = element_text(size = fontsize), 
        legend.title = element_blank(),
        legend.text = element_text(size = fontsize) )+
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle = "(a) Digital learning management systems
")


## ---- mobile_phones_apps_data_download
keywords= c("Cell-Ed", "Eneza Education", "Funzi", 
            "KaiOS", "Ubongo", 
            "Ustad Mobile")

# Ubongo is the maximum category by 15-08-2021
keywords1 <- c("Ubongo", "Cell-Ed", "Eneza Education", "Funzi", "Ubongo")
trends1 <-  gtrends(keywords1, gprop =channel, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("Ubongo", "Ustad Mobile")
trends2 <-  gtrends(keywords2, gprop =channel, 
                    time = time )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Ubongo")
data_world <- bind_rows(trends1, trends21)
#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_world, 
          here::here("data", 
                     "mobile_phones_apps_world.csv"))




## ---- mobile_phones_apps_analysis
keywords= c("Cell-Ed", "Eneza Education", "Funzi", "KaiOS", "Ubongo", 
            "Ustad Mobile")
data_world <- read.csv( here::here("data", "mobile_phones_apps_world.csv"))


keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")

p2<- dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>%
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(b) Systems built for use on basic mobile phones
")



## ---- offline_functionality_data_download
keywords <-  c("Kolibri", "Rumie", "Ustad Mobile")
trends <-  gtrends(keywords, gprop =channel, 
                   time = time )
trends <- trends$interest_over_time
data_world <- trends
#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_world,
          here::here("data", 
                     "offline_functionality_world.csv"))

## ---- offline_functionality_analysis
keywords <-  c("Kolibri", "Rumie", "Ustad Mobile")
data_world <- read.csv( here::here( "data", "offline_functionality_world.csv"))
hits1<- ifelse(
  data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)
keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")


p3<- dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>% 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
 # scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(c) Systems with strong offline functionality")

## ---- mooc_data_download
keywords= c("Alison", "Canvas", "Coursera", "European Schoolnet Academy", 
            "EdX", "iCourse", "Future Learn", "Icourses", "TED-Ed Earth School",
            "Udemy", "XuetangX")

## Canvas is the maximum search category by 15-08-2021

keywords1 <- c("Canvas", "Alison", "Coursera", "European Schoolnet Academy", "EdX")
trends1 <-  gtrends(keywords1, gprop =channel, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("Canvas", "iCourse", "Future Learn", "Icourses", "TED-Ed Earth School")
trends2 <-  gtrends(keywords2, gprop =channel, time = time )
trends2 <- trends2$interest_over_time
keywords3 <- c("Canvas", "Udemy", "XuetangX")
trends3 <-  gtrends(keywords3, gprop =channel, time = time )
trends3 <- trends3$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Canvas")
trends31 <-trends3 %>%  filter(keyword != "Canvas")
data_world <- bind_rows(trends1, trends21, trends31)
#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_world, here::here( "data", "mooc_world.csv"))


## ---- mooc_analysis
keywords= c("Alison", "Canvas", "Coursera", "European Schoolnet Academy", 
            "EdX", "iCourse", "Future Learn", "Icourses", "TED-Ed Earth School",
            "Udemy", "XuetangX")
data_world <- read.csv( here::here( "data", "mooc_world.csv"))

hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p4 <- dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>% 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(d) Massive Open Online Course (MOOC) Platforms")



## ---- Self_directed_learning_data_download
keywords= c("ABRA", "British Council", "Byju’s", "Code It", 
            "Code.org", "Code Week", "Discovery Education", "Duolingo",
            "Edraak", "Facebook Get Digital", "Feed the Monster",
            "History of Africa", "Geekie", "Khan Academy",
            "KitKit School", "LabXchange", "Madrasa",
            "Mindspark", "Mosoteach", "Music Crab", "OneCourse",
            "Polyup", "Quizlet", "SDG Academy Library", "Siyavula",
            "Smart History", "youtube")
#### consider youtube seperately for the analysis


# Quizlet  highest as of 15-08-2021
keywords1 <- c("Quizlet", "ABRA", "British Council", "Byju’s", "Code It")
trends1 <-  gtrends(keywords1, gprop =channel, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("Quizlet", "Code.org", "Code Week", "Discovery Education", "Duolingo")
trends2 <-  gtrends(keywords2, gprop =channel, time = time )
trends2 <- trends2$interest_over_time
keywords3 <- c("Quizlet", "Edraak", "Facebook Get Digital", "Feed the Monster",
               "History of Africa")
trends3 <-  gtrends(keywords3, gprop =channel, time = time )
trends3 <- trends3$interest_over_time
keywords4 <- c("Quizlet", "Geekie", "Khan Academy", "KitKit School", "LabXchange")
trends4 <-  gtrends(keywords4, gprop =channel, time = time )
trends4 <- trends4$interest_over_time
keywords5 <- c("Quizlet", "Madrasa", "Mindspark", "Mosoteach", "Music Crab")
trends5 <-  gtrends(keywords5, gprop =channel, time = time )
trends5 <- trends5$interest_over_time
keywords6 <- c("Quizlet","OneCourse", "Polyup", "Quizlet", "SDG Academy Library")
trends6 <-  gtrends(keywords6, gprop =channel, time = time )
trends6 <- trends6$interest_over_time
keywords7 <- c("Quizlet", "Siyavula", "Smart History")
trends7 <-  gtrends(keywords7, gprop =channel, time = time )
trends7 <- trends7$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Quizlet")
trends31 <-trends3 %>%  filter(keyword != "Quizlet")
trends41 <-trends2 %>%  filter(keyword != "Quizlet")
trends51 <-trends3 %>%  filter(keyword != "Quizlet")
trends61 <-trends2 %>%  filter(keyword != "Quizlet")
trends71 <-trends3 %>%  filter(keyword != "Quizlet")
data_world <- bind_rows(trends1, trends21, trends31, trends41, trends51, trends61, trends71)

#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_world, here::here( "data", "Self_directed_learning_world.csv"))

keywords <- c("Quizlet", "YouTube")
trends <-  gtrends(keywords, gprop =channel, time = time )
youtube_world <- trends$interest_over_time
write.csv(youtube_world, here::here( "data", "youtube_world.csv"))


## ---- Self_directed_learning_analysis
keywords= c("ABRA", "British Council", "Byju’s", "Code It", 
            "Code.org", "Code Week", "Discovery Education", "Duolingo",
            "Edraak", "Facebook Get Digital", "Feed the Monster",
            "History of Africa", "Geekie", "Khan Academy",
            "KitKit School", "LabXchange", "Madrasa",
            "Mindspark", "Mosoteach", "Music Crab", "OneCourse",
            "Polyup", "Quizlet", "SDG Academy Library", "Siyavula",
            "Smart History", "youtube")
data_world <- read.csv( here::here( "data", "Self_directed_learning_world.csv"))

hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p5 <-  dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>% 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom" ,  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(e) Self-directed learning content")

## ---- Mobile_reading_data_download
keywords= c("African Storybook", "Global Digital Library", "Reads", 
            "Room to Read", "StoryWeaver", "Worldreader")

# Reads is the  highest as of 15-08-2021
keywords1 <- c("African Storybook", "Global Digital Library", "Reads", 
               "Room to Read", "StoryWeaver")
trends1 <-  gtrends(keywords1, gprop =channel, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("Reads", "Worldreader")
trends2 <-  gtrends(keywords2, gprop =channel, time = time )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Reads")
data_world <- bind_rows(trends1, trends21)
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "Mobile_reading_world.csv"))



## ---- Mobile_reading_analysis
keywords= c("African Storybook", "Global Digital Library", "Reads", 
            "Room to Read", "StoryWeaver", "Worldreader")

data_world <- read.csv( here::here( "data", "Mobile_reading_world.csv"))

hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p6 <-  dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>% 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(f) Mobile reading applications")



## ---- Collaboration_platforms_data_download 
## entire world

keywords= c("Zoom", "Dingtalk", "Lark", "Hangouts Meet",
            "Teams", "Skype", "WhatsApp")

# WhatsApp  highest as of 15-01-2021
keywords1 <- c("Zoom", "Dingtalk", "Lark", "Hangouts Meet", "WhatsApp")
trends1 <-  gtrends(keywords1, gprop =channel, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("Teams", "Skype", "WhatsApp")
trends2 <-  gtrends(keywords2, gprop =channel, time = time )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "WhatsApp")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)

data_world <- bind_rows(trends1, trends21)
write.csv(data_world, here::here( "data", "Collaboration_platforms_world.csv"))


## ---- Collaboration_platforms_eduonly_data_download 

### This analysis was restricted to education catgory:
#Otherwise hits generates and error


category <- categories[categories$name=="Education",2] %>% as.numeric()
keywords= c("Zoom", "Dingtalk", "Lark", "Hangouts Meet",
            "Teams", "Skype", "WhatsApp")

# WhatsApp is the  highest search in education category as of 18-01-2021
keywords1 <- c("Zoom", "Dingtalk", "Lark", "Hangouts Meet", "WhatsApp")
trends1 <-  gtrends(keywords1, gprop =channel, time = time, category = category )
trends1 <- trends1$interest_over_time
keywords2 <- c("Teams", "Skype", "WhatsApp")
trends2 <-  gtrends(keywords2, gprop =channel, time = time, category = category )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "WhatsApp")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)

data_world <- bind_rows(trends1, trends21)
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "edu_only_Collaboration_platforms_world.csv"))




## ---- Collaboration_platforms_eduonly_analysis

data_world <- read.csv( here::here( "data", "edu_only_Collaboration_platforms_world.csv"))

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p7 <-  dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>% 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(g) Collaboration platforms that support live-video communication")




## ---- Tools_for_teachers_data_download
keywords= c("Thinglink", "Buncee", "EdPuzzle", "EduCaixa",
            "Kaltura", "Nearpod", "Pear Deck", "Squigl", "Trello")

# Nearpod is the highest search  as of 15-08-2021
keywords1 <- c("Nearpod", "Thinglink", "Buncee", "EdPuzzle", "EduCaixa")
trends1 <-  gtrends(keywords1, gprop =channel, time = time)
trends1 <- trends1$interest_over_time
keywords2 <- c("Kaltura", "Nearpod", "Pear Deck", "Squigl", "Trello")
trends2 <-  gtrends(keywords2, gprop =channel, time = time )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Nearpod")

data_world <- bind_rows(trends1, trends21)
write.csv(data_world, here::here( "data", "Tools_for_teachers_world.csv"))

## ---- Tools_for_teachers_analysis

data_world <- read.csv( here::here( "data", "Tools_for_teachers_world.csv"))
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p8 <-  dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>% 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom", 
        text = element_text(size = fontsize),
        legend.title = element_blank(),
        legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(h) Tools for teachers to create of digital learning content")


## ---- External_repositories_DL_data_download

### world  data
keywords= c("Brookings", "Common Sense Education", "Commonweatlh of Learning",
            "Education Nation", "EdSurge", "Global Business Coalition for Education",
            "Keep Learning Going", "Koulu.me", "Organisation internationale de la Francophonie",
            "UNEVOC Resources", "UNHCR")

# Brookings is the  highest search  as of 18-01-2021
keywords1 <- c("Brookings", "Common Sense Education", "Commonweatlh of Learning",
               "Education Nation", "EdSurge")
trends1 <-  gtrends(keywords1, gprop =channel, time = time)
trends1 <- trends1$interest_over_time
keywords2 <- c("Brookings","Global Business Coalition for Education",
               "Keep Learning Going", "Koulu.me", "Organisation internationale de la Francophonie")
trends2 <-  gtrends(keywords2, gprop =channel, time = time )
trends2 <- trends2$interest_over_time
keywords3 <- c("Brookings", "UNEVOC Resources", "UNHCR")
trends3 <-  gtrends(keywords3, gprop =channel, time = time )
trends3 <- trends3$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Brookings")
trends31 <-trends3 %>%  filter(keyword != "Brookings")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)
trends31$hits <- as.numeric( trends31$hits)
data_world <- bind_rows(trends1, trends21, trends31)
write.csv(data_world, here::here("data", "External_repositories_DL_world.csv"))

## ---- External_repositories_DL_analysis
keywords= c("Brookings", "Common Sense Education", "Commonweatlh of Learning",
            "Education Nation", "EdSurge", "Global Business Coalition for Education",
            "Keep Learning Going", "Koulu.me", "Organisation internationale de la Francophonie",
            "UNEVOC Resources", "UNHCR")

data_world <- read.csv( here::here( "data", "External_repositories_DL_world.csv"))

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p9 <-  dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>% 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom", 
        text = element_text(size = fontsize), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 7))+
 # scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(i) External repositories of distance learning solutions")



## ---- psychosocial_support_data_download

keywords <- c("psychosocial support")

trends <- gtrends(keywords, gprop =channel, time = time)
data_world_all <- trends$interest_over_time
category <- categories[categories$name=="Education",2] %>% as.numeric()
trends <- gtrends(keywords, gprop =channel, time = time, category = category )
data_world_edu <- trends$interest_over_time
data_world <- bind_rows(data_world_all, data_world_edu )
write.csv(data_world, here::here( "data", "psychosocial_support_world.csv"))


## ---- psychosocial_support_analysis
keywords <- c("psychosocial support")
data_world <- read.csv( here::here( "data", "psychosocial_support_world.csv"))

dataW <- data_world %>%
  select(date, category, hits) %>%
  mutate(category = factor(category, levels = c(0, 74), labels = c("All", "Education")))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p1 <- dataW %>% 
  ggplot(aes(x=date,y=category,fill=hits))+
  geom_tile(size=0.1, colour = "grey50")+
  scale_y_discrete(expand=c(0,0))+
  ggtitle("All")+
  labs(x="",y="")+
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read_csv(here::here("Paper_online_Learning_Dec", "data", "psychosocial_support_SL.csv"))

if(nrow(data_SL) == 0)
{
  print(p1)
} else{
  dataSL <- data_SL %>%
    select(date, category, hits) %>%
    mutate(category = factor(category, levels = c(0, 74), labels = c("All", "Education")))
  p2 <- dataSL %>% 
    ggplot(aes(x=date,y=category,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    scale_x_discrete(expand=c(0,0),
                     breaks=c("2019-12-01","2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01"))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
  print(p)
}



## ---- Online_proctoring_data_download
# source : https://www.softwaresuggest.com/blog/best-online-exam-proctoring-software/#7_ExamSoft
# source 2: https://blog.mettl.com/top-5-proctoring-solution-providers/
keywords= c("Mettl", "Examus", "ProctorU", "Examity", "Verificient",
            "AIProctor","ExamSoft", "Proview", "Conduct Exam",
            "ProctorExam", "PSI Bridge", "Pearson VUE", 
            "MeritTrac", "Honorlock", "Proctortrack", "Proctorio",
            "Talview")
# Pearson VUE is the  highest search as of 15-08-2021
keywords1 <- c("Pearson VUE", "Mettl", "Examus", "ProctorU", 
               "Examity")
trends1 <-  gtrends(keywords1, gprop =channel, time = time)
trends1 <- trends1$interest_over_time
keywords2 <- c("Pearson VUE","Verificient", "AIProctor",
               "ExamSoft", "Proview")
trends2 <-  gtrends(keywords2, gprop =channel, time = time )
trends2 <- trends2$interest_over_time
keywords3 <- c("Pearson VUE", "Conduct Exam", "ProctorExam", 
               "PSI Bridge", "MeritTrac")
trends3 <-  gtrends(keywords3, gprop =channel, time = time )
trends3 <- trends3$interest_over_time
keywords4 <- c("Pearson VUE", "Honorlock", "Proctortrack", "Proctorio",
               "Talview")
trends4 <-  gtrends(keywords4, gprop =channel, time = time )
trends4 <- trends4$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Pearson VUE")
trends31 <-trends3 %>%  filter(keyword != "Pearson VUE")
trends41 <-trends4 %>%  filter(keyword != "Pearson VUE")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)
trends31$hits <- as.numeric( trends31$hits)
trends41$hits <- as.numeric( trends41$hits)
data_world <- bind_rows(trends1, trends21, trends31, trends41)
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "Online_proctoring_world.csv"))




## ---- Online_proctoring_analysis
keywords= c("Mettl", "Examus", "ProctorU", "Examity", "Verificient",
            "AIProctor","ExamSoft", "Proview", "Conduct Exam",
            "ProctorExam", "PSI Bridge", "Pearson VUE", 
            "MeritTrac", "Honorlock", "Proctortrack", "Proctorio",
            "Talview")

data_world <- read.csv( here::here( "data", "Online_proctoring_world.csv"))

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p10 <-  dataW %>% as_tibble() %>%
  distinct(date, keyword, .keep_all = TRUE) %>%
  as_tsibble(index= date, key = keyword) %>%
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(j) Tools for online proctoring")

## ---- plot1
p <- (p1)/
  (p2) /
  (p3) /
  (p4) /
  (p5)
print(p)

## ---- plot2
p <- (p6)/
  (p7) /
  (p8) /
  (p9) /
  (p10)
print(p)




## ---- other

keywords= c("Google Classroom - Topic", 
            "Learning management system - Topic",
            "Moodle - Topic",
            "Virtual learning environment",
            "zoom")
# filter only  education
category <- categories[categories$name=="Education",2] %>% as.numeric()
#set the geographic area: DE = Germany
country=c('LK')  # use this to find country code https://en.wikipedia.org/wiki/ISO_3166-2
#set the time window
time=("2019-01-01 2021-08-15")
#set channels 
channel='web'
trends = gtrends(keywords, gprop =channel,geo=country, time = time, category = category )






## ---- extract_data_most_popular
## Most popular quiries in education 

# set category- use 'data(categories)' to retreive valid codes. 74- Education
category <- categories[categories$name=="Education",2] %>% as.numeric()
#set the geographic area: DE = Germany
# use this to find country code https://en.wikipedia.org/wiki/ISO_3166-2
country <- c('LK') 
#set the time window
# First Sri Lankan found on 3 March 2020
time <- ("2020-03-03 2021-08-15")
#set channels 
channel <- 'web'
trends <- gtrends( gprop =channel,geo=country, time = time , category = category)
#select only releted topics
related_topics_edu <- trends$related_topics
# filter only rising values
related_topics_edu %>% filter(related_topics == "rising" ) %>% 
  select(value) %>% unique()



## ---- extract_data_distance_learning
## MAnusally selected the followin ketwords related to online education in Sri Lanka
keywords= c("Google Classroom", 
            "Learning management system",
            "Moodle",
            "Virtual learning environment",
            "zoom")
# filter only  education category
category <- categories[categories$name=="Education",2] %>% as.numeric()
#set the geographic area: DE = Germany
country=c('LK')  # use this to find country code https://en.wikipedia.org/wiki/ISO_3166-2
#set the time window
time=("2019-01-01 2021-08-15")
#set channels 
channel='web'
trends_dis_edu <- gtrends(keywords, gprop =channel,geo=country, time = time, category = category )




## ---- delete

covidcasesW1 <- covidcasesW %>% pivot_longer(
  cols = confirmed : recovered,
  names_to = "Type",
  values_to = "Counts") %>%
  mutate(Type = factor(Type, levels = c("confirmed", "death", "recovered")))%>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(index = date, key = Type) %>%
  mutate(Counts = ifelse(Counts <0, NA, Counts))


pc <- covidcasesW1 %>%  autoplot() +
  scale_color_viridis_d(direction = -1, end=0.8) +
  scale_x_date(limits = as.Date(c('2017-12-03','2021-01-10')))





## covid google trends
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "education_search_world.csv"))

# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")

p1 <- dataW %>% 
  ggplot(aes(x=date,y=keyword,fill=hits))+
  geom_tile(size=0.1, colour = "grey50")+
  scale_y_discrete(expand=c(0,0))+
  ggtitle("All")+
  labs(x="",y="")+
  scale_fill_viridis(guide = "colourbar")+
  theme(panel.grid = element_blank(), text = element_text(size = 16))

#p1

pa<- dataW%>% ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) +
  #scale_color_manual(values = c("red", viridis::viridis(6)))
  scale_color_viridis_d(direction = -1, end=0.8)


data_covid <- read.csv( here::here("Paper_online_Learning_Dec", "data", "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)

keyword_ord<- data_covid %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW <- data_covid %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")

pb<- dataW %>%  ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) + 
  scale_color_viridis_d(direction = -1, end=0.8)

#p<- gridExtra::grid.arrange(p1,p2, ncol=1)
p<- ggarrange(pc,pb,pa, ncol=1, common.legend = FALSE, legend="bottom", align = "hv")
print(p)

######################################