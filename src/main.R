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
  theme(legend.position = "bottom", text = element_text(size = 14))


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
        text = element_text(size = 14))


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
  guides(colour = guide_legend(nrow = 2, title="keywords"))

p <- p1/p3/p5

print(p)


## ---- ccf_analysis_covid_online_edu
# Cross-Correlation Function Estimation
data_world <- read.csv( here::here(
  "Paper_online_Learning_Dec", "data",
  "education_search_world.csv"))
# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0,
               data_world$hits)
data_world$hits <- as.numeric(hits1)
keyword_ord<- data_world %>% 
  group_by(keyword) %>%
  summarise(total_hits = 
              sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% 
  select(keyword) %>% 
  as_vector()
dataW_edu <- data_world %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, 
                          levels = keyword_ord))
dataW_edu$date = as.Date(dataW$date, "%Y-%m-%d")
print(head(dataW_edu))

d_edu<- dataW_edu %>% 
  filter(keyword == "Online teaching") %>%
  select(-keyword) 
d_edu$date = as.Date(d_edu$date, "%Y-%m-%d")
d_edu <- d_edu %>%as_tsibble(index = date) 
print(head(d_edu))

data_covid <- read.csv( here::here(
  "Paper_online_Learning_Dec", "data", 
  "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)
keyword_ord<- data_covid %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()

dataW_covid <- data_covid %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW_covid$date = as.Date(dataW_covid$date, "%Y-%m-%d")

d_covid<- dataW_covid %>% 
  filter(keyword == "corona") %>%
  select(-keyword) %>% 
  as_tsibble(index = date) 
data2 <- left_join(d_edu, d_covid, by =  "date",
                  suffix = c(".edu", ".corona"))

p<- data2 %>% CCF(hits.edu, hits.corona) %>%autoplot()
print(p)

#print(p)
#tbl1 <- tibble(
#  date = as.Date("2017-01-01") + 0:99,
#  value = rnorm(100)
#) 

#tbl1 <- as_tsibble(tbl1)

#tbl2 <- tibble(
 # date = as.Date("2017-01-01") + 0:99,
#  value = rnorm(100, 50)
#) 

#tbl2 <- as_tsibble(tbl1)

#data <- left_join(tbl1, tbl2, by = "date")
#data %>% CCF(value.x, value.y) %>% autoplot()

## source: https://online.stat.psu.edu/stat510/lesson/8/8.2
# The result, showing lag (the  in xt+h) and correlation with yt :


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
          here::here("Paper_online_Learning_Dec", 
                     "data", 
                     "trends_edu_world.csv"))
write.csv(edu_world$related_topics,
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "related_topics_edu_world.csv"))
write.csv(edu_world$related_queries, 
          here::here("Paper_online_Learning_Dec",
                     "data",
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
          here::here("Paper_online_Learning_Dec",
                     "data",
                     "trends_dist_edu_world.csv"))
write.csv(dist_edu_world$related_topics,
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "related_topics_dist_edu_world.csv"))
write.csv(dist_edu_world$related_queries, 
          here::here("Paper_online_Learning_Dec",
                     "data", 
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
          here::here("Paper_online_Learning_Dec",
                     "data", "trends_edu_SL.csv"))
write.csv(edu_SL$related_topics, 
          here::here("Paper_online_Learning_Dec", 
                     "data", 
                     "related_topics_edu_SL.csv"))
write.csv(edu_SL$related_queries, 
          here::here("Paper_online_Learning_Dec", 
                     "data", 
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
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "trends_dist_edu_SL.csv"))
write.csv(dist_edu_SL$related_topics,
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "related_topics_dist_edu_SL.csv"))
write.csv(dist_edu_SL$related_queries,
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "related_queries_dist_edu_SL.csv"))


## ---- Draw_map_wolrd_SL

# TS plot for education world
# TS plot for distance Learning world
# TS plot for education SL
# TS plot for distance Learning  SL

W_edu <- read.csv(
  here::here("Paper_online_Learning_Dec", 
             "data", "trends_edu_world.csv"))
W_dist_edu <- read.csv(
  here::here("Paper_online_Learning_Dec",
             "data", "trends_dist_edu_world.csv"))
SL_edu <- read.csv(
  here::here("Paper_online_Learning_Dec", 
             "data", "trends_edu_SL.csv"))
SL_dist_edu <- read.csv(
  here::here("Paper_online_Learning_Dec",
             "data", "trends_dist_edu_SL.csv"))

data <- bind_rows(W_edu, W_dist_edu,
                  SL_edu, SL_dist_edu, 
                  .id = "id")
data <- data %>%
  mutate(
    id=factor(id, 
              levels = c(1,2,3,4), 
              labels = c( "Edu All",
                         "Dist. Edu All",
                         "Edu SL", "Dist. Edu SL"))) 
  
data <- data %>% select(date, id, hits)
p <- data %>% 
  ggplot(aes(x=date,y=id,fill=hits))+
  geom_tile() + 
  geom_tile(colour="black",size=0.25)+
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(
    expand=c(0,2),
    breaks=c("2019-12-01", "2020-01-01",
             "2020-02-01","2020-03-01",
             "2020-04-01","2020-05-01",
             "2020-06-01","2020-07-01",
             "2020-08-01","2020-09-01",
             "2020-10-01", "2020-11-01",
             "2020-12-01", "2021-01-01", 
             "2021-02-01","2021-03-01",
             "2021-04-01","2021-05-01",
             "2021-06-01","2021-07-01",
             "2021-08-01"))+
  theme_grey(base_size=8)

p  

# World map for education world
# World map for distance Learning world
# SL map for education SL
# SL map for distance Learning  SL

## ---- extract_info

#category <- categories[categories$name=="Education",2] %>% as.numeric()
country <- c('LK') 
#set channels 
channel <- 'web'

get_info_world <- function(x)
{
  #trends = gtrends(x, gprop =channel, time = time, category = category )
  trends = gtrends(x, gprop =channel,
                   time = time )
  return(trends$interest_over_time)
}

get_info_SL <- function(x)
{
  #trends = gtrends(x, gprop =channel,geo=country, time = time, category = category )
  trends = gtrends(x, gprop =channel,
                   geo=country, time = time )
   return(trends$interest_over_time)
  

}


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
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "Digital_lms_world.csv"))



# extract data for Sri Lanka
keywords1 <- c("Google Classroom", "CenturyTech", 
               "ClassDojo", "Edmodo", "Edraak" )
trends1 <-  gtrends(keywords1, 
                    gprop =channel,geo=country, 
                    time = time  )
trends1 <- trends1$interest_over_time
keywords2 <- c("Google Classroom", "EkStep",
               "Moodle", "Nafham", "Paper Airplanes")
trends2 <-  gtrends(keywords2,
                    gprop =channel,geo=country,
                    time = time  )
trends2 <- trends2$interest_over_time
keywords3 <- c("Google Classroom", "Schoology",
               "Seesaw", "Skooler")
trends3 <-  gtrends(keywords3, 
                    gprop =channel,geo=country,
                    time = time  )
trends3 <- trends3$interest_over_time

trends21 <-trends2 %>%  
  filter(keyword != "Google Classroom")
trends31 <-trends3 %>%  
  filter(keyword != "Google Classroom")

#data_SL<- purrr::map_df(keywords, get_info_SL)
data_SL <- bind_rows(trends1, trends21, trends31)
write.csv(data_SL, 
          here::here("Paper_online_Learning_Dec",
                     "data", "Digital_lms_SL.csv"))


## ---- Digital_lms_analysis
keywords= c("CenturyTech", "ClassDojo", "Edmodo", "Edraak",
            "EkStep", "Google Classroom", "Moodle", "Nafham",
            "Paper Airplanes", "Schoology", "Seesaw", "Skooler")
#data_world <- purrr::map_df(keywords, get_info_world)

data_world <- read.csv( here::here("Paper_online_Learning_Dec",
                                   "data", "Digital_lms_world.csv"))

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
  

data_SL <- read.csv(here::here("Paper_online_Learning_Dec", 
                               "data", "Digital_lms_SL.csv"))

hits1<- ifelse(data_SL$hits ==  "<1", 0, data_SL$hits)
data_SL$hits <- as.numeric(hits1)
keyword_ord<- data_SL %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()


dataSL <- data_SL %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")

p2 <- dataSL %>% 
  ggplot(aes(x=date,y=keyword,fill=hits))+
  geom_tile(size=0.4, colour = "grey50")+
  scale_y_discrete(expand=c(0,0))+
  ggtitle("Sri Lanka")+
  labs(x="",y="")+
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))
  

#p<- gridExtra::grid.arrange(p1,p2, ncol=1)
p<- ggarrange(p1,p2, ncol=1,
              common.legend = TRUE, 
              legend="bottom", align = "hv")
print(p)




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
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "mobile_phones_apps_world.csv"))



# extract data for Sri Lanka
# KaiOS is the maximum category in Sri Lanka by 15-08-2021
keywords1 <- c("KaiOS", "Cell-Ed", "Eneza Education", "Funzi", "Ubongo")
trends1 <-  gtrends(keywords1, gprop =channel,geo=country, time = time  )
trends1 <- trends1$interest_over_time
keywords2 <- c("KaiOS", "Ustad Mobile")
trends2 <-  gtrends(keywords2, gprop =channel,geo=country, time = time  )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "KaiOS")
#data_SL<- purrr::map_df(keywords, get_info_SL)
data_SL <- bind_rows(trends1, trends21)
#data_SL<- purrr::map_df(keywords, get_info_SL)
write.csv(data_SL, 
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "mobile_phones_apps_SL.csv"))




## ---- mobile_phones_apps_analysis
keywords= c("Cell-Ed", "Eneza Education", "Funzi", "KaiOS", "Ubongo", 
            "Ustad Mobile")
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "mobile_phones_apps_world.csv"))


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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "mobile_phones_apps_SL.csv"))

#if(is_empty(data_SL))
#{
#  print(p1)
#} else{
  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")
p2 <- dataSL %>% 
  ggplot(aes(x=date,y=keyword,fill=hits))+
  geom_tile(size=0.4, colour = "grey50")+
  scale_y_discrete(expand=c(0,0))+
  ggtitle("Sri Lanka")+
  labs(x="",y="")+
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))


#p<- gridExtra::grid.arrange(p1,p2, ncol=1)
p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv" )
print(p)
#}


## ---- offline_functionality_data_download
keywords <-  c("Kolibri", "Rumie", "Ustad Mobile")
trends <-  gtrends(keywords, gprop =channel, 
                   time = time )
trends <- trends$interest_over_time
data_world <- trends
#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_world,
          here::here("Paper_online_Learning_Dec",
                     "data", 
                     "offline_functionality_world.csv"))

# Extract data from Sri Lanka
trends <-  gtrends(keywords, gprop =channel,
                   geo=country, time = time  )
trends <- trends$interest_over_time
data_SL <- trends
#data_SL<- purrr::map_df(keywords, get_info_SL)
write.csv(data_SL,
          here::here("Paper_online_Learning_Dec", 
                     "data",
                     "offline_functionality_SL.csv"))


## ---- offline_functionality_analysis
keywords <-  c("Kolibri", "Rumie", "Ustad Mobile")
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "offline_functionality_world.csv"))
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
p1 <- dataW %>% 
  ggplot(aes(x=date,y=keyword,fill=hits))+
  geom_tile(size=0.1, colour = "grey50")+
  scale_y_discrete(expand=c(0,0))+
  ggtitle("All")+
  labs(x="",y="")+
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



#data_SL <- read.csv(here::here("Paper_online_Learning", "data", "offline_functionality_SL.csv"))

data_SL <- read_csv(
  here::here("Paper_online_Learning_Dec", "data", "offline_functionality_SL.csv"))

if(nrow(data_SL) == 0)
{
  print(p1)
} else{
  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  p2 <- dataSL %>% 
    ggplot(aes(x=date,y=keyword,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    scale_x_discrete(expand=c(0,0),
                     breaks=c("2019-12-01","2019-12-01", "2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01"))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
  print(p)
}



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
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "mooc_world.csv"))


# extract data for Sri Lanka
## Canvas is the maximum search category in Sri Lanka by 15-08-2021
keywords1 <-c("Canvas", "Alison", "Coursera", "European Schoolnet Academy", "EdX")
trends1 <-  gtrends(keywords1, gprop =channel,geo=country, time = time  )
trends1 <- trends1$interest_over_time
keywords2 <- c("Canvas", "iCourse", "Future Learn", "Icourses", "TED-Ed Earth School")
trends2 <-  gtrends(keywords2, gprop =channel,geo=country, time = time  )
trends2 <- trends2$interest_over_time
keywords3 <-  c("Canvas", "Udemy", "XuetangX")
trends3 <-  gtrends(keywords3, gprop =channel,geo=country, time = time )
trends3 <- trends3$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Canvas")
trends31 <-trends3 %>%  filter(keyword != "Canvas")
#data_SL<- purrr::map_df(keywords, get_info_SL)
data_SL <- bind_rows(trends1, trends21, trends31)
#data_SL<- purrr::map_df(keywords, get_info_SL)
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "mooc_SL.csv"))




## ---- mooc_analysis
keywords= c("Alison", "Canvas", "Coursera", "European Schoolnet Academy", 
            "EdX", "iCourse", "Future Learn", "Icourses", "TED-Ed Earth School",
            "Udemy", "XuetangX")
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "mooc_world.csv"))

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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "mooc_SL.csv"))

  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")
  p2 <- dataSL %>% 
    ggplot(aes(x=date,y=keyword,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
  print(p)



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
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "Self_directed_learning_world.csv"))

keywords <- c("Quizlet", "YouTube")
trends <-  gtrends(keywords, gprop =channel, time = time )
youtube_world <- trends$interest_over_time
write.csv(youtube_world, here::here("Paper_online_Learning_Dec", "data", "youtube_world.csv"))


######################33
# Extract data for Sri Lanka  
# British Council highest as of 15-08-2021
keywords1 <- c("Quizlet", "ABRA", "British Council", "Byju’s", "Code It")
trends1 <-  gtrends(keywords1, gprop =channel,geo=country, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("British Council", "Code.org", "Code Week", "Discovery Education", "Duolingo")
trends2 <-  gtrends(keywords2, gprop =channel,geo=country, time = time )
trends2 <- trends2$interest_over_time
keywords3 <- c("British Council", "Edraak", "Facebook Get Digital", "Feed the Monster",
               "History of Africa")
trends3 <-  gtrends(keywords3, gprop =channel,geo=country, time = time )
trends3 <- trends3$interest_over_time
keywords4 <- c("British Council", "Geekie", "Khan Academy", "KitKit School", "LabXchange")
trends4 <-  gtrends(keywords4, gprop =channel,geo=country, time = time )
trends4 <- trends4$interest_over_time
keywords5 <- c("British Council", "Madrasa", "Mindspark", "Mosoteach", "Music Crab")
trends5 <-  gtrends(keywords5, gprop =channel,geo=country, time = time )
trends5 <- trends5$interest_over_time
keywords6 <- c("British Councilt","OneCourse", "Polyup", "Quizlet", "SDG Academy Library")
trends6 <-  gtrends(keywords6, gprop =channel, geo=country,time = time )
trends6 <- trends6$interest_over_time
keywords7 <- c("British Council", "Siyavula", "Smart History")
trends7 <-  gtrends(keywords7, gprop =channel,geo=country, time = time )
trends7 <- trends7$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "British Council")
trends31 <-trends3 %>%  filter(keyword != "British Council")
trends41 <-trends2 %>%  filter(keyword != "British Council")
trends51 <-trends3 %>%  filter(keyword != "British Council")
trends61 <-trends2 %>%  filter(keyword != "British Council")
trends71 <-trends3 %>%  filter(keyword != "British Council")
data_SL <- bind_rows(trends1, trends21, trends31, trends41, trends51, trends61, trends71)

#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "Self_directed_learning_SL.csv"))

keywords <- c("British Council", "YouTube")
trends <-  gtrends(keywords, gprop =channel,geo=country, time = time )
youtube_SL <- trends$interest_over_time
write.csv(youtube_SL, here::here("Paper_online_Learning_Dec", "data", "youtube_SL.csv"))



## ---- Self_directed_learning_analysis
keywords= c("ABRA", "British Council", "Byju’s", "Code It", 
            "Code.org", "Code Week", "Discovery Education", "Duolingo",
            "Edraak", "Facebook Get Digital", "Feed the Monster",
            "History of Africa", "Geekie", "Khan Academy",
            "KitKit School", "LabXchange", "Madrasa",
            "Mindspark", "Mosoteach", "Music Crab", "OneCourse",
            "Polyup", "Quizlet", "SDG Academy Library", "Siyavula",
            "Smart History", "youtube")
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "Self_directed_learning_world.csv"))

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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "Self_directed_learning_SL.csv"))

  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")
    p2 <- dataSL %>% 
    ggplot(aes(x=date,y=keyword,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
  print(p)

## Note: Add youtube analysis as well

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

## download SL data
# Reads is the  highest as of 15-08-2021
keywords1 <- c("African Storybook", "Global Digital Library", "Reads", 
               "Room to Read", "StoryWeaver")
trends1 <-  gtrends(keywords1, gprop =channel,geo=country, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("Reads", "Worldreader")
trends2 <-  gtrends(keywords2, gprop =channel, geo=country, time = time )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Reads")
data_SL <- bind_rows(trends1, trends21)
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "Mobile_reading_SL.csv"))



## ---- Mobile_reading_analysis
keywords= c("African Storybook", "Global Digital Library", "Reads", 
            "Room to Read", "StoryWeaver", "Worldreader")

data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "Mobile_reading_world.csv"))

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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "Mobile_reading_SL.csv"))

  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")
  p2 <- dataSL %>% 
    ggplot(aes(x=date,y=keyword,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
  print(p)





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
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "Collaboration_platforms_world.csv"))


#### sri lanka data

keywords= c("Zoom", "Dingtalk", "Lark", "Hangouts Meet",
            "Teams", "Skype", "WhatsApp")

# WhatsApp is the  highest search in Sri Lanka as of 15-08-2021
keywords1 <- c("Zoom", "Dingtalk", "Lark", "Hangouts Meet", "WhatsApp")
trends1 <-  gtrends(keywords1, gprop =channel,geo=country, time = time )
trends1 <- trends1$interest_over_time
keywords2 <- c("Teams", "Skype", "WhatsApp")
trends2 <-  gtrends(keywords2, gprop =channel, geo=country,time = time )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "WhatsApp")
data_SL <- bind_rows(trends1, trends21)
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "Collaboration_platforms_SL.csv"))



## ---- Collaboration_platforms_analysis
keywords= c("Zoom", "Dingtalk", "Lark", "Hangouts Meet",
            "Teams", "Skype", "WhatsApp")
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "Collaboration_platforms_world.csv"))

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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "Collaboration_platforms_SL.csv"))
hits1<- ifelse(data_SL$hits ==  "<1", 0, data_SL$hits)
data_SL$hits <- as.numeric(hits1)

  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")
  p2 <- dataSL %>% 
    ggplot(aes(x=date,y=keyword,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
  print(p)





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


### SL data

# WhatsApp is the  highest search in education category in Sri Lanka as of 18-01-2021
keywords1 <- c("Zoom", "Dingtalk", "Lark", "Hangouts Meet", "WhatsApp")
trends1 <-  gtrends(keywords1, gprop =channel,geo=country, time = time,  category = category )
trends1 <- trends1$interest_over_time
keywords2 <- c("Teams", "Skype", "WhatsApp")
trends2 <-  gtrends(keywords2, gprop =channel, geo=country,time = time,  category = category )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "WhatsApp")
data_SL <- bind_rows(trends1, trends21)
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "edu_only_Collaboration_platforms_SL.csv"))



## ---- Collaboration_platforms_eduonly_analysis

data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "edu_only_Collaboration_platforms_world.csv"))

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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "edu_only_Collaboration_platforms_SL.csv"))

  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  
  dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")
  p2 <- dataSL %>% 
    ggplot(aes(x=date,y=keyword,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
  print(p)





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
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "Tools_for_teachers_world.csv"))

### SL data

# Trello is the  highest search in Sri Lanka as of 15-08-2021
keywords1 <- c("Trello", "Thinglink", "Buncee", "EdPuzzle", "EduCaixa")
trends1 <-  gtrends(keywords1, gprop =channel, geo=country,time = time)
trends1 <- trends1$interest_over_time
keywords2 <- c("Kaltura", "Nearpod", "Pear Deck", "Squigl", "Trello")
trends2 <-  gtrends(keywords2, gprop =channel,geo=country, time = time )
trends2 <- trends2$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Trello")

data_SL <- bind_rows(trends1, trends21)
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "Tools_for_teachers_SL.csv"))

## ---- Tools_for_teachers_analysis

data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "Tools_for_teachers_world.csv"))
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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "Tools_for_teachers_SL.csv"))

  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")
  p2 <- dataSL %>% 
    ggplot(aes(x=date,y=keyword,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom",  align = "hv")
  print(p)





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
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "External_repositories_DL_world.csv"))

### Sri Lanka data
# UNHCR is the  highest search  as of 18-01-2021
keywords1 <- c("UNHCR", "Common Sense Education", "Commonweatlh of Learning",
               "Education Nation", "EdSurge")
trends1 <-  gtrends(keywords1, gprop =channel,geo=country, time = time)
trends1 <- trends1$interest_over_time
keywords2 <- c("UNHCR","Global Business Coalition for Education",
               "Keep Learning Going", "Koulu.me", "Organisation internationale de la Francophonie")
trends2 <-  gtrends(keywords2, gprop =channel,geo=country, time = time )
trends2 <- trends2$interest_over_time
keywords3 <- c("Brookings", "UNEVOC Resources", "UNHCR")
trends3 <-  gtrends(keywords3, gprop =channel,geo=country, time = time )
trends3 <- trends3$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "UNHCR")
trends31 <-trends3 %>%  filter(keyword != "UNHCR")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)
trends31$hits <- as.numeric( trends31$hits)
data_SL <- bind_rows(trends1, trends21, trends31)
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "External_repositories_DL_SL.csv"))



## ---- External_repositories_DL_analysis
keywords= c("Brookings", "Common Sense Education", "Commonweatlh of Learning",
            "Education Nation", "EdSurge", "Global Business Coalition for Education",
            "Keep Learning Going", "Koulu.me", "Organisation internationale de la Francophonie",
            "UNEVOC Resources", "UNHCR")

data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "External_repositories_DL_world.csv"))

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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "External_repositories_DL_SL.csv"))

  keyword_ord<- data_SL %>% 
    group_by(keyword) %>%
    summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
    arrange(total_hits) %>% select(keyword) %>% as_vector()
  
  
  dataSL <- data_SL %>%
    select(date, keyword, hits) %>%
    mutate(keyword = factor(keyword, levels = keyword_ord))
  dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")
  
  p2 <- dataSL %>% 
    ggplot(aes(x=date,y=keyword,fill=hits))+
    geom_tile(size=0.4, colour = "grey50")+
    scale_y_discrete(expand=c(0,0))+
    ggtitle("Sri Lanka")+
    labs(x="",y="")+
    scale_fill_viridis()+
    theme(panel.grid = element_blank(), text = element_text(size = 16))
  
  
  #p<- gridExtra::grid.arrange(p1,p2, ncol=1)
  p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
  print(p)



## ---- psychosocial_support_data_download

keywords <- c("psychosocial support")

trends <- gtrends(keywords, gprop =channel, time = time)
data_world_all <- trends$interest_over_time
category <- categories[categories$name=="Education",2] %>% as.numeric()
trends <- gtrends(keywords, gprop =channel, time = time, category = category )
data_world_edu <- trends$interest_over_time
data_world <- bind_rows(data_world_all, data_world_edu )
write.csv(data_world, here::here("Paper_online_Learning_Dec", "data", "psychosocial_support_world.csv"))



trends <- gtrends(keywords, gprop =channel, time = time, geo=country)
data_SL_all <- trends$interest_over_time
category <- categories[categories$name=="Education",2] %>% as.numeric()
trends <- gtrends(keywords, gprop =channel, time = time, category = category ,geo=country)
data_SL_edu <- trends$interest_over_time
data_SL <- bind_rows(data_SL_all, data_SL_edu )
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "psychosocial_support_SL.csv"))



## ---- psychosocial_support_analysis
keywords <- c("psychosocial support")
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "psychosocial_support_world.csv"))

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


### SL data
# Pearson VUE is the  highest search  in Sri Lanka as of 15-08-2021
keywords1 <- c("Pearson VUE", "Mettl", "Examus", "ProctorU", 
               "Examity")
trends1 <-  gtrends(keywords1, gprop =channel,geo=country, time = time)
trends1 <- trends1$interest_over_time
keywords2 <- c("Pearson VUE","Verificient", "AIProctor",
               "ExamSoft", "Proview")
trends2 <-  gtrends(keywords2, gprop =channel,geo=country, time = time )
trends2 <- trends2$interest_over_time
keywords3 <- c("Pearson VUE", "Conduct Exam", "ProctorExam", 
               "PSI Bridge", "MeritTrac")
trends3 <-  gtrends(keywords3, gprop =channel,geo=country, time = time )
trends3 <- trends3$interest_over_time
keywords4 <- c("Pearson VUE", "Honorlock", "Proctortrack", "Proctorio",
               "Talview")
trends4 <-  gtrends(keywords4, gprop =channel,geo=country, time = time )
trends4 <- trends4$interest_over_time
trends21 <-trends2 %>%  filter(keyword != "Pearson VUE")
trends31 <-trends3 %>%  filter(keyword != "Pearson VUE")
trends41 <-trends4 %>%  filter(keyword != "Pearson VUE")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)
trends31$hits <- as.numeric( trends31$hits)
trends41$hits <- as.numeric( trends41$hits)
data_SL <- bind_rows(trends1, trends21, trends31, trends41)
write.csv(data_SL, here::here("Paper_online_Learning_Dec", "data", "Online_proctoring_SL.csv"))


## ---- Online_proctoring_analysis
keywords= c("Mettl", "Examus", "ProctorU", "Examity", "Verificient",
            "AIProctor","ExamSoft", "Proview", "Conduct Exam",
            "ProctorExam", "PSI Bridge", "Pearson VUE", 
            "MeritTrac", "Honorlock", "Proctortrack", "Proctorio",
            "Talview")

data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "Online_proctoring_world.csv"))

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
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))



data_SL <- read.csv(here::here("Paper_online_Learning_Dec", "data", "Online_proctoring_SL.csv"))

keyword_ord<- data_SL %>% 
  group_by(keyword) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  arrange(total_hits) %>% select(keyword) %>% as_vector()


dataSL <- data_SL %>%
  select(date, keyword, hits) %>%
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataSL$date = as.Date(dataSL$date, "%Y-%m-%d")

p2 <- dataSL %>% 
  ggplot(aes(x=date,y=keyword,fill=hits))+
  geom_tile(size=0.4, colour = "grey50")+
  scale_y_discrete(expand=c(0,0))+
  ggtitle("Sri Lanka")+
  labs(x="",y="")+
  scale_fill_viridis()+
  theme(panel.grid = element_blank(), text = element_text(size = 16))


#p<- gridExtra::grid.arrange(p1,p2, ncol=1)
p<- ggarrange(p1,p2, ncol=1, common.legend = TRUE, legend="bottom", align = "hv")
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


## ---- Synchrony_between_ts
# Covid cases
covidcasesW <- read.csv(here::here("Paper_online_Learning_Dec", "data", "covid_cases.csv"))
covidcasesW <- covidcasesW %>% select(-X)

## covid search google trends
data_covid <- read.csv( here::here("Paper_online_Learning_Dec", "data", "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)
# covid search google trends - arrange in wide format
covid_search <- data_covid %>%
  select(date, hits, keyword) %>%
  pivot_wider(names_from = keyword, values_from = hits)

## online learning google trends
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "education_search_world.csv"))
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
  labs(title= "corona with online learning")

p2 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_dist = difference(`Distance learning`)) %>%
  CCF(diff_corona, diff_dist ) %>%
  autoplot()+
  labs(title= "corona with Distance learning")

p3 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_teach = difference(`Online teaching`)) %>%
  CCF(diff_corona, diff_teach ) %>%
  autoplot()+
  labs(title= "corona with Online teaching")

p4 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_edu = difference(`Distance education`)) %>%
  CCF(diff_corona, diff_edu ) %>%
  autoplot()+
  labs(title= "corona with Distance education")

p5 <- data %>% 
  mutate(diff_corona = difference(corona),
         diff_proc = difference(`online proctoring`)) %>%
  CCF(diff_corona, diff_proc ) %>%
  autoplot()+
  labs(title= "corona with online proctoring")



pa <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_online = difference(`Online Learning`)) %>%
  CCF(diff_corona, diff_online ) %>%
  autoplot()+
  labs(title= "covid 19 with online learning")

pb <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_dist = difference(`Distance learning`)) %>%
  CCF(diff_corona, diff_dist ) %>%
  autoplot()+
  labs(title= "covid 19 with Distance learning")

pc <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_teach = difference(`Online teaching`)) %>%
  CCF(diff_corona, diff_teach ) %>%
  autoplot()+
  labs(title= "covid 19 with Online teaching")

pd <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_edu = difference(`Distance education`)) %>%
  CCF(diff_corona, diff_edu ) %>%
  autoplot()+
  labs(title= "covid 19 with Distance education")

pe <- data %>% 
  mutate(diff_corona = difference(`covid 19`),
         diff_proc = difference(`online proctoring`)) %>%
  CCF(diff_corona, diff_proc ) %>%
  autoplot()+
  labs(title= "covid 19 with online proctoring")


(p1 | pa ) / 
  (p2 | pb )/
  (p3 | pc )/
  (p4 | pd )/
  (p5 | pe )

## ---- dtw

# Covid cases
covidcasesW <- read.csv(here::here("Paper_online_Learning_Dec", "data", "covid_cases.csv"))
covidcasesW <- covidcasesW %>% select(-X)

## covid search google trends
data_covid <- read.csv( here::here("Paper_online_Learning_Dec", "data", "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)
# covid search google trends - arrange in wide format
covid_search <- data_covid %>%
  select(date, hits, keyword) %>%
  pivot_wider(names_from = keyword, values_from = hits)

## online learning google trends
data_world <- read.csv( here::here("Paper_online_Learning_Dec", "data", "education_search_world.csv"))
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

library(dtw)
x<- scale(data$`Online Learning`)
y<- scale(data$corona)
#align <- dtw(data$`Online Learning`, data$corona, keep = T)
align <- dtw(x[,1], y[,1], keep = T)
#align <- dtw( data$corona, (data$corona + rnorm(163)), keep = T)
p1 <- dtwPlotTwoWay(align)
p2 <- dtwPlotDensity(align, normalize = TRUE)
p3<- dtwPlotThreeWay(align)
#align$distance


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