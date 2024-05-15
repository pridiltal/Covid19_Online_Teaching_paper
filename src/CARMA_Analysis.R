# CARMA Analysis
### Supplemental Materials (Data and scripts)
###  Distance Learning Solutions with COVID-19 Pandemic
### Updated the analysis on 6/5/2024 for CARMA conference

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
time <- ("2019-12-01 2024-04-30")

## ---- distance_learning_world
## Offline data retrieval
keywords= c("Online Learning", "Online teaching",
            "Distance learning","Distance education",
            "online proctoring")
#data_world <- purrr::map_df(keywords, get_info_world)
category <- categories[
  categories$name=="Education",2] |> 
  as.numeric()
#set channels 
channel <- 'web'
trends <-  gtrends(keywords,
                   gprop =channel,
                   time = "2017-12-01 2024-04-30" )
trends <- trends$interest_over_time
write.csv(trends, here::here(
  "dataaftercovid", "education_search_world.csv"))



## ---- covid19_trend_world
## Offline data retrieval
keywords= c("covid 19", "corona", "covid")
#set channels 
channel <- 'web'
trends <-  gtrends(keywords, gprop =channel,
                   time = "2017-12-01 2024-04-30" )
trends <- trends$interest_over_time
write.csv(trends, here::here( "dataaftercovid",
                              "covid_search_world.csv"))


## ---- covid_epidemic_datasave
# Data retrieved from coronavirus R package on 19-01-2021
library(coronavirus)
data("coronavirus")

covid_cases <- coronavirus |> 
  group_by(type, date) |>
  summarise(total_cases = sum(cases)) |>
  pivot_wider(names_from = type, 
              values_from = total_cases) |>
  arrange(date) 
#mutate(active = confirmed - death - recovered)

covid_cases <- covid_cases |> as_tsibble(index = date)
write.csv(covid_cases, here::here(
  "dataaftercovid", "covid_cases.csv"))


## ---- distanceLearningWorldAnalysis
# covid actual cases
covidcasesW <- read.csv(here::here( "dataaftercovid",
                                    "covid_cases.csv"))
covidcasesW <- covidcasesW |> select(-X)

covidcasesW1 <- covidcasesW |> pivot_longer(
  cols = confirmed : recovery,
  names_to = "Type",
  values_to = "Counts") |>
  mutate(Type = factor(Type, levels = c("confirmed", "death", "recovery")))|>
  mutate(date = as.Date(date)) |>
  as_tsibble(index = date, key = Type) |>
  mutate(Counts = ifelse(Counts <0, NA, Counts))


p1 <- covidcasesW1 |>  autoplot() +
  xlab("date") + 
  scale_color_brewer(palette="Dark2") +
  scale_x_date(limits = as.Date(c('2017-12-03','2024-04-30'))) +
  theme_bw()+
  theme(legend.position = "bottom", text = element_text(size = 14)) +
  ggtitle("(a)")+
  xlab("")



## Covid search
data_covid <- read.csv( here::here( "dataaftercovid",
                                    "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)

keyword_ord<- data_covid |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_covid |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")

p3<- dataW |>  ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) + 
  scale_color_brewer(palette="Dark2")  +
  scale_x_date(limits = as.Date(c('2017-12-03','2024-04-30'))) +
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14)) +
  ggtitle("(b)")+
  xlab("")


## covid google trends
data_world <- read.csv( here::here( "dataaftercovid", 
                                    "education_search_world.csv"))

# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")


p5<- dataW|> ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) +
  scale_color_brewer(palette="Dark2")  +
  scale_x_date(limits = as.Date(c('2017-12-03','2024-04-30'))) +
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))+
  guides(colour = guide_legend(nrow = 2, title="keywords"))+
  ggtitle("(c)")

p <- p1/p3/p5

print(p)


############ Google trend footprint

############### Updated after this for CARMA
#set the time window
time <- ("2019-12-01 2024-04-30")
channel <- 'web'
category <- categories[
  categories$name=="Education",2] |> 
  as.numeric()
## ---- Digital_lms_data_download

keywords= c("CenturyTech", "ClassDojo", "Edmodo", "Edraak",
            "EkStep", "Google Classroom", "Moodle", "Nafham",
            "Paper Airplanes", "Schoology", "Seesaw", "Skooler")

# Google Classroom is the maximum category by 30-04-2024
keywords1 <- c("Google Classroom",
               "CenturyTech", "ClassDojo", 
               "Edmodo", "Edraak" )
trends1 <-  gtrends(keywords1, 
                    gprop =channel, time = time, 
                    category = category)
trends1 <- trends1$interest_over_time
keywords2 <- c("Google Classroom", "EkStep",
               "Moodle", "Nafham", 
               "Paper Airplanes")
trends2 <-  gtrends(keywords2, 
                    gprop =channel, time = time,  category = category )
trends2 <- trends2$interest_over_time
keywords3 <- c("Google Classroom", "Schoology", 
               "Seesaw", "Skooler")
trends3 <-  gtrends(keywords3, gprop =channel,
                    time = time,  category = category )
trends3 <- trends3$interest_over_time

trends21 <-trends2 |> 
  filter(keyword != "Google Classroom")
trends31 <-trends3 |> 
  filter(keyword != "Google Classroom")

data_world <- bind_rows(trends1, trends21, 
                        trends31)
write.csv(data_world,
          here::here("dataaftercovid", 
                     "Digital_lms_world.csv"))


## ---- DigitalLmsAnalysis
fontsize = 12
keywords= c("CenturyTech", "ClassDojo", "Edmodo", "Edraak",
            "EkStep", "Google Classroom", "Moodle", "Nafham",
            "Paper Airplanes", "Schoology", "Seesaw", "Skooler")

data_world <- read.csv( here::here("dataaftercovid", "Digital_lms_world.csv"))

hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")


p1 <- dataW |>
  as_tsibble(index= date, key = keyword) |>
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

# Ubongo is the maximum category by 30-04-2024
keywords1 <- c("Ubongo", "Cell-Ed", "Eneza Education", "Funzi")
trends1 <-  gtrends(keywords1, gprop =channel, time = time , category = category)
trends1 <- trends1$interest_over_time
keywords2 <- c("Ubongo", "Ustad Mobile", "KaiOS")
trends2 <-  gtrends(keywords2, gprop =channel, 
                    time = time , category = category)
trends2 <- trends2$interest_over_time
trends21 <-trends2 |>  filter(keyword != "Ubongo")
data_world <- bind_rows(trends1, trends21)

write.csv(data_world, 
          here::here("dataaftercovid", 
                     "mobile_phones_apps_world.csv"))




## ---- mobile_phones_apps_analysis
keywords= c("Cell-Ed", "Eneza Education", "Funzi", 
            "KaiOS", "Ubongo", "Ustad Mobile")
data_world <- read.csv( here::here("dataaftercovid", "mobile_phones_apps_world.csv"))

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> 
  select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")

p2<- dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |>
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom", 
        text = element_text(size = fontsize),
        legend.title = element_blank(),
        legend.text = element_text(size = fontsize))+
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(b) Systems built for use on basic mobile phones
")


## ---- offline_functionality_data_download
keywords <-  c("Kolibri", "Rumie", "Ustad Mobile")
trends <-  gtrends(keywords, gprop =channel, 
                   time = time , category = category)
trends <- trends$interest_over_time
data_world <- trends
#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_world,
          here::here("dataaftercovid", 
                     "offline_functionality_world.csv"))



## ---- offline_functionality_analysis
keywords <-  c("Kolibri", "Rumie", "Ustad Mobile")
data_world <- read.csv( here::here( "dataaftercovid", "offline_functionality_world.csv"))
hits1<- ifelse(
  data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)
keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")


p3<- dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |> 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom", 
        text = element_text(size = fontsize),
        legend.title = element_blank(), 
        legend.text = element_text(size = fontsize))+
  # scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(c) Systems with strong offline functionality")



## ---- mooc_data_download
keywords= c("Alison", "Canvas", "Coursera", "European Schoolnet Academy", 
            "EdX", "iCourse", "Future Learn", "Icourses", "TED-Ed Earth School",
            "Udemy", "XuetangX")

## Canvas is the maximum search category by 30-04-2024

keywords1 <- c("Canvas", "Alison", "Coursera", "European Schoolnet Academy", "EdX")
trends1 <-  gtrends(keywords1, gprop =channel, time = time, category = category )
trends1 <- trends1$interest_over_time
keywords2 <- c("Canvas", "iCourse", "Future Learn", "Icourses", "TED-Ed Earth School")
trends2 <-  gtrends(keywords2, gprop =channel, time = time , category = category)
trends2 <- trends2$interest_over_time
keywords3 <- c("Canvas", "Udemy", "XuetangX")
trends3 <-  gtrends(keywords3, gprop =channel, time = time, category = category )
trends3 <- trends3$interest_over_time
trends21 <-trends2 |>  filter(keyword != "Canvas")
trends31 <-trends3 |>  filter(keyword != "Canvas")
data_world <- bind_rows(trends1, trends21, trends31)
#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_world, here::here( "dataaftercovid", "mooc_world.csv"))


## ---- mooc_analysis
keywords= c("Alison", "Canvas", "Coursera", "European Schoolnet Academy", 
            "EdX", "iCourse", "Future Learn", "Icourses", "TED-Ed Earth School",
            "Udemy", "XuetangX")
data_world <- read.csv( here::here( "dataaftercovid", "mooc_world.csv"))

hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p4 <- dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |> 
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


# Quizlet  highest as of 30-04-2024
keywords1 <- c("Quizlet", "ABRA", "British Council", "Byju’s", "Code It")
trends1 <-  gtrends(keywords1, gprop =channel, time = time, category = category )
trends1 <- trends1$interest_over_time
keywords2 <- c("Quizlet", "Code.org", "Code Week", "Discovery Education", "Duolingo")
trends2 <-  gtrends(keywords2, gprop =channel, time = time, category = category )
trends2 <- trends2$interest_over_time
keywords3 <- c("Quizlet", "Edraak", "Facebook Get Digital", "Feed the Monster",
               "History of Africa")
trends3 <-  gtrends(keywords3, gprop =channel, time = time, category = category )
trends3 <- trends3$interest_over_time
keywords4 <- c("Quizlet", "Geekie", "Khan Academy", "KitKit School", "LabXchange")
trends4 <-  gtrends(keywords4, gprop =channel, time = time, category = category )
trends4 <- trends4$interest_over_time
keywords5 <- c("Quizlet", "Madrasa", "Mindspark", "Mosoteach", "Music Crab")
trends5 <-  gtrends(keywords5, gprop =channel, time = time, category = category )
trends5 <- trends5$interest_over_time
keywords6 <- c("Quizlet","OneCourse", "Polyup", "Quizlet", "SDG Academy Library")
trends6 <-  gtrends(keywords6, gprop =channel, time = time, category = category )
trends6 <- trends6$interest_over_time
keywords7 <- c("Quizlet", "Siyavula", "Smart History")
trends7 <-  gtrends(keywords7, gprop =channel, time = time, category = category )
trends7 <- trends7$interest_over_time
trends21 <-trends2 |>  filter(keyword != "Quizlet")
trends31 <-trends3 |>  filter(keyword != "Quizlet")
trends41 <-trends2 |>  filter(keyword != "Quizlet")
trends51 <-trends3 |>  filter(keyword != "Quizlet")
trends61 <-trends2 |>  filter(keyword != "Quizlet")
trends71 <-trends3 |>  filter(keyword != "Quizlet")
data_world <- bind_rows(trends1, trends21, trends31, trends41, trends51, trends61, trends71)

#data_world <- purrr::map_df(keywords, get_info_world)
write.csv(data_world, here::here( "dataaftercovid", "Self_directed_learning_world.csv"))

keywords <- c("Quizlet", "YouTube")
trends <-  gtrends(keywords, gprop =channel, time = time , category = category)
youtube_world <- trends$interest_over_time
write.csv(youtube_world, here::here( "dataaftercovid", "youtube_world.csv"))


## ---- Self_directed_learning_analysis
keywords= c("ABRA", "British Council", "Byju’s", "Code It", 
            "Code.org", "Code Week", "Discovery Education", "Duolingo",
            "Edraak", "Facebook Get Digital", "Feed the Monster",
            "History of Africa", "Geekie", "Khan Academy",
            "KitKit School", "LabXchange", "Madrasa",
            "Mindspark", "Mosoteach", "Music Crab", "OneCourse",
            "Polyup", "Quizlet", "SDG Academy Library", "Siyavula",
            "Smart History", "youtube")
data_world <- read.csv( here::here( "dataaftercovid", "Self_directed_learning_world.csv"))

hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p5 <-  dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |> 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom" ,  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(e) Self-directed learning content")

## ---- Mobile_reading_data_download
keywords= c("African Storybook", "Global Digital Library", "Reads", 
            "Room to Read", "StoryWeaver", "Worldreader")

# Reads is the  highest as of 30-04-2024
keywords1 <- c("African Storybook", "Global Digital Library", "Reads", 
               "Room to Read", "StoryWeaver")
trends1 <-  gtrends(keywords1, gprop =channel, time = time, category = category )
trends1 <- trends1$interest_over_time
keywords2 <- c("Reads", "Worldreader")
trends2 <-  gtrends(keywords2, gprop =channel, time = time , category = category)
trends2 <- trends2$interest_over_time
trends21 <-trends2 |>  filter(keyword != "Reads")
data_world <- bind_rows(trends1, trends21)
write.csv(data_world, here::here( "dataaftercovid", "Mobile_reading_world.csv"))



## ---- Mobile_reading_analysis
keywords= c("African Storybook", "Global Digital Library", "Reads", 
            "Room to Read", "StoryWeaver", "Worldreader")

data_world <- read.csv( here::here( "dataaftercovid", "Mobile_reading_world.csv"))

hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p6 <-  dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |> 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(f) Mobile reading applications")



## ---- Collaboration_platforms_data_download 
## entire world

keywords= c("Zoom", "Dingtalk", "Lark", "Hangouts Meet",
            "Teams", "Skype", "WhatsApp")

# WhatsApp  highest as of 30-04-2024
keywords1 <- c("Zoom", "Dingtalk", "Lark", "Hangouts Meet", "WhatsApp")
trends1 <-  gtrends(keywords1, gprop =channel, time = time , category = category)
trends1 <- trends1$interest_over_time
keywords2 <- c("Teams", "Skype", "WhatsApp")
trends2 <-  gtrends(keywords2, gprop =channel, time = time, category = category )
trends2 <- trends2$interest_over_time
trends21 <-trends2 |>  filter(keyword != "WhatsApp")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)

data_world <- bind_rows(trends1, trends21)
write.csv(data_world, here::here( "dataaftercovid", "Collaboration_platforms_world.csv"))


## ---- Collaboration_platforms_eduonly_data_download 

### This analysis was restricted to education catgory:
#Otherwise hits generates and error

category <- categories[categories$name=="Education",2] |> as.numeric()
keywords= c("Zoom", "Dingtalk", "Lark", "Hangouts Meet",
            "Teams", "Skype", "WhatsApp")

# WhatsApp is the  highest search in education category as of 30-04-2024
keywords1 <- c("Zoom", "Dingtalk", "Lark", "Hangouts Meet", "WhatsApp")
trends1 <-  gtrends(keywords1, gprop =channel, time = time, category = category )
trends1 <- trends1$interest_over_time
keywords2 <- c("Teams", "Skype", "WhatsApp")
trends2 <-  gtrends(keywords2, gprop =channel, time = time, category = category )
trends2 <- trends2$interest_over_time
trends21 <-trends2 |>  filter(keyword != "WhatsApp")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)

data_world <- bind_rows(trends1, trends21)
write.csv(data_world, here::here("dataaftercovid", "edu_only_Collaboration_platforms_world.csv"))




## ---- Collaboration_platforms_eduonly_analysis

data_world <- read.csv( here::here( "dataaftercovid", "edu_only_Collaboration_platforms_world.csv"))

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p7 <-  dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |> 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  text = element_text(size = fontsize), legend.title = element_blank(), legend.text = element_text(size = fontsize))+
  #scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(g) Collaboration platforms that support live-video communication")




## ---- Tools_for_teachers_data_download
keywords= c("Thinglink", "Buncee", "EdPuzzle", "EduCaixa",
            "Kaltura", "Nearpod", "Pear Deck", "Squigl", "Trello")

# Nearpod is the highest search  as of 30-04-2024
keywords1 <- c("Nearpod", "Thinglink", "Buncee", "EdPuzzle", "EduCaixa")
trends1 <-  gtrends(keywords1, gprop =channel, time = time, category = category)
trends1 <- trends1$interest_over_time
keywords2 <- c("Kaltura", "Nearpod", "Pear Deck", "Squigl", "Trello")
trends2 <-  gtrends(keywords2, gprop =channel, time = time, category = category )
trends2 <- trends2$interest_over_time
trends21 <-trends2 |>  filter(keyword != "Nearpod")

data_world <- bind_rows(trends1, trends21)
write.csv(data_world, here::here( "dataaftercovid", "Tools_for_teachers_world.csv"))

## ---- Tools_for_teachers_analysis

data_world <- read.csv( here::here( "dataaftercovid", "Tools_for_teachers_world.csv"))
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p8 <-  dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |> 
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
trends1 <-  gtrends(keywords1, gprop =channel, time = time, category = category)
trends1 <- trends1$interest_over_time
keywords2 <- c("Brookings","Global Business Coalition for Education",
               "Keep Learning Going", "Koulu.me", "Organisation internationale de la Francophonie")
trends2 <-  gtrends(keywords2, gprop =channel, time = time, category = category )
trends2 <- trends2$interest_over_time
keywords3 <- c("Brookings", "UNEVOC Resources", "UNHCR")
trends3 <-  gtrends(keywords3, gprop =channel, time = time, category = category )
trends3 <- trends3$interest_over_time
trends21 <-trends2 |>  filter(keyword != "Brookings")
trends31 <-trends3 |>  filter(keyword != "Brookings")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)
trends31$hits <- as.numeric( trends31$hits)
data_world <- bind_rows(trends1, trends21, trends31)
write.csv(data_world, here::here("dataaftercovid", "External_repositories_DL_world.csv"))


## ---- External_repositories_DL_analysis
keywords= c("Brookings", "Common Sense Education", "Commonweatlh of Learning",
            "Education Nation", "EdSurge", "Global Business Coalition for Education",
            "Keep Learning Going", "Koulu.me", "Organisation internationale de la Francophonie",
            "UNEVOC Resources", "UNHCR")

data_world <- read.csv( here::here( "dataaftercovid", "External_repositories_DL_world.csv"))

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p9 <-  dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |> 
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom", 
        text = element_text(size = fontsize), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 7))+
  # scale_colour_viridis_d(guide = "colourbar", direction = -1) + 
  scale_colour_discrete(guide = "colourbar") + 
  labs(subtitle ="(i) External repositories of distance learning solutions")


## ---- Online_proctoring_data_download
# source : https://www.softwaresuggest.com/blog/best-online-exam-proctoring-software/#7_ExamSoft
# source 2: https://blog.mettl.com/top-5-proctoring-solution-providers/
keywords= c("Mettl", "Examus", "ProctorU", "Examity", "Verificient",
            "AIProctor","ExamSoft", "Proview", "Conduct Exam",
            "ProctorExam", "PSI Bridge", "Pearson VUE", 
            "MeritTrac", "Honorlock", "Proctortrack", "Proctorio",
            "Talview")
# Pearson VUE is the  highest search as of 30-04-2024
keywords1 <- c("Pearson VUE", "Mettl", "Examus", "ProctorU", 
               "Examity")
trends1 <-  gtrends(keywords1, gprop =channel, time = time, category = category)
trends1 <- trends1$interest_over_time
keywords2 <- c("Pearson VUE","Verificient", "AIProctor",
               "ExamSoft", "Proview")
trends2 <-  gtrends(keywords2, gprop =channel, time = time, category = category )
trends2 <- trends2$interest_over_time
keywords3 <- c("Pearson VUE", "Conduct Exam", "ProctorExam", 
               "PSI Bridge", "MeritTrac")
trends3 <-  gtrends(keywords3, gprop =channel, time = time , category = category)
trends3 <- trends3$interest_over_time
keywords4 <- c("Pearson VUE", "Honorlock", "Proctortrack", "Proctorio",
               "Talview")
trends4 <-  gtrends(keywords4, gprop =channel, time = time , category = category)
trends4 <- trends4$interest_over_time
trends21 <-trends2 |>  filter(keyword != "Pearson VUE")
trends31 <-trends3 |>  filter(keyword != "Pearson VUE")
trends41 <-trends4 |>  filter(keyword != "Pearson VUE")
trends1$hits <- as.numeric( trends1$hits)
trends21$hits <- as.numeric( trends21$hits)
trends31$hits <- as.numeric( trends31$hits)
trends41$hits <- as.numeric( trends41$hits)
data_world <- bind_rows(trends1, trends21, trends31, trends41)
write.csv(data_world, here::here("dataaftercovid", "Online_proctoring_world.csv"))



## ---- Online_proctoring_analysis
keywords= c("Mettl", "Examus", "ProctorU", "Examity", "Verificient",
            "AIProctor","ExamSoft", "Proview", "Conduct Exam",
            "ProctorExam", "PSI Bridge", "Pearson VUE", 
            "MeritTrac", "Honorlock", "Proctortrack", "Proctorio",
            "Talview")

data_world <- read.csv( here::here( "dataaftercovid", "Online_proctoring_world.csv"))

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")
p10 <-  dataW |> as_tibble() |>
  distinct(date, keyword, .keep_all = TRUE) |>
  as_tsibble(index= date, key = keyword) |>
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



## ---- psychosocial_support_data_download

keywords <- c("psychosocial support")

trends <- gtrends(keywords, gprop =channel, time = time)
data_world_all <- trends$interest_over_time
category <- categories[categories$name=="Education",2] |> as.numeric()
trends <- gtrends(keywords, gprop =channel, time = time, category = category )
data_world_edu <- trends$interest_over_time
data_world <- bind_rows(data_world_all, data_world_edu )
write.csv(data_world, here::here( "dataaftercovid", "psychosocial_support_world.csv"))


## ---- psychosocialSupport
keywords <- c("psychosocial support")
data_world <- read.csv( here::here( "dataaftercovid", "psychosocial_support_world.csv"))

dataW <- data_world |>
  select(date, category, hits) |>
  mutate(category = factor(category, levels = c(0, 74), labels = c("All", "Education")))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")

p1 <- dataW |>
  as_tsibble(index= date, key = category) |>
  autoplot(hits, size= 1) +
  theme(legend.position = "bottom",  
        text = element_text(size = 12), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12))+
  scale_colour_viridis_d(guide = "colourbar",
                         direction = -1, end = 0.6) 
print(p1)


