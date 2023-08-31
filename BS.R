install.packages("ggplot2")
install.packages("GGally")
install.packages("statsr")
install.packages("dplyr")
install.packages('car')
install.packages("psych")
library("ggplot2")
library("GGally")
library("statsr")
library("dplyr")
library("car")
library("psych")

df <- read.csv("C:/PythonWorks/Move_OTT/movie_final_data_for_analysis_v3.csv")

y_2021 <- df$imdb_rate_2021
y_2019 <- df$imdb_rate_2019
y_2017 <- df$imdb_rate_2017
y_pop <- df$movie_popularity_log

colname <- colnames(df)
colname

basic_col = colname[c(6:8)]
power_col = colname[c(9:13)]
region_col = colname[c(14:18)]
lan_col = colname[c(19:23)]
genre_col = colname[c(24:92)]
ott_col = colname[c(96:101)]

basic_info_ <- as.matrix(df %>% select(basic_col))
power_info_ <- as.matrix(df %>% select(power_col))
region_info_ <- as.matrix(df %>% select(region_col))
lan_info_ <- as.matrix(df %>% select(lan_col))
genre_info_ <- as.matrix(df %>% select(genre_col))
ott_info_ <- as.matrix(df %>% select(ott_col))


data_2021 <- df %>% filter(df$imdb_rate_2021 != 0)
data_2021

data_2019 <- df %>% filter(df$imdb_rate_2019 != 0)
data_2019

data_2017 <- df %>% filter(df$imdb_rate_2017 != 0)
data_2017

model_2021 <- lm(y_2021 ~ basic_info_ + region_info_ + lan_info_ +
              genre_info_ + power_info_ + ott_info_ , data_2021)
summary(model_2021)

plot(y_2021 ~ basic_info_ + region_info_ + lan_info_ +
       genre_info_ + power_info_ + ott_info_ , data = data_2021)


model_2019 <- lm(y_2019 ~ basic_info_ + region_info_ + lan_info_ +
                   genre_info_ + power_info_ + ott_info_ , data_2019)
summary(model_2019)

model_2017 <- lm(y_2017 ~ basic_info_ + region_info_ + lan_info_ +
                   genre_info_ + power_info_ + ott_info_, data_2017)
summary(model_2017)

model_pop <- lm(y_pop ~ basic_info_ + region_info_ + lan_info_ +
                   genre_info_ + power_info_ + ott_info_, df)
summary(model_pop)


model <- lm(y_2021 ~ release_year + region_ + language_ +
              genre_1_ + genre_2_ + genre_3_ +
              runtime_min_log + is_adult +
              dircetor_total_num_movies + director_num_academy_winner +
              director_total_popluarity + stars_num_academy_winner +
              stars_popularity_point_log + is_original_movie + 
              Avail_OTT + amazon_prime + netflix + amazon_prime +
              disney_plus + num_ott_avail, data = data_2021)
summary(model)

install.packages('tinytex')
