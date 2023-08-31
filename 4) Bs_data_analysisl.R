library("ggplot2")
library("GGally")
library("statsr")
library("dplyr")
library("car")
library("psych")
library('tidyverse')
library('broom')
library('sjPlot')
library('sjmisc')
library('sjlabelled')
library('stargazer')


df <- read.csv("C:/PythonWorks/Move_OTT/movie_final_data_for_analysis_with_dol.csv")
df

data_2021 <- df %>% filter(df$imdb_rate_2021 != 0)
data_2019 <- df %>% filter(df$imdb_rate_2019 != 0)
data_2017 <- df %>% filter(df$imdb_rate_2017 != 0)

y_2021 <- data_2021$imdb_rate_2021
y_2019 <- data_2019$imdb_rate_2019
y_2017 <- data_2017$imdb_rate_2017
y_pop <- df$movie_popularity_log


model_1 <- lm(y_2021 ~ release_year + region_ + language_ +
                genre_ + budget_dol_log +
                runtime_min_log + is_adult +
                dircetor_total_num_movies + director_num_academy_winner +
                director_total_popluarity + stars_num_academy_winner +
                stars_popularity_point_log , data = data_2021)

model_2 <- lm(y_2021 ~ release_year + region_ + language_ +
                genre_ + budget_dol_log +
                runtime_min_log + is_adult +
                dircetor_total_num_movies + director_num_academy_winner +
                director_total_popluarity + stars_num_academy_winner +
                stars_popularity_point_log + is_original_movie + 
                Avail_OTT , data = data_2021)

model_pop_1 <- lm(y_pop ~ release_year + region_ + language_ +
                    genre_ + budget_dol_log +
                    runtime_min_log + is_adult +
                    dircetor_total_num_movies + director_num_academy_winner +
                    director_total_popluarity + stars_num_academy_winner +
                    stars_popularity_point_log , data = data_2021)

model_pop_2 <- lm(y_pop ~ release_year + region_ + language_ +
                    genre_ + budget_dol_log +
                    runtime_min_log + is_adult +
                    dircetor_total_num_movies + director_num_academy_winner +
                    director_total_popluarity + stars_num_academy_winner +
                    stars_popularity_point_log + is_original_movie + 
                    Avail_OTT , data = data_2021)

model_2019 <- lm(y_2019 ~ release_year + region_ + language_ +
                   genre_ + budget_dol_log +
                   runtime_min_log + is_adult +
                   dircetor_total_num_movies + director_num_academy_winner +
                   director_total_popluarity + stars_num_academy_winner +
                   stars_popularity_point_log + is_original_movie + 
                   Avail_OTT , data = data_2019)

tab_model(model_1, model_2, model_pop_1, model_pop_2,col.order = c('p','est'),
          dv.labels = c('Model_without_ott(IMDb ratings)',
                        'Model_with_ott(IMDb ratings)',
                        'Model_without_ott(Popularity)',
                        'Model_with_ott(Popularity'),auto.label = 0)

stargazer(model_1,model_2, model_pop_1, model_pop_2,
          title='If the avialabilty of OTT platforms affect the success of the movies', 
          type='html', out = "First_model.html")

stargazer(model_2, model_2019,
          title='If the impact of the OTT availability growth after covid-19', 
          type='html', out = "Second_model.html")

model_comparison_imdbratings <- anova(model_1, model_2)
mdeol_comparison_popularity <- anova(model_pop_1,model_pop_2)


stargazer(model_comparison_imdbratings, summary = F, 
          title='Model comparison for IMDb ratings',
          type = 'html', out = 'First_Anova.html')

stargazer(mdeol_comparison_popularity, summary = F, title='Model comparison for popularity',
          type = 'html', out = 'Second_Anova.html')

