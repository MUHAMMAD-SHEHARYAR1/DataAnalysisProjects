#age gaps and bechdel test

#setup

library(tidyverse)
theme_set(theme_minimal())

age_gaps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-02-14/age_gaps.csv')
bechdel <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-09/raw_bechdel.csv')

#orientation

View(bechdel)
ggplot(bechdel,
       aes(x = rating)) +
  geom_bar()

View(age_gaps)
ggplot(age_gaps,
       aes(x = age_difference)) +
  geom_histogram()

#Data prep

age_gaps <- age_gaps %>%
  mutate(man_older = case_when(
    character_1_gender == 'man' & character_2_gender == 'woman' ~
      actor_1_age - actor_2_age,
    character_2_gender == 'man' & character_1_gender == 'woman' ~
      actor_2_age - actor_1_age
  ))

ggplot(age_gaps,
       aes(x= man_older)) +
  geom_histogram()
mean(age_gaps$man_older,
     na.rm = TRUE)

# Join the Data
movies <- age_gaps %>%
  left_join(bechdel,
            by = c('movie_name' = 'title',
                   'release_year' = 'year'))

# 2017 wonder woman has 3 bechdel ratings??

bechdel %>%
  filter(title == "Wonder Woman")

movies <- movies %>%
  filter(!(id %in% c(9293,9294)))

#Are there others?
bechdel %>%
  count(title,
        year) %>%
  filter(n > 1)

# Visualization: age gap and bechdel rating

movies <- movies %>%
  mutate(rating_cat = as.factor(rating)) %>%
  drop_na(man_older,
          rating)

ggplot(movies, aes(x= rating_cat,
                   y = man_older)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .2) +
  scale_x_discrete(labels = c('Fail',
                              'No conversation',
                              'Only about men',
                              'Pass')) +
  labs(x = 'Bechdel test result',
       y = 'Male age gap',
       title = 'Something to talk about')

#Correlation
cor(movies$man_older,
    movies$rating)

cor(movies$man_older,
    movies$rating,
    method = 'spearman')

#Testing
model <- lm(man_older ~ rating_cat,
            data = movies)
summary(model)

model <- aov(man_older ~ rating_cat,
            data = movies)
TukeyHSD(model)
