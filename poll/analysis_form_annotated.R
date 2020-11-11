# load packages -- if it's your first time, install them first
library(dplyr) # for wrangling
library(tidyr) # for reshaping
library(ggplot2) # for plotting
library(forcats) # for working with factors

# or, if you are lazy -- which I definitely am
library(tidyverse) # loads all the aforementioned packages and some more…

data <- read_csv("form_results.csv") # read in data

View(data) # let's have a look… 

glimpse(data) # View() often is not the best choice -- for instance, when you have larger tibbles

# first question: students' experience with different applications
relationship <- data %>% # build new tibble 
  select(1:7) %>% # subset columns
  rename_at(vars(starts_with("How is your relationship")), # rename columns
            funs(str_extract(., "(?<=\\[).+?(?=\\])"))) %>% # regexes will not be part of the course -- but are extremely useful
  pivot_longer(-Zeitstempel, # reshape data to longer format -- necessary for plotting it
               names_to = "application", 
               values_to = "experience") %>% 
  mutate(experience = as_factor(experience), # make 'experience' a factor
         experience = fct_relevel(experience, c(
           "Never heard of it",
           "I know how to open the application",
           "Used it a couple of times",
           "Used it for a project/assignment/paper",
           "Advanced user")))

ggplot(data = relationship) + # plot it
  geom_bar(aes(experience, fill = experience)) +
  facet_grid(application ~ .) # make different plots according to different applications

# second question: students' experience with R
terms_of_r <- data %>% 
  select(1, 8:15) %>% 
  rename_at(vars(starts_with("In terms of")),
            funs(str_extract(., "(?<=\\[).+?(?=\\])"))) %>% 
  pivot_longer(-Zeitstempel, 
               names_to = "achievement", 
               values_to = "experience") %>%
  mutate(experience = as_factor(experience),
         experience = fct_relevel(experience, c(
           "Never heard of it",
           "Read of it",
           "Did it a couple of times",
           "Did it for something somebody else graded (project/paper/etc.)",
           "Doing it on a daily basis")))

ggplot(data = terms_of_r) +
  geom_bar(aes(experience, fill = experience)) +
  facet_grid(achievement ~ .)

# third question: what the students want to do in the final part
final_part <- data %>% 
  select(1, answer = 16) # renaming the  variable can be done in the selection process as well -- handy!

ggplot(data = final_part, aes(answer)) +
  geom_bar() # not readable

final_part %>% count(answer) %>% # count occurences of different answers 
  mutate(percentage = round(n/sum(n), 2)) # create new columm with percentage

# so far so good? Did you notice the ' %>% ' operator? It's going to become your best friend!


