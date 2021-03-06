library(tidyverse)

data <- read_csv("poll/assessment.zip") %>% 
  select(experience_r = 3, experience_python = 7, quanti_1 = 16, quanti_2 = 17, quanti_3 = 18,
         continent = 19, mail = 20, group_preference = 21) %>% 
  filter(mail != "tamara.dick@stud.uni-regensburg.de") %>% 
  mutate(across(starts_with("experience"), 
                ~case_when(
                  . == "Never heard of it" ~ 1,
                  . == "I know how to open the application" ~ 2,
                  . ==  "Used it a couple of times" ~ 3,
                  . ==  "Used it for a project/assignment/paper" ~ 4)),
         across(starts_with("quanti"), 
                ~case_when(
                  . == "Never heard of it" ~ 1,
                  . == "Read of it" ~ 2,
                  . ==  "Did it a couple of times" ~ 3,
                  . ==  "Did it for something somebody else graded (project/paper/etc.)" ~ 4)))



## assigning groups for weekly R assignments -- emphasis lies on location and stated preferences

group_1 <- data %>% 
  filter(continent == "Asia") %>% 
  select(group_1 = mail) %>% 
  pivot_longer(everything(), names_to = "group", values_to = "mail")
group_2 <- data %>% 
  filter(str_detect(mail, pattern = "Fatten|Alina|erhan")) %>% 
  select(group_2 = mail) %>% 
  pivot_longer(everything(), names_to = "group", values_to = "mail")
group_3 <- data %>% 
  filter(str_detect(mail, pattern = "Grant|Ernesto")) %>% 
  select(group_3 = mail) %>% 
  pivot_longer(everything(), names_to = "group", values_to = "mail")
remainder <- data %>% filter(!mail %in% c(group_1$mail, group_2$mail, group_3$mail)) %>% select(mail, experience_r)                     

r_experienced <- remainder %>% 
  arrange(-experience_r) %>% 
  slice(1:4) %>% 
  select(mail) %>% 
  mutate(mail = case_when(str_detect(mail, "hlp") ~ "petr.hladik@stud.uni-regensburg.de",
                          TRUE ~ mail)) %>% 
  bind_rows(tibble(mail = "sophia.grill@stud.uni-regensburg.de"))
r_unexperienced <- remainder %>% 
  arrange(experience_r) %>% 
  slice(1:6)

list <- vector(mode = "list", length = 5)
set.seed(123)
x <- sample(1:6, size = 5)
for (i in 1:5) {
  j <- x[[i]]
  name <- paste0("group_", as.character(i+3))
  list[[i]] <- c(r_experienced$mail[[i]], r_unexperienced$mail[[j]]) %>% enframe(name = NULL, value = name)
}

r_groups_temp <- bind_rows(group_1, group_2, group_3,
          list %>% 
            bind_cols()%>% 
            pivot_longer(everything(), names_to = "group", values_to = "mail")) 

r_groups_final <- r_unexperienced %>% 
  filter(!mail %in% r_groups_temp$mail) %>% 
  mutate(group = sample(r_groups_temp$group, 1)) %>% 
  select(group, mail) %>% 
  bind_rows(r_groups_temp) %>% 
  arrange(group) 


## paper groups

paper_groups <- r_experienced %>% rowid_to_column("group") %>% 
  left_join(r_groups_final %>%
              filter(!mail %in% r_experienced$mail) %>% 
              select(-group) %>% 
              pingers::shuffle() %>% 
              mutate(group = c(1:5, 1:5, 1:3)),
            by = "group") %>% 
  pivot_longer(starts_with("mail"), names_to = NULL, values_to = "mail") %>% 
  distinct(mail, .keep_all = TRUE)
