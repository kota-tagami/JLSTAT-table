library(tidyverse)
library(readxl)

# https://www.ipss.go.jp/site-ad/TopPageData/PopPyramid2017_J.html
filename <- "pyramidDataPP29J.xls"
filepath <- 
  str_c("Population", "Source", filename, sep = "/")

dM00 <- 
  filepath %>% 
  read_xls(
    sheet = "M",
    col_names = T,
    skip = 2,
    n_max = 103
  )

dM10 <- 
  dM00 %>%
  rename(age = 男性) %>% 
  mutate(
    sex = "Men"
  ) %>% 
  pivot_longer(-c(sex, age), names_to = "year") %>% 
  mutate(
    value = value * 1000
  )


dF00 <- 
  filepath %>% 
  read_xls(
    sheet = "F",
    col_names = T,
    skip = 2,
    n_max = 103
  )

dF10 <- 
  dF00 %>%
  rename(age = 女性) %>% 
  mutate(
    sex = "Women"
  ) %>% 
  pivot_longer(-c(sex, age), names_to = "year") %>% 
  mutate(
    value = value * 1000
  )



dt00 <- bind_rows(dM10, dF10)

dt10 <- 
  dt00 %>% 
  filter(
    ! age %in% c("総数")
  ) %>% 
  mutate(
    year = year %>% as.numeric(),
    sex = sex %>% fct_inorder(),
    age = age %>% 
      str_extract("\\d{1,3}") %>% 
      as.numeric(),
    age_cat = case_when(
      age %>% between(0, 4) ~ "0-4",
      age %>% between(5, 9) ~ "5-9",
      age %>% between(10, 14) ~ "10-14",
      age %>% between(15, 19) ~ "15-19",
      age %>% between(20, 24) ~ "20-24",
      age %>% between(25, 29) ~ "25-29",
      age %>% between(30, 34) ~ "30-34",
      age %>% between(35, 39) ~ "35-39",
      age %>% between(40, 44) ~ "40-44",
      age %>% between(45, 49) ~ "45-49",
      age %>% between(50, 54) ~ "50-54",
      age %>% between(55, 59) ~ "55-59",
      age %>% between(60, 64) ~ "60-64",
      age %>% between(65, 69) ~ "65-69",
      age %>% between(70, 74) ~ "70-74",
      age %>% between(75, 79) ~ "75-79",
      age %>% between(80, 84) ~ "80-84",
      age %>% between(85, 89) ~ "85-89",
      age %>% between(90, 100) ~ "90-"
    ),
    age_cat = age_cat %>% 
      fct_inorder(),
  )


dt20 <- 
  dt10 %>% 
  group_by(year, sex, age_cat) %>% 
  summarise(
    value = sum(value, na.rm = T), 
    .groups = "drop"
  ) %>% 
  mutate(
    pref = "total"
  ) %>% 
  relocate(pref) %>% 
  arrange(year, sex, age_cat)




write_csv(
  dt20, 
  str_c("Population", "Table", "population_pyramid_total.csv", sep = "/"
))
