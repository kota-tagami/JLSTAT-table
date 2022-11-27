#-----------------------------#
# >> setup << #
#-----------------------------#
source("helper.R")

## https://www.ipss.go.jp/pp-shicyoson/j/shicyoson18/t-page.asp
## 2022-11-27
## 地域別の推計値の合計は、「日本の将来推計人口（平成29年推計）」（出生中位・死亡中位仮定）による推計値に合致します。

data_name <- "suikei_kekka.xls"
data_path <- str_c("Population", "Source", data_name, sep = "/")


#-----------------------------#
# >> Data << #
#-----------------------------#
dt00 <- 
  data_path %>% 
  read_excel(
    col_names = F,
    skip = 4
  )

dt01 <- 
  dt00 %>% 
  select(
    "code" = 1, 
    "type" = 2, 
    "pref" = 3, 
    "year" = 5, 
    "value" = 6
  ) %>% 
  filter(
    type == "a"
  ) %>% 
  mutate(
    pref = pref %>% 
      fct_inorder(),
    year = year %>% 
      str_remove_all("年") %>% 
      as.numeric(),
  ) %>% 
  select(-c(code, type))



#-----------------------------#
# >> save << #
#-----------------------------#
save_table(dt01, "Population", "total_prefecture_projection")



