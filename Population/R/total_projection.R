#-----------------------------#
# >> setup << #
#-----------------------------#
source("helper.R")

## https://www.ipss.go.jp/pp-zenkoku/j/zenkoku2017/db_zenkoku2017/db_s_suikeikekka_10.html
## 2022-11-03

data_name <- "10-1.xls"
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


col_names <- c(
  "死亡中位_出生中位",
  "死亡中位_出生高位",
  "死亡中位_出生低位",
  "死亡高位_出生中位",
  "死亡高位_出生高位",
  "死亡高位_出生低位",
  "死亡低位_出生中位",
  "死亡低位_出生高位",
  "死亡低位_出生低位"
)


dt01 <- 
  dt00 %>% 
  select(
    year = `...2`, `...3`:last_col()
  )

names(dt01) <- c("year", col_names)


dt10 <- 
  dt01 %>% 
  pivot_longer(
    -year, 
    names_to = c("死亡", "出生"),
    names_pattern = "死亡(.*)_出生(.*)"
  ) 


dt11 <- 
  dt10 %>% 
  mutate(
    across(
      c(`死亡`, `出生`),
      ~ fct_relevel(., "低位", "中位", "高位")
    ),
    value = value * 1000
  ) %>% 
  arrange(year, `死亡`, `出生`)


#-----------------------------#
# >> save << #
#-----------------------------#
save_table(dt11, "Population", "total_projection")


