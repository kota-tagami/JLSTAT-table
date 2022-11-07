#-----------------------------#
# >> setup << #
#-----------------------------#
source("helper.R")

## https://www.ipss.go.jp/pp-zenkoku/j/zenkoku2017/db_zenkoku2017/db_zenkoku2017syosaikekka.html
## 2022-11-07

data_name <- "1-9a.xls"
data_path <- str_c("Population", "Source", data_name, sep = "/")


#-----------------------------#
# >> Data << #
#-----------------------------#
get_dt <- function(sheetname, year) {
  dt0 <- 
    data_path %>% 
    read_excel(
      sheet = sheetname,
      col_names = F
    ) %>% 
    select(
      age = `...1`, `男性` = `...3`, `女性` = `...4`
    )
  
  dt1a <- 
    dt0 %>% 
    slice(7:27) %>% 
    mutate(year = year)
  
  dt1b <- 
    dt0 %>% 
    slice(33:53) %>% 
    mutate(year = year + 5)
  
  if (nrow(dt1b) < 2) {
    dt2 <- bind_rows(dt1a)
  } else {
    dt2 <- bind_rows(dt1a, dt1b)
  }
  
  dt2
}

sheets <- list(
  sheetname = excel_sheets(data_path),
  year = seq(2015, 2065, 10)
)


dt00 <- 
  sheets %>% 
  pmap_dfr(get_dt)


dt10 <- 
  dt00 %>% 
  pivot_longer(
    -c(year, age),
    names_to = "sex"
  ) %>% 
  mutate(
    age = age %>% 
      stringi::stri_trans_nfkc() %>% 
      str_remove_all(" ") %>% 
      str_c(., "歳") %>% 
      fct_inorder(),
    age = age %>% 
      fct_collapse(
        "85歳以上" = c(
          "85~89歳", "90~94歳", "95~99歳",
          "100+歳"
        )
      ),
    sex = sex %>% 
      fct_inorder(),
    value = value %>% 
      as.numeric() %>% 
      {. * 1000}
  ) %>% 
  relocate(year, sex, age) %>% 
  arrange(year, sex, age) %>% 
  group_by(year, sex, age) %>% 
  summarise(
    value = sum(value, na.rm = T),
    .groups = "drop"
  )
  

#-----------------------------#
# >> save << #
#-----------------------------#
save_table(dt10, "Population", "pyramid_total_projection")

