#-----------------------------#
# >> setup << #
#-----------------------------#
source("helper.R")

#:[ 国勢調査「時系列データ 男女，年齢，配偶関係」 ]:#
stat_id <- "0003410380"


#-----------------------------#
# >> META data << #
#-----------------------------#
res_meta <- estat_api(
  api_method = "getMetaInfo", 
  params = list(
    "statsDataId" = stat_id,
    "explanationGetFlg" = "N"
  )
)

# listviewer::jsonedit(res_meta)

df_meta <- 
  res_meta %>% 
  pluck("GET_META_INFO", "METADATA_INF", "CLASS_INF", "CLASS_OBJ") %>% 
  tibble(data = .) %>% 
  hoist(
    data,
    var_id = "@id", var_lab = "@name",
    class_data = "CLASS"
  ) %>%
  unnest_auto(class_data) %>% 
  hoist(
    class_data,
    val_id = "@code", val_lab = "@name",
    val_level = "@level", val_unit = "@unit"
  )

# DT::datatable(df_meta)


#-----------------------------#
# >> Stats data << #
#-----------------------------#
res_stats <- estat_api(
  api_method = "getStatsData",
  params = list(
    "statsDataId" = stat_id,
    "cdTab" = "020",
    "cdCat01" = "110,120"
  )
)

df_stats <- 
  res_stats %>% 
  pluck("GET_STATS_DATA", "STATISTICAL_DATA", "DATA_INF", "VALUE") %>% 
  tibble(data = .) %>% 
  unnest_auto(data) %>% 
  rename_with(
    ~ str_remove_all(., "\\@") %>% 
      str_replace_all("\\$", "value")
  ) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(-id) %>% 
  left_join(
    df_meta %>% 
      select(var_id, val_id, var_lab, val_lab),
    by = c("name" = "var_id", "value" = "val_id")
  ) %>% 
  mutate(
    across(where(is.character), stringi::stri_trans_nfkc),
    var_lab = if_else(is.na(var_lab), name, var_lab),
    val_lab = if_else(is.na(val_lab), value, val_lab)
  ) %>% 
  select(-c(name, value)) %>% 
  pivot_wider(names_from = var_lab, values_from = val_lab)


df_stats1 <- 
  df_stats %>% 
  filter(
    ! `時間軸(調査年)` %>% 
      str_detect("不詳補完値"),
    ! `年齢(5歳階級)_時系列` %>% 
      str_detect("再掲"),
    ! `年齢(5歳階級)_時系列` %in% c(
      "総数"
    ),
  ) %>% 
  mutate(
    value = as.numeric(value),
    sex = `男女_時系列` %>% 
      str_c(., "性"),
    year = `時間軸(調査年)` %>% 
      str_remove_all("年") %>% 
      as.numeric(),
    across(where(is.character), fct_inorder),
    age = `年齢(5歳階級)_時系列` %>% 
      fct_collapse(
        "85歳以上" = c(
          "85~89歳", "90~94歳", "95~99歳",
          "100~104歳", "105~109歳", "110歳以上"
        )
      ),
  ) %>% 
  group_by(year, sex, age) %>% 
  summarise(
    value = sum(value, na.rm = T),
    .groups = "drop"
  ) 


#-----------------------------#
# >> save << #
#-----------------------------#
save_table(df_stats1, "Population", "pyramid_total")

