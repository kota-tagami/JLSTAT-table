#-----------------------------#
# >> setup << #
#-----------------------------#
source("helper.R")

#:[ 国勢調査「時系列データ 男女，年齢，配偶関係」 ]:#
stat_id <- "0003410379"


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

cat_area <- 
  df_meta %>% 
  filter(
    var_id %in% c("area"),
    ! val_id %in% c("00000", "00100", "00200"),
  ) %>% 
  pull(val_id) %>% 
  str_c(collapse = ",")


#-----------------------------#
# >> Stats data << #
#-----------------------------#
res_stats <- estat_api(
  api_method = "getStatsData",
  params = list(
    "statsDataId" = stat_id,
    "cdTab" = "020",
    "cdCat01" = "100",
    "cdArea" = cat_area
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
    val_lab = if_else(is.na(val_lab), value, val_lab),
  ) %>% 
  select(-c(name, value)) %>% 
  pivot_wider(names_from = var_lab, values_from = val_lab) %>% 
  mutate(
    value = value %>% 
      as.numeric(),
  )


#-----------------------------#
# >> save << #
#-----------------------------#
save_table(df_stats, "Population", "total_prefecture")

