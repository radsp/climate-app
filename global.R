
# Load data -------------------------------------------------

## If run locally
# x0 <- read_csv("data/climate.csv", show_col_types = F)
# x0 <- read_csv("data/climate_test.csv", show_col_types = F)

## If on platform
x0 <- read_civis(sql("SELECT * FROM staging_pmihq.climate_app WHERE (country IN ('Malawi', 'Rwanda', 'Senegal') AND (variable_name = 'rf'))")) %>%
  mutate(date = as.Date(as.character(date)))

x0 <- x0 %>%
  rename(adm_label_tmp = adm_label)

adm_master <- x0 %>%
  select(geo_id, country, admin_level_1, admin_level_2, aggregation_level) %>% distinct()

adm0_choice <- adm_master %>%
  filter(aggregation_level == 0) %>% select(geo_id, country) %>% arrange(country) %>%
  relocate(country) %>% deframe()






mo_lookup <- data.frame(month = 1:12, month_ssn = c(7:12, 1:6)) %>%
  mutate(date_ssn = case_when(month %in% 7:12 ~ as.Date(paste0("2021-", month, "-01")),
                              TRUE ~ as.Date(paste0("2022-", month, "-01"))),
         date_cy = as.Date(paste0("2022-", month, "-01")))


date_now <- as.Date(paste0(format(Sys.Date(), "%Y-%m"), "-01"))
  

# Country list ----------------------------------------------

ctry <- sort(unique(as.character(x0$country)))

# Colors ---------------------------------------------------

clr_bar_grey <- "#cccccc"
clr_red <- "#cc3311"
clr_blue <- "#0077BB"
clr_orange <- "#EE7733"
clr_grey <- "#BBBBBB"

clr_seq <- c(clr_orange, clr_blue, clr_red)

lgnd_seq <- c("'lineorange'", "'lineblue'", "'linered'")
