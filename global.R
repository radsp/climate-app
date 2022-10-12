
# Load data -------------------------------------------------


adm_master <- read_civis(232951469)


adm0_choice <- adm_master %>%
  filter(aggregation_level == 0) %>% select(geo_id, country) %>% arrange(country) %>%
  relocate(country) %>% deframe()

ctry <- sort(unique(as.character(adm_master$country)))


mo_lookup <- data.frame(month = 1:12, month_ssn = c(7:12, 1:6)) %>%
  mutate(date_ssn = case_when(month %in% 7:12 ~ as.Date(paste0("2021-", month, "-01")),
                              TRUE ~ as.Date(paste0("2022-", month, "-01"))),
         date_cy = as.Date(paste0("2022-", month, "-01")))


date_now <- as.Date(paste0(format(Sys.Date(), "%Y-%m"), "-01"))
   

# Colors ---------------------------------------------------

clr_bar_grey <- "#cccccc"
clr_red <-  "#cc3311"
clr_blue <- "#0077BB"
clr_green <- "#4daf4a"   # "#EE7733"
clr_grey <- "#BBBBBB"

clr_seq <- c(clr_green, clr_blue, clr_red)

lgnd_seq <- c("'linegreen'", "'lineblue'", "'linered'")

clr_epi1 <- "#fb8072"
clr_epi2 <- "#80b1d3"
clr_epi3 <- "#8dd3c7"

clr_epi_seq <- c(clr_epi3, clr_epi2, clr_epi1)

# lgnd_epi_seq <- c("'container-circle green'", "'container-circle blue'", "'container-circle red'")

lgnd_epi_seq <- c("linegreen-epi'", "'lineblue-epi'", "'linered-epi'")


