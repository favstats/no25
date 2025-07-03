
all_dat <- readRDS("../data/all_data.rds")

# readRDS("data/all_data.rds") %>% count(entities_groups.group_name)

# all_dat <- readRDS("../data/all_data.rds") %>%
#   filter(str_detect(entities_groups.group_name, "Parties"))

# all_dat %>% count(party)

# readRDS("data/all_data.rds") %>% count(entities_groups.group_name)

# all_dat %>% count(entities_groups.group_name)

classified_advertisers <- all_dat  %>% 
  select(page_id = advertisers_platforms.advertiser_platform_ref, page_name = advertisers_platforms.advertiser_platform_name, classification = entities.name, colo = entities.color) %>%
  distinct(page_id, .keep_all = T)

color_dat <- classified_advertisers %>% distinct(classification, colo) %>% 
  rename(party = classification, colors = colo)

# classified_advertisers <- readr::read_rds("https://github.com/favstats/wtm_au/raw/refs/heads/main/data/all_dat.rds") %>% 
#   select(page_id, page_name, classification = party) %>%
#   distinct(page_id, .keep_all = T) %>% 
#   filter(classification != "unknown")


election_dat30 <- readRDS("../data/election_dat30.rds")  %>% 
  as_tibble() %>% 
  select(-party) %>%
  # left_join(classified_advertisers %>% select(page_id, party = classification)) %>% 
  filter(is.na(no_data)) %>% 
  left_join(all_dat %>% select(page_id, party, entities.name)) %>%
  drop_na(party) %>%
  mutate(internal_id = page_id) %>%
  filter(!(party %in% c("Others","And", "AND", "Reg", "Oth", "Gov", "Sta", "Inv", "Pol", "Company", "Other Political Party","Government Institution","Independent", "Media Organization", "NGO/Civil Society","Unknown", "Government", "Federal Government", "FedGov", "LocGov"))) %>% 
  mutate(party = entities.name) %>%
  filter(str_detect(party, "Climate advocacy|gas advertisers", negate = T)) %>% 
  drop_na(party) %>% 
  mutate(total_spend_formatted = readr::parse_number(as.character(total_spend_formatted))) %>% 
  mutate(total_num_ads = readr::parse_number(as.character(total_num_ads))) %>% 
  mutate(total_spend_pct = as.numeric(total_spend_pct))%>% 
  mutate(num_ads = readr::parse_number(as.character(num_ads)))


# readRDS("data/election_dat30.rds")  %>%
#   as_tibble() %>%
#   select(-party) %>%
#   # left_join(classified_advertisers %>% select(page_id, party = classification)) %>%
#   filter(is.na(no_data)) %>%
#   left_join(all_dat %>% select(page_id, party, entities.name)) %>%
#   drop_na(party) %>%
#   count(entities.name)

# all_dat %>% select(page_id, party, entities.name) %>% count(entities.name)

election_dat7 <- readRDS("../data/election_dat7.rds")  %>%
  as_tibble() %>% 
  select(-party) %>%
  # left_join(classified_advertisers %>% select(page_id, party = classification)) %>% 
  filter(is.na(no_data)) %>% 
  left_join(all_dat %>% select(page_id, party, entities.name)) %>%
  drop_na(party) %>%
  mutate(internal_id = page_id) %>%
  filter(!(party %in% c("Others","And", "AND", "Reg", "Oth", "Gov", "Sta", "Inv", "Pol", "Company", "Other Political Party","Government Institution","Independent", "Media Organization", "NGO/Civil Society","Unknown", "Government", "Federal Government", "FedGov", "LocGov"))) %>% 
  mutate(party = entities.name) %>%
  filter(str_detect(party, "Climate advocacy|gas advertisers", negate = T))  %>% 
  drop_na(party) %>% 
  mutate(total_spend_formatted = readr::parse_number(as.character(total_spend_formatted))) %>% 
  mutate(total_num_ads = readr::parse_number(as.character(total_num_ads))) %>% 
  mutate(total_spend_pct = as.numeric(total_spend_pct))%>% 
  mutate(num_ads = readr::parse_number(as.character(num_ads)))
  




currency_symbol <- "€"

# color_dat <- election_dat30 %>% 
#   mutate(party = entities.name) %>% 
#   distinct(party, entities.color) %>% 
#   rename(colors = entities.color)

# color_dat <- data.frame(
#   party = c("ACP-NPA", "AD", "AG", "ALP", "C", "CLP", "FFP", "Katter", "LNPQ", "LPA", "ONP", "Reason", "United Australia Party"),
#   colors = c("#006644",  # ACP-NPA (Agrarian/Nationalist - Green)
#              "#BFDA01",  # AD (Liberal/Gold)
#              "#3F9C35",  # AG (Australian Greens - Green)
#              "#DE3533",  # ALP (Australian Labor Party - Red)
#              "#00008B",  # C (Conservative - Dark Blue)
#              "#CC0000",  # CLP (Country Liberal Party - Red)
#              "#FFCC00",  # FFP (Family First Party - Yellow)
#              "#800000",  # Katter's Australian Party - Maroon
#              "#034694",  # LNPQ (Liberal National Party of Queensland - Blue)
#              "#0047AB",  # LPA (Liberal Party of Australia - Blue)
#              "#FF6600",  # ONP (One Nation Party - Orange)
#              "#7E4798",  # Reason Party - Purple
#              "#FFD700"   # United Australia Party - Gold
#   )
# )


color_dat <- distinct(color_dat, party, .keep_all = TRUE) %>% drop_na()


fin <- (as.Date(election_dat30$ds[1])-lubridate::days(1))
begin7 <- fin-lubridate::days(6)
begin30 <- fin-lubridate::days(29)

tibble(fin,
       begin7,
       begin30) %>%
  write_csv(here::here("dates.csv"))



# Function to create Dutch date strings with suffixes
create_date <- function(x) {
  the_date <- format(x, "%e %b") # %e for day of the month without leading zeros, %B for full month name in Dutch
  # In Dutch, date suffixes are not commonly used so we can omit the 'append_date_suffix' part
  return(trimws(the_date)) # trimws to remove any leading or trailing whitespace which might be left after %e
}

last7days_string <- paste0(create_date(begin7), " - ", create_date(fin), " ", lubridate::year(fin))
last30days_string <- paste0(create_date(begin30), " - ", create_date(fin), " ", lubridate::year(fin))

write_lines(last7days_string, "last7days_string.txt")
write_lines(last30days_string, "last30days_string.txt")


# election_dat30 %>% count(party)
#   arrange(left_right) %>% View()

most_left_party <- color_dat$party[1]
# most_left_party <- "The Green Party"
# # Print the Dutch date range strings
# print(last7days_string)
# print(last30days_string)
#
# # Reset locale back to the original if necessary
# Sys.setlocale("LC_TIME", "C")
# print("oo")

color_dat <- color_dat %>% 
  mutate(colors = case_when(
    colors == "#aaa" ~ "#A9A9A9",
    colors == "orange" ~ "#f77604",
    colors == "green" ~ "#7aba2d",
    colors == "lightgreen" ~ "#90EE90",
    colors == "darkgray" ~ "#A9A9A9",
    colors == "darkblue" ~ "#000080",
    colors == "black" ~ "#000000",
    colors == "lightblue" ~ "#ADD8E6",
    colors == "#ccc" ~ "#cccccc",
    is.na(colors) ~ "#00BFFF",
    T ~ colors
  ))


sets <- list(cntry = "NO")