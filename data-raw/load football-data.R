suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(lubridate)
})

rm(list = ls())

# download league data from football-data.co.uk
load_league_data <- function(league) {
  
  url_main <- "https://www.football-data.co.uk/"
  url_league <- paste0(url_main, league, "m.php")
  
  links <- read_html(url_league) %>% html_nodes("a") %>% html_attr("href")
  
  data <- lapply(paste0(url_main, links[grep("\\.csv$", links)]) , 
                 function(URL) { 
                   tryCatch(read.csv(URL, stringsAsFactors = F, check.names = F, na.strings = c("NA", "")) %>%
                              {cbind(Season = unlist(regmatches(URL, gregexpr("\\d+", URL)))[2] %>% substr(1, 2), .)},
                            error = function(e) {
                              warning(conditionMessage(e)); print(URL); 
                              NULL
                            })
                 }
  )
}

leagues <- c("england", "scotland", "germany", "italy", "spain", 
             "france", "netherlands", "belgium", "portugal")

# Load all CSV files in a list & merge into a single dataframe 
football_data <- lapply(leagues, load_league_data)  %>% do.call(c, .)  %>% 
  data.table::rbindlist(fill = T) %>% as.data.frame()  

# data required for a valid obs (Season, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG)
cols <- c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG")

football_data <- football_data %>% 
  filter(if_all(all_of(cols), ~ !is.na(.) )) %>%
  # remove cols with few observations (1%) - unwanted columns are introduced
  .[, colSums(is.na(.)) < 0.99*nrow(.)] %>%
  mutate(Date = parse_date_time(Date, c("dmy", "dmY")), 
         Season = ifelse(between(as.numeric(Season), 0, 49), 2000 + as.numeric(Season), 1900 + as.numeric(Season))) %>%
  arrange(Date)

saveRDS(football_data, file = "./data-raw/football-data.rds")
