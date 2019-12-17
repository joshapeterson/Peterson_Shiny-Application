# Libraries ----------------------------------------------------------------------

library(tidyverse)
library(tidytext)

# Loading Data -------------------------------------------------------------------

if (!exists("nobel_winners")){
    nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
    nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")
}

# Data Prep ----------------------------------------------------------------------

df1 <- nobel_winners %>% 
    select("category", "organization_country", "prize_share", "prize_year", "birth_date")

df2 <- nobel_winner_all_pubs %>% 
    select("laureate_id", "prize_year", "pub_year", "pub_year")

df1$birth_date = as.Date(df1$birth_date, "%Y-%m-%d")

df1$year_of_birth = as.numeric(format(df1$birth_date, "%Y"))

by_nobel <- df1 %>% 
    group_by(category)

by_nobel_2 <- df2 %>% 
    group_by(laureate_id) %>% 
    summarize(min_prize_year = min(prize_year))

by_nobel_3 <- df2 %>%
    group_by(laureate_id) %>% 
    summarize(min_pub_year = min(pub_year))

by_nobel_4 <- inner_join(by_nobel_2, by_nobel_3, by="laureate_id")

by_nobel_4 <- by_nobel_4 %>% 
    mutate(academic_experience = min_prize_year-min_pub_year) 

by_nobel_5 <- inner_join(nobel_winner_all_pubs,by_nobel_4, by="laureate_id")

grouped_nobel_5 <- by_nobel_5 %>% 
    group_by(laureate_id)

dt <- nobel_winners %>% 
    select(category, birth_country, death_country, organization_country, prize_year) %>% 
    filter(!is.na(organization_country)) %>% 
    group_by(organization_country) %>% 
    summarize(Frequency = n())

# Extract Data for Motivation Analysis -------------------------------------------

motivation_text <- nobel_winners %>%
    select("prize_year", "category", "motivation")

# Extract Data for Paper Title Analysis ------------------------------------------

paper_title_text <- nobel_winner_all_pubs %>%
    select("prize_year", "category", "title", "is_prize_winning_paper")


paper_title_text$title <- gsub("[0-9]", "", paper_title_text$title)

# Selections ---------------------------------------------------------------------

cat_selection <- levels(factor(motivation_text$category))

cat_selection_lower <- list(
    "Chemistry" = "chemistry",
    "Medicine" = "medicine",
    "Physics" = "physics"
)

prize_selection <- list(
    "Yes" = "YES",
    "No" = "NO"
)

country_selection <- levels(factor(df1$organization_country))

# Functions ----------------------------------------------------------------------

getFreq_motivation <- function(data, prize_category, year1, year2) {
    
    tidy_motivation <- data %>%
        unnest_tokens(word, motivation) %>%
        anti_join(stop_words)
    
    if (prize_category != "All") {
        tidy_motivation_n <- tidy_motivation %>%
            filter(category == prize_category) %>%
            filter(prize_year >= year1 & prize_year <= year2) %>%
            count(word) %>%
            na.omit() %>%
            arrange(desc(n))
    } else {
        tidy_motivation_n <- tidy_motivation %>%
            filter(prize_year >= year1 & prize_year <= year2) %>%
            count(word) %>%
            na.omit() %>%
            arrange(desc(n))        
    }
    
    return(tidy_motivation_n)
}

getFreq_paper_titles <- function(data, prize_category, year1, year2, is_prize_winner) {
    
    tidy_title <- data %>%
        unnest_tokens(word, title) %>%
        anti_join(stop_words)
    
    if (prize_category != "All") {
        tidy_title_n <- tidy_title %>%
            filter(category == prize_category) %>%
            filter(is_prize_winning_paper == is_prize_winner) %>%
            filter(prize_year >= year1 & prize_year <= year2) %>%
            count(word) %>%
            na.omit() %>%
            arrange(desc(n))
    } else {
        tidy_title_n <- tidy_title %>%
            filter(is_prize_winning_paper == is_prize_winner) %>%
            filter(prize_year >= year1 & prize_year <= year2) %>%
            count(word) %>%
            na.omit() %>%
            arrange(desc(n))        
    }
    
    return(tidy_title_n)
}