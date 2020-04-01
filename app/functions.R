get_female_obesity <- function(x){
  national_stats %>%
    filter(location_desc == x & gender == "Female" & !is.na(obesity)) %>%
    select(year_end, location_desc, sample_size, gender, obesity) %>%
    arrange(year_end)
}

get_female_overweight <- function(x){
  national_stats %>%
    filter(location_desc == x, gender == "Female" & !is.na(overweight)) %>%
    select(year_end, location_desc, sample_size, gender, overweight) %>%
    arrange(year_end)
}

get_male_overweight <- function(x){
  national_stats %>%
    filter(location_desc == x, gender == "Male" & !is.na(overweight)) %>%
    select(year_end, location_desc, sample_size, gender, overweight) %>%
    arrange(year_end)
}

get_male_obesity <- function(x){
  national_stats %>%
    filter(location_desc == x & gender == "Male" & !is.na(obesity)) %>%
    select(year_end, location_desc, sample_size, gender, obesity) %>%
    arrange(year_end)
}

get_race_obesity <- function(x){
  national_stats %>%
    filter(location_desc == x & !is.na(sample_size) & !is.na(race_ethnicity) & race_ethnicity != "" & !is.na(obesity)) %>%
    select(year_end, location_desc, sample_size, race_ethnicity, obesity) %>%
    arrange(year_end)
}

get_income_obesity <- function(x){
  national_stats %>%
    filter(location_desc == x & !is.na(sample_size) & !is.na(income) & income != "" & income != "Data not reported" & !is.na(obesity)) %>%
    select(year_end, location_desc, sample_size, income, obesity) %>%
    arrange(year_end)
}


