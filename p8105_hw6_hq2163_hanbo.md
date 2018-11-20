p8105\_hw6\_hq2163\_hanbo
================
Hanbo Qiu
November 19, 2018

Problem 1
---------

**import a dataframe and make some alternation on some variables:**

``` r
homicides = read_csv(file = "./homicide-data.csv") %>% 
   mutate(city_state = str_c(city, ", ", state),
        resolved = as.numeric(disposition == "Closed by arrest"),
        victim_age = as.numeric(victim_age),
        victim_race = ifelse(victim_race == "White", "white", "non-white"), 
        victim_race = relevel(factor(victim_race),ref = "white")) %>% 
   filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"))
## Parsed with column specification:
## cols(
##   uid = col_character(),
##   reported_date = col_integer(),
##   victim_last = col_character(),
##   victim_first = col_character(),
##   victim_race = col_character(),
##   victim_age = col_character(),
##   victim_sex = col_character(),
##   city = col_character(),
##   state = col_character(),
##   lat = col_double(),
##   lon = col_double(),
##   disposition = col_character()
## )
## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
## coercion
```

Create a city\_state variable (e.g. “Baltimore, MD”), and a binary variable indicating whether the homicide is solved. Omit cities Dallas, TX; Phoenix, AZ; and Kansas City, MO and Tulsa, AL. Modifiy victim\_race to have categories white and non-white, with white as the reference category. Modify victim\_age as numeric.
