#assignment3

# In this assignment we will be scraping the CIA World Factbook. Some code, including XPath
# query info, is provided to get you up to speed. The focus will be on generalizing your code
# and using functions with iteration.

# In most cases you can answer the question by providing the code that will give the answer.
# If a textual answer is required, add it as a comment to the code. You will be graded on
# correctness as well as proper style (check https://style.tidyverse.org/).

# Development hint: Try your functions on a selection of URLs first, instead of on all of them.
# Downloading the information for all countries will take some time and this will slow down
# your development process. Only test on all of them when it works for, say, 10 countries or so.

rm(list = ls())

library(plyr)
library(dplyr)
library(RCurl)
library(tidyr)
library(tidyverse)
library(xml2)

base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"

# Question 1 --------------------------------------------------------------

# Create a function called get_population_ranking which scrapes the ranking from
# fields/335rank.html (add it to the given base_url).
# a. Extract the 4 elements for each country, given the 4 xpath queries that are provided to you
# b. Rename the “value” column to “population”
# c. Rename the “rank” column to “rank.population”
# d. Remove the “../” from all urls so they can be added to the base_url

#' Question 1: Get Population Ranking
#'
#' @return Tibble with 4 added columns: country_link, country, population, and rank.population.
#'
#' @examples See code below.

get_population_ranking <- function() {
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url = str_c(base_url, "fields/335rank.html")
  raw_html <- read_html(getURL(url, .encoding = "UTF-8"))
  
  results <- list()
  
  for (nm in names(xpath_expressions)) {
    results[[nm]] <- raw_html %>%
      xml_find_all(xpath_expressions[nm]) %>%
      as_list() %>% unlist()
  }
  
  results %>%
    as_tibble() %>%
    rename(population = value,
           rank.population = rank) %>%
    mutate(country_link = str_replace_all(country_link, "\\.\\./", ""))
}

results_global <- get_population_ranking()

# Question 2 --------------------------------------------------------------

# Create a function called get_land_area with country_link as a parameter, which will
# visit each country_link (what you got from the previous question) and which will extract
# the land area element from the page using the given XPath query.
# a. You do not have to clean or transform the output (yet).
# b. Your function should work when country_link is a single url, but also when it is a character vector with multiple urls (iterate over the urls).
# c. The output should be a character vector of the same length as the country_link parameter.

#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls.
#'
#' @return Character vector with retrieved land area information from urls.
#'
#' @examples See code below.

get_land_area <- function(country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  urls = str_c(base_url, country_link)
  
  results <- as_tibble(matrix(nrow = length(country_link), ncol = 1))
  
  for (i in seq_along(urls)) {
    raw_html <- read_html(getURL(urls[i], .encoding = "UTF-8"))
    results[i, 1] <- raw_html %>%
      xml_find_all(xpath) %>%
      as_list() %>% unlist()
  }
  
  return(results)
}

results_global["land_area"] <- get_land_area(results_global$country_link)

# Question 3 --------------------------------------------------------------

# Create a function called get_population_density that will use the land area and
# population information acquired using the previous two functions and compute the
# population density for each country.
# a. Retrieve the population ranking using get_population_ranking
# b. Retrieve for each country the land_area from the country_link url using get_land_area
# c. Transform population and land area into a number (be careful with Ethiopia)
# d. Add a column called population_density as population / land_area.

#' Question 3: Get Population Density
#'
#' @return Implicitly returns a results tibble with the added devision (population / land_area).
#'
#' @examples See code below.

get_population_density <- function(){
  results_global %>%
    mutate(land_area = str_replace_all(land_area, " million", ",000,000"),
           land_area = as.numeric(str_replace_all(land_area, "[^\\d]", "")),
           population = as.numeric(str_replace_all(population, "[^\\d]", "")),
           population_density = population / land_area)
}

results_global <- get_population_density()
  
# Note: we opted for not - again - retrieving the population and area information since we already
# retrieved this in the previous two functions, which took a long time to do so. It can easily be
# implemented in the current function though using:
  # results <- get_population_ranking()
  # results["land_area"] <- get_land_area(results$country_link)
  
# Question 4 --------------------------------------------------------------

# Create a function called get_rankings that will scrape the overview of all the available rankings.
# a. Use the two provided XPath queries to create a characteristic and a characteristic_link column.
# b. Remove the “../” from the characteristic_link column
# c. Remove the “:” from the characteristic column and put it in lowercase

#' Question 4: Get All Provided Rankings
#'
#' @return Tibble with 2 added columns: characteristic, and characteristic_link.
#'
#' @examples See code below.

get_rankings <- function() {
  xpath <- c("characteristic" = "//div[@class='field_label']/strong/a",
             "characteristic_link" = "//div[@class='field_label']/strong/a/@href")
  url <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  raw_html <- read_html(getURL(url, .encoding = "UTF-8"))
  
  results <- list()
  
  for (nm in names(xpath)) {
    results[[nm]] <- raw_html %>%
      xml_find_all(xpath[nm]) %>%
      as_list() %>% unlist()
  }
  
  results %>%
    as_tibble() %>%
    mutate(characteristic = tolower(str_replace_all(characteristic, ":", "")),
           characteristic_link = str_replace_all(characteristic_link, "\\.\\./", ""))
}

get_rankings()

# Question 5 --------------------------------------------------------------

# Create a get_ranking and get_country_characteristic function that are generalizations of the get_population_ranking and get_land_area functions.
# a. For get_ranking, renaming a column with a value from a variable can be done using special syntax
#     see: https://stackoverflow.com/questions/45472480/howto-rename-a-column-to-a-variable-name-in-a-tidyverse-way#45472543
# b. Note that the extra parameters have default values such that calling them without specifying any of the extra parameters 
#     should result in the same output as the get_population_ranking and get_land_area functions, respectively.

#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return Tibble with 4 added columns: country_link, country, population, and rank.population.
#'
#' @examples See code below.

get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url <- str_c(base_url, url)
  raw_html <- read_html(getURL(url, .encoding = "UTF-8"))
  
  results <- list()
  
  for (nm in names(xpath_expressions)) {
    results[[nm]] <- raw_html %>%
      xml_find_all(xpath_expressions[nm]) %>%
      as_list() %>% unlist()
  }
  
  results %>%
    as_tibble() %>%
    rename(!!characteristic := value,
           !!str_c("rank.", characteristic) := rank) %>%
    mutate(country_link = str_replace_all(country_link, "\\.\\./", ""))
}

get_ranking()

#Now try it out when specifying a parameter:

get_ranking(url = "fields/220rank.html", characteristic = "unemployment rate")

#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return Character vector with retrieved land area information from urls.
#'
#' @examples See code below.

get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  xpath <- str_c("//div[@id='",xpath_field_id,"']/div[",item,"]/span[2]")
  urls = str_c(base_url, country_link)
  
  results <- as_tibble(matrix(nrow = length(country_link), ncol = 1))
  
  for (i in seq_along(urls)) {
    raw_html <- read_html(getURL(urls[i], .encoding = "UTF-8"))
    results[i, 1] <- raw_html %>%
      xml_find_all(xpath) %>%
      as_list() %>% unlist()
  }
  return(results)
}

get_country_characteristic("geos/pk.html", item = 3) # Example Pakistans water sq km

# Question 6 --------------------------------------------------------------

# Create a combine_rankings function which will use the generalized get_ranking function to download multiple rankings.
# a. Use as input the rankings from get_rankings (both url and characteristic name)
# b. Combine (or ‘roll-up’) all rankings into a single data frame using full_join

#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return List(s) for the requested characteristic, joined if more than one
#'
#' @examples See code below.

combine_rankings <- function(rankings){
  
  results <- list()
  
  for(i in 1:nrow(rankings)) {
    char <- rankings[i,]$characteristic
    link <- rankings[i,]$characteristic_link
    ranking <- get_ranking(url = link, characteristic = char)
    results[[char]] <- select(ranking, country, char)
  }
  
  join_all(results, by = "country", type = "full")
}

combined_rankings <- combine_rankings(get_rankings()[1:10,]) # First 10 to check (max. is 77, i.e. all characteristics)
combined_rankings_gdp <- combine_rankings(filter(get_rankings(), str_detect(characteristic, "gdp"))) # Characteristics that have to do with gdp (gdp in name)