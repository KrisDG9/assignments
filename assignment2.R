#assignment2

rm(list = ls())

library(dplyr)
library(lintr)
library(stringr)
library(tidyverse)

# Question 1 ------------------------------------------------------------------------------------------------------

# Create a function called tidy_df with two input parameters, the first a data frame, the
# second a column prefix. This function should automatically tidy the data frame by
# gathering all columns that start with the given column prefix.

# For example, if a data frame has column var1, var2, and var3, this function should add a
# column “variable” containing either “var1”, “var2”, or “var3” and a column “value” that
# contains the value that previously was in the “var1”, “var2”, or “var3” column.

# Be sure to leave columns that do not start with the given prefix as is.

# Function:

tidy_df <- function(data, column_prefix = "var"){
  gather(data, starts_with(column_prefix), key = "variable", value = "value")
}

#Checking whether this runs properly:

df <- data_frame(id = c(1,2,3,4,5),
var = c("a","d","g","f","i"),
a1 = c(3,5,1,2,2),
b1 = c(2,4,1,2,3),
a2 = c(8,1,2,5,1),
b2 = c(1,6,4,7,2),
a3 = c(7,7,2,3,1),
b3 = c(1,1,4,9,6))

tidy_df(df, "a")

# Question 2 ------------------------------------------------------------------------------------------------------

# Create a function called extract_possible_names that uses a regular expression 
# to extract all words that start with a capital letter. Use the function 
# get_jane_austen_data to get a data frame of textual data.

# Your function should return a data frame with one row per extracted name and
# a. A column “id”, a unique identifier for this name
# b. A column “text_id” which is the “id” from the original data frame to link back
# c. A column “name” which is the extracted name

# The tidy_df function you created before might come in handy here.

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row

get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}

get_jane_austen_data()

# extract_possible_names 

extract_possible_names <- function(data, reg) {
  data %>%
    rename(text_id = id) %>%                      # text_id is the id from the original df to link back
    mutate(name = str_extract_all(text, reg)) %>% # extract (capitalised words)
    unnest(name) %>%                              # replacement of tidy_df
    mutate(id = row_number()) %>%                 # adding the unique identifier
    select(id, text_id, name, title)              # keep the title (needed in q4)
}

possible_names <- extract_possible_names(austen_text, "[A-Z][a-z]+")

possible_names

# Note: 
  # It was unclear to me whether the "unique identifier" should be per name or per name incidence.
  # I opted for providing an id per incidence since that is more "unique".

# Question 3 ------------------------------------------------------------------------------------------------------

# Unfortunately, your function also extracts words that are capitalised because they are
# the first word of the sentence. Create a function called filter_names to filter out those
# words. For this task, you can use a data frame of word frequencies, given in austen_word_freqs.Rds. 
# Keep only names that appear capitalised at least 75% of the time.

# filter_names

filter_names <- function(possible) {
  
  freqs <- readRDS(austen_word_freqs, refhook = NULL)
  
  filtered_summary <- possible %>%
    mutate(name_lower = tolower(name)) %>%                # names to lowercase, because freqs is lowercase
    group_by(name_lower) %>%                              # do everything per name
    summarise(name = name[1], caps = n()) %>%             # keep capitalised name, count number of capitalised occurrences
    inner_join(freqs, by = c("name_lower" = "word")) %>%  # combine summary (cols: name_lower, name, caps) with matching rows from freqs (col: total count)
    filter(caps / count >= 0.75)                          # only keep those that are capitalised at least 75% of the time
  
  semi_join(possible, filtered_summary, by = "name")      # match possible names with filtered list from earlier, not copying columns over from the summary
}

names <- filter_names(possible_names)

names # comment on output (both obs diff between names and possible_names and by eyeballing the data [bijv. maanden, sir, etc.]) and stating that the q4 approach is therefore not optimal [maybe suggest improvement])

# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
count_names_per_book <- function(names_list) {
  names_list %>%
    group_by(title) %>% # alle operaties per boek
    summarise(unique_names = length(unique(name)), name_occurrences = n()) # optional after this: filter out non-books
}

names_per_book <- count_names_per_book(names)
names_per_book

max_unique_names <- which.max(names_per_book$unique_names[1:8]) # limit to 'real' books here?
paste0('Book with most unique names: ', names_per_book$title[max_unique_names], ' (', names_per_book$unique_names[max_unique_names], ')')

max_unique_names <- names_per_book[which.max(names_per_book$unique_names[1:8]),]
paste0('Book with most unique names: ', max_unique_names$title, ' (', max_unique_names$unique_names, ')')

max_name_occurrences <- which.max(names_per_book$name_occurrences[1:8])
paste0('Book with most name occurrences: ', names_per_book$title[max_name_occurrences], ' (', names_per_book$name_occurrences[max_name_occurrences], ')')

max_name_occurrences <- names_per_book[which.max(names_per_book$name_occurrences[1:8]),]
paste0('Book with most name occurrences: ', max_name_occurrences$title, ' (', max_name_occurrences$name_occurrences, ')')
