library(tidyverse)
library(rvest)
library(assertthat)

rsw_release_page <- read_html("https://www.rstudio.com/products/rstudio/release-notes/")
rsc_release_page <- read_html("https://docs.rstudio.com/connect/news/")
rspm_release_page <- read_html("https://docs.rstudio.com/rspm/news/")




# RSW ---------------------------------------------------------------------



rswnotes <- rsw_release_page %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "mx-auto", " " ))]') %>%
  map(html_text) %>% 
  map(.f = function(x)  str_split(x, pattern = "\n") %>%
        unlist() %>%
        keep(.p = nchar(.) >= 1 & !str_detect(., "^\\s*$"))) %>% 
  unlist()

# get index for js script to remove and remove it
js_start <- (which(str_detect(rswnotes, "toggleNoteExpansion()")))
js_end <- max((which(str_detect(rswnotes, "EXPAND ALL"))))
assert_that(js_end > js_start)
rswnotes <- rswnotes[-c(js_start:js_end)]
assert_that(!any(str_detect(rswnotes, "toggleNoteExpansion()")))

# remove 2012 and older
#old_start <- (which(str_detect(notes, "^0.96.331.*")))
#notes <- notes[-c(old_start:length(notes))]

# trim leading spaces
rswnotes <- str_trim(rswnotes, side = "left")

# convert to tibble and add a column for version
rswnotes <- tibble(note = rswnotes)
rswnotes <- rswnotes %>% 
  mutate(
    version = if_else(str_detect(note, "^RStudio\\sv?[0-9]{1,4}\\.[0-9]{1,4}.*$"),
                      note,
                      NA_character_)
  ) %>% 
  fill(version)




# RSC ---------------------------------------------------------------------



rscnotes <- rsc_release_page %>% 
  html_nodes(xpath = '//h2 | //*[contains(concat( " ", @class, " " ), concat( " ", "md-typeset", " " ))]//li | //p') %>%
  map(html_text) %>% 
  map(~str_remove_all(., "[\n]")) %>% 
  map(str_trim) %>% 
  unlist() %>% 
  unique() %>% 
  str_replace_all("  ", " ")

# convert to tibble and add a column for version
rscnotes <- tibble(note = rscnotes) %>% 
  mutate(note = str_remove(note, "[¶#]$"))

rscnotes <- rscnotes %>% 
  mutate(
    version = if_else(str_detect(note, "^(RStudio Connect)?(Version)?\\sv?[0-9]{1,4}\\.[0-9]{1,4}.*$"),
                      note,
                      NA_character_)
  ) %>% 
  fill(version)

if(rscnotes[1,1] == "Introduction"){
  rscnotes <- rscnotes %>% slice(2:nrow(rscnotes))
}



# RSPM --------------------------------------------------------------------



rspmnotes <- rspm_release_page %>% 
  html_nodes(xpath = '//h2 | //*[contains(concat( " ", @class, " " ), concat( " ", "md-typeset", " " ))]//li | //p') %>%
  map(html_text) %>% 
  map(~str_remove_all(., "[\n]")) %>% 
  map(str_trim) %>% 
  unlist()

# convert to tibble and add a column for version
rspmnotes <- tibble(note = rspmnotes) %>% 
  mutate(note = str_remove(note, "[¶#]$"))

rspmnotes <- rspmnotes %>% 
  mutate(note = str_trim(note)) %>% 
  mutate(
    version = if_else(str_detect(note, "^(RStudio Package Manager)?(Version)?\\sv?[0-9.]*$"),
                      note,
                      NA_character_)
  ) %>% 
  fill(version)

if(str_detect(rspmnotes[1,1], "Introduction")){
  rspmnotes <- rspmnotes %>% slice(2:nrow(rspmnotes))
}



