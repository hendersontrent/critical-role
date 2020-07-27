#---------------------------------------
# This script sets out to load and
# process the Critical Role data
#
# NOTE: Data from - 
# https://docs.google.com/spreadsheets/d/1FFuw5c6Hk1NUlHv2Wvr5b9AElLA51KtRl9ZruPU8r9k/edit#gid=134431706
#
# This script requires setup.R to have
# been run first
#---------------------------------------

#----------------------------------------
# Author: Trent Henderson, 27 July 2020
#----------------------------------------

# Import data from worksheets into a list

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

d <- read_excel_allsheets("data/All Rolls - Wildemount.xlsx")

# Remove unnecessary first worksheet

d1 <- d

d1 = d1[-1]

# Retain only columns I need as worksheets are uneven

keep <- c("Episode", "Character", "Type of Roll", "Total Value", "Natural Value")

d2 <- lapply(d1, function(x) subset(x, select = intersect(keep, colnames(x))))

# Pull dataframes from a list into 1 final dataframe for analysis
# Function source: https://stackoverflow.com/questions/55706560/make-rbindlist-skip-ignore-or-change-class-attribute-of-the-column

varnames <- names(d2[[1]]) # variable names
vattr <- purrr::map_chr(varnames, ~class(d2[[1]][[.x]])) # variable attributes

for (i in seq_along(d2)) {
  # assign the same attributes of list 1 to the rest of the lists
  for (j in seq_along(varnames)) {
    if (varnames[[j]]  %in% names(d2[[i]])) {
      class(d2[[i]][[varnames[[j]]]]) <- vattr[[j]]
    } 
  }
}

df_merged <- rbindlist(d2, fill = TRUE, use.names = TRUE) %>%
  clean_names()

#---------------------CLEANING--------------------------------------

clean <- df_merged %>%
  drop_na() %>%
  mutate(total_value = case_when(
    total_value == "Nat20" ~ "120",
    total_value == "Nat19" ~ "119",
    total_value == "Nat18" ~ "118",
    total_value == "Nat17" ~ "117",
    total_value == "Nat16" ~ "116",
    total_value == "Nat15" ~ "115",
    total_value == "Nat14" ~ "114",
    total_value == "Nat13" ~ "113",
    total_value == "Nat12" ~ "112",
    total_value == "Nat11" ~ "111",
    total_value == "Nat10" ~ "110",
    total_value == "Nat9"  ~ "109",
    total_value == "Nat8"  ~ "108",
    total_value == "Nat7"  ~ "107",
    total_value == "Nat6"  ~ "106",
    total_value == "Nat5"  ~ "105",
    total_value == "Nat4"  ~ "104",
    total_value == "Nat3"  ~ "103",
    total_value == "Nat2"  ~ "102",
    total_value == "Nat1"  ~ "101",
    TRUE                   ~ total_value)) %>%
  filter(total_value != "Unknown") %>%
  mutate(total_value = as.numeric(total_value)) %>%
  filter(total_value > 0) %>%
  mutate(episode = gsub("C2E", "", episode)) %>%
  mutate(episode = case_when(
    episode == "1519084800" ~ "020",
    episode == "1519171200" ~ "021",
    episode == "1519257600" ~ "022",
    episode == "1519344000" ~ "023",
    episode == "1519430400" ~ "024",
    episode == "1519516800" ~ "025",
    episode == "1519603200" ~ "026",
    episode == "1519689600" ~ "027",
    episode == "1519776000" ~ "028",
    TRUE                    ~ episode)) %>%
  mutate(episode = as.numeric(episode))

#---------------------STORAGE---------------------------------------

save(clean, file = "data/clean.Rda")
