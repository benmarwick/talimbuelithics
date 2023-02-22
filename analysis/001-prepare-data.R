library(tidyverse)
library(readxl)

# previous work
# - https://gist.github.com/benmarwick/5693206#file-wi_14_archy_299_lithics-r
# - lab notes: https://docs.google.com/document/d/1d3Q7VbyS-wHhS3hw6JRykNymjQeVlxuNMMOtxIe8Ev8/edit#
# - data: https://docs.google.com/spreadsheets/d/1WZxwyetJGQagg5rDk9rLBi3AhfwbT6i75qAWD4qfkF0/edit#gid=9
# - readings: https://canvas.uw.edu/courses/874528/files/folder/PDFs%20for%20the%20lab%20report

# import data

# Marwick's data
bm_data_counts <-
  read_excel(here::here("data/data-marwick/Lithics Research Group Data Sheet AU13 WI14 & SP14.xlsx"))

bm_data_complete_flakes_all <-
  read_excel(here::here("data/data-marwick/Lithics Research Group Data Sheet AU13 WI14 & SP14.xlsx"),
             sheet = "Complete flakes")

bm_data_retouch <-
  read_excel(here::here("data/data-marwick/Lithics Research Group Data Sheet AU13 WI14 & SP14.xlsx"),
             sheet = "retouch")

# Bulbeck's data
db_data_complete_flakes_spits_31_50_all <-
  read_excel(here::here("data/data-bulbeck/Talimbue_B_complete_flakes_B31_B64_analysed.xlsx"),
             sheet = "spits 31-50 all")

db_data_complete_flakes_51_down_other <-
  read_excel(here::here("data/data-bulbeck/Talimbue_B_complete_flakes_B31_B64_analysed.xlsx"),
             sheet = "51 down other")

db_data_complete_flakes_51_down_retouch <-
  read_excel(here::here("data/data-bulbeck/Talimbue_B_complete_flakes_B31_B64_analysed.xlsx"),
             sheet = "51 down retouch")

db_data_broken_flakes_spits_31_50_all <-
  read_excel(here::here("data/data-bulbeck/Talimbue_B_broken_flakes_spits_31_64_analysed.xlsx"),
             sheet = "All spits 31 to 50")

db_data_broken_flakes_51_down_retouch <-
  read_excel(here::here("data/data-bulbeck/Talimbue_B_broken_flakes_spits_31_64_analysed.xlsx"),
             sheet = "51 down retouched")

db_data_cores <-
  read_excel(here::here("data/data-bulbeck/Talimbue_B_cores analysed.xlsx"),
             sheet = "All cores")

db_data_retouch <-
  read_excel(here::here("data/data-bulbeck/Talimbue_B_retouched.xlsx"),
             sheet = "Total")

##  Combine

# complete flakes
tb_complete_flakes <-
full_join(db_data_complete_flakes_51_down_other,
          db_data_complete_flakes_spits_31_50_all) %>%
full_join(db_data_complete_flakes_51_down_retouch) %>%
full_join(bm_data_complete_flakes_all %>%
              mutate(Specimen = as.character(Specimen),
                     `% Cortex (increments of 10%)` = as.character(`% Cortex (increments of 10%)`)),
            by = c("ID" = "Specimen",
            "Spit" = "Spit",
            "Oriented Length (mm)" = "Length (Percussion Axis)",
            "Oriented Width (mm)" = "Width (Medial)",
            "Oriented Thick (mm)" = "Thickness (Medial)",
            "Weight (Gram)" = "Mass (g)",
            "Striking Platform Type" = "Platform Type (plain, multiple, faceted, focalised, cortical, crushed)",
            "Striking Flat Form Width (mm)" = "Platform Width",
            "Striking Flat Form Thickness (mm)" = "Platform Thickness",
            "Material" = "Raw Material",
            "Termination" = "Termination Type (Feather, Hinge, Overshot)",
            "Cortex Persentation" = "% Cortex (increments of 10%)",
            "Dorsal Scars" = "Number of Dorsal Scars"))

# table(tb_complete_flakes$`Striking Platform Type`) shows many unclear data points
# show how to us case_when to harmonise them
# each of the categorical variables will need this procedure

# show how to add a column that is the grouping column for the
# three main phases


# retouch
tb_retouch <-
  full_join(db_data_retouch %>%
              mutate(Spit = as.numeric(Spit),
                     `Maksimal Length( mm)` = as.numeric(`Maksimal Length( mm)`)),
            bm_data_retouch %>%
              mutate(`Maximum Dimension` = as.numeric(`Maximum Dimension`),
                     `Mass (g)` = as.numeric(`Mass (g)`)),
            by = c(
              "ID" = "Specimen # (must begin with RT)"   ,
              "Class" = "Type (complete, redirecting, bipolar, unclear)",
              "Spit" = "Spit" ,
              "Maksimal Length( mm)" = "Maximum Dimension" ,
              "Weight (Gram)" = "Mass (g)"  ,
              "Material" ="Raw Material (see https://docs.google.com/a/uw.edu/document/d/1d3Q7VbyS-wHhS3hw6JRykNymjQeVlxuNMMOtxIe8Ev8/edit#bookmark=id.ynxhk0xvizbp)"
            ))


# cores
tb_cores <- db_data_cores # I didn't record any, it seems?


# Analysis plan

# Analysis coverage

# Counts and weights of artefact classes by spit

# Raw material counts and weights by artefact class

# Core types by raw materials

# Core metrics by raw materials

# Complete Flake types by raw materials

# Complete Flake metrics and attributes by raw materials

# Retouch types by raw materials

# Retouch metrics and attributes by raw materials


