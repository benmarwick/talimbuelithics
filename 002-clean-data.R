
# Clean the categorical variables for the complete flakes, first we explore to see
# what needs cleaning, then we clean

# which variables are categorical? There are the ones that exist in both DB's and BM's data,
# so let's focus on these

# "Striking Platform Type"
# "Material"
# "Termination"
# "Cortex Persentation"

#----------------------------------------------------
# working on "Striking Platform Type"

# explore them...
tb_complete_flakes %>%
  group_by(`Striking Platform Type`) %>%
  tally(sort = TRUE)

# we see many meaningless duplicates due to upper case/lower case, different spelling, etc.
# let's fix them...

tb_complete_flakes_clean <-  # note that now we are using tb_complete_flakes_clean
tb_complete_flakes %>%
  mutate(`Striking Platform Type` = str_to_lower(`Striking Platform Type` ))

# take a look again to see the improvement...
tb_complete_flakes_clean %>%
  group_by(`Striking Platform Type`) %>%
  tally(sort = TRUE)

# fix some of the spelling mistakes
tb_complete_flakes_clean <-
  tb_complete_flakes_clean %>%
  mutate(`Striking Platform Type` = case_when(
    `Striking Platform Type` == "faceted striking plat from" ~ "faceted",
    `Striking Platform Type` == "cortical striking platform" ~ "cortical",
    `Striking Platform Type` == "flat striking platform" ~ "plain",
    `Striking Platform Type` == "focalized" ~ "focalised",
    `Striking Platform Type` == "muliple" ~ "multiple",
    TRUE ~ `Striking Platform Type`))

# take a look yet again to see the improvement...
tb_complete_flakes_clean %>%
  group_by(`Striking Platform Type`) %>%
  tally(sort = TRUE) # looks good!

#----------------------------------------------------
# working on "Material"

# explore them...
tb_complete_flakes_clean %>%
  group_by(Material) %>%
  tally(sort = TRUE)

# from BM's notes: RC = red chert, BC = brown chert, BKC = black chert, GRC = green chert.

# fix some of the spelling mistakes
tb_complete_flakes_clean <-
  tb_complete_flakes_clean %>%
  mutate(Material = case_when(
    Material == "RC" ~ "Chert",
    Material == "BC" ~ "Chert",
    Material == "GC" ~ "Chert",
    Material == "WC" ~ "Chert",
    Material == "BLC" ~ "Chert",
    Material == "YC" ~ "Chert",
    Material == "GC&YC" ~ "Chert",
    Material == "QZ" ~ "Quartz",
    Material == "OT" ~ "Other",
    Material == "SD" ~ "Other",
    Material == "Andesit" ~ "Andesite",
    is.na(Material)  ~ "Other",
    TRUE ~ Material))

# take a look yet again to see the improvement...
tb_complete_flakes_clean %>%
  group_by(Material) %>%
  tally(sort = TRUE) # looks good!

#----------------------------------------------------
# working on "Termination"

# explore them...
tb_complete_flakes_clean %>%
  group_by(Termination) %>%
  tally(sort = TRUE)

# from BM's notes: RC = red chert, BC = brown chert, BKC = black chert, GRC = green chert.

# fix some of the spelling mistakes
tb_complete_flakes_clean <-
  tb_complete_flakes_clean %>%
  mutate(Termination= str_to_lower(Termination)) %>%
  mutate(Termination = case_when(
    Termination == "hiinge" ~ "hinge",
    TRUE ~ Termination))

# take a look yet again to see the improvement...
tb_complete_flakes_clean %>%
  group_by(Termination) %>%
  tally(sort = TRUE) # looks good!

#----------------------------------------------------
# working on "Cortex Persentation"

# explore them...
tb_complete_flakes_clean %>%
  group_by(`Cortex Persentation`) %>%
  tally(sort = TRUE)

# put them into similar categories
tb_complete_flakes_clean <-
  tb_complete_flakes_clean %>%
  mutate(`Cortex amount`  = case_when(
    `Cortex Persentation` == 0   ~ "None",
    `Cortex Persentation` %in% c(0.1, 0.2, 10:25)  ~ "1-25%",
    `Cortex Persentation` %in% c(0.3, 0.4, 0.5, 30:50)  ~ "26-50%",
    `Cortex Persentation` %in% c(0.6, 0.7, 0.8, 60:80)  ~ "51-80%",
    `Cortex Persentation` %in% c(0.9, 1, 90:100)  ~ "90-100%",
    TRUE ~ `Cortex Persentation`)) %>%
  mutate(`Cortex amount` = factor(`Cortex amount`,
                                  levels = c("None",
                                             "1-25%",
                                             "26-50%",
                                             "51-75%",
                                             "90-100%")))

# take a look yet again to see the improvement...
tb_complete_flakes_clean %>%
  group_by(`Cortex amount`) %>%
  tally(sort = TRUE) # looks good!

#----------------------------------------------------
# add phases for the cleaned complete flakes

# From https://doi.org/10.15184/aqy.2019.134

# These three pulses of deposition, c.
# 4100–4500 cal BP (spits 10–21, 0.50–1m depth),
# 6500–7500 cal BP (spits 25–45, 1.30–2.30m depth) and
# 9500–9900 cal BP (spits 56–64, 2.90–3.20m)

# add phase to spit

tb_complete_flakes_clean <-
  tb_complete_flakes_clean %>%
  mutate(depositional_phase = case_when(
    between(Spit, 10, 21) ~ "dep_phase_3",
    between(Spit, 25, 45) ~ "dep_phase_2",
    between(Spit, 56, 64) ~ "dep_phase_1",
    TRUE ~ "no_phase"
  ))

# keep only artefacts in phases and drop those not in phases
tb_complete_flakes_clean_phase_only <-
  tb_complete_flakes_clean %>%
  filter(str_detect(depositional_phase, "dep_phase_"))

# add phases to retouch
tb_retouch <-
tb_retouch %>%
  mutate(depositional_phase = case_when(
    between(Spit, 10, 21) ~ "dep_phase_3",
    between(Spit, 25, 45) ~ "dep_phase_2",
    between(Spit, 56, 64) ~ "dep_phase_1",
    TRUE ~ "no_phase"
  ))

# add phases to cores
tb_cores <-
tb_cores %>%
  mutate(depositional_phase = case_when(
    between(Spit, 10, 21) ~ "dep_phase_3",
    between(Spit, 25, 45) ~ "dep_phase_2",
    between(Spit, 56, 64) ~ "dep_phase_1",
    TRUE ~ "no_phase"
  ))

#----
tb_complete_flakes_clean <-
  tb_complete_flakes_clean %>%
  mutate(pca_group = case_when(
    Spit %in% c(1, 2, 5, 6, 7, 8, 9, 10, 11) ~ "pca_1",
    Spit %in% c(3, 17, 18, 20, 22, 23) ~ "pca_2",
    TRUE ~ "no_pca_group"
  ))


# ------
bm_data_counts <-
  bm_data_counts %>%
  filter(!is.na(Spit)) %>%
  replace(is.na(.), 0)

