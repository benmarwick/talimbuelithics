

# make the number ID unique
tb_retouch$number <- make.unique(as.character(tb_retouch$ID),
                                 sep = "_")

retouch_giur_per_section <-
  tb_retouch %>%
  dplyr::select(grep("^number$|_t|_T", names(tb_retouch))) %>%
  pivot_longer(-number) %>%
  separate(name, c("zone", "tee"), sep = "_") %>%
  pivot_wider(names_from =  tee, values_from = value) %>%
  mutate(t_T = t / T)  %>%
  filter(t_T <= 1)

retouch_giur_per_artefact <-
retouch_giur_per_section %>%
  group_by(number) %>%
  summarise(mean_giur = mean(t_T, na.rm = TRUE)) %>%
  left_join(tb_retouch)

# example of exploring some categorical variables over time
retouch_giur_per_artefact %>%
  group_by(`Notch or nose?`, Spit)  %>%
  tally

# overall distribution of GIUR
ggplot(retouch_giur_per_artefact) +
  aes(mean_giur) +
  geom_histogram()

# GIUR by spit
ggplot(retouch_giur_per_artefact) +
  geom_boxplot( aes(as.factor(Spit),
                    mean_giur)) +
  geom_smooth(aes(Spit,
                  mean_giur)) +
  theme_minimal()

# GIUR by depositional_phase
ggplot(retouch_giur_per_artefact) +
  geom_boxplot( aes(as.factor(depositional_phase),
                    mean_giur)) +
  theme_minimal()
