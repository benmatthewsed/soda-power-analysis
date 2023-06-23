library(tidyverse)
library(sandwich)
library(lmtest)
source(here::here("script", "xx-functions.R"))
set.seed(nchar("soda power analysis") ^ 3)


# define parameters

soda_effect_size <- 0.1
# assumes larger soda at t1 gives larger soda at t2
# although we don't use this in the power calculations
# this is a small effect size - around 0.2-0.6 soda points
# (it depends on the location on the scale exactly how much)


miss_prop <- 0.25
# proportion of data at t2 (unrelated to t1 soda)

study_n <- 1000
# sample size

clusters_n <- 4
# number of recruitment conditions

cluster_effect_size <- 0.1
# this is the variance for the cluster random effect, again
# this isn't large (could be around a whole soda point either way)

id_effect_size <- 1
# this is between person variability and is very large
# so like 5 or 6 soda points


intervention_effect <- -1 
# this is change over time
# 


# to move people down three points is about one grade
# so try an effect of 0.5

# effect size of -1

# that's about 1 point

# soda is offered to everybody
# but not everyone will take it up
# control group are people in the service who haven't used the
# soda
# probalby larger

# so they don't use the soda at all

# does soda score reduce in the second sweep
# and 

# measure dosage at two timepoints

# lowest amount would have higher soda scores

# highest soda scores would be more stable

# so it's about change over time


tmp <- 
gen_soda_data(
  soda_effect_size = soda_effect_size,  
  miss_prop = miss_prop,
  study_n = study_n,
  clusters_n = 4,
  cluster_effect_size = 0.1,
  id_effect_size = 1,
  intervention_effect = -0.5
)

glmer_mod <- 
tmp |> 
  lme4::glmer.nb(soda ~ time + (1 | id), data = _)

broom.mixed::tidy(glmer_mod)$p.value[[2]]

glm_mod <- 
  tmp |> 
  MASS::glm.nb(soda ~ time, data = _)

broom::tidy(glm_mod)$p.value[[2]]


fit <- 
tmp |> 
  lm(soda ~ time, data = _)



tmp |> 
  filter(!is.na(soda)) |> 
  group_by(time) |> 
  summarise(mean_soda = mean(soda))

# intervention effect is 0.5 points


start <- Sys.time()


# estimate effect sizes ---------------------------------------------------





sims <- 
tibble(
  sim_n = seq(1:1000)
) |> 
  mutate(sim = map(sim_n, ~ gen_soda_data(
    soda_effect_size = soda_effect_size,  
    miss_prop = miss_prop,
    study_n = study_n,
    clusters_n = 4,
    cluster_effect_size = 0.1,
    id_effect_size = 1,
    intervention_effect = -0.1
  )))


sims |> 
  unnest(sim) |> 
  group_by(sim_n, time) |> 
  summarise(mean_soda = mean(soda, na.rm = TRUE)) |> 
  summarise(diff = mean_soda[time == 0] - mean_soda[time == 1]) |> 
  ggplot(aes(x = diff)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(diff)))

sims |> 
  unnest(sim) |> 
  mutate(soda_group = case_when(
    soda <= 3 ~ "Group 1",
    soda >3 & soda <=7 ~ "Group 2",
    soda >7 & soda <=14 ~ "Group 3",
    soda > 14 ~ "Group 4"
  )) |> 
  filter(!is.na(soda)) |> 
  pivot_wider(id_cols = c(sim_n, id),
              names_from = time,
              values_from = soda_group) |> 
  count(`0`, `1`) |> 
  filter(!is.na(`1`)) |> 
  group_by(`0`) |> 
  mutate(prop = n / sum(n))
  

# effect size of -0.1 would give ~20% in group 2 down to 1
# around 22% in group 3 down to 2 or 1
# and around 50% of people in group 4 dropping down
# note that some will also go up from group 1 to group 2 etc.
# and an average difference of around 0.4 points


sims <- 
  tibble(
    sim_n = seq(1:1000)
  ) |> 
  mutate(sim = map(sim_n, ~ gen_soda_data(
    soda_effect_size = soda_effect_size,  
    miss_prop = miss_prop,
    study_n = study_n,
    clusters_n = 4,
    cluster_effect_size = 0.1,
    id_effect_size = 1,
    intervention_effect = -0.25
  )))


sims |> 
  unnest(sim) |> 
  group_by(sim_n, time) |> 
  summarise(mean_soda = mean(soda, na.rm = TRUE)) |> 
  summarise(diff = mean_soda[time == 0] - mean_soda[time == 1]) |> 
  ggplot(aes(x = diff)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(diff)))

sims |> 
  unnest(sim) |> 
  mutate(soda_group = case_when(
    soda <= 3 ~ "Group 1",
    soda >3 & soda <=7 ~ "Group 2",
    soda >7 & soda <=14 ~ "Group 3",
    soda > 14 ~ "Group 4"
  )) |> 
  filter(!is.na(soda)) |> 
  pivot_wider(id_cols = c(sim_n, id),
              names_from = time,
              values_from = soda_group) |> 
  count(`0`, `1`) |> 
  filter(!is.na(`1`)) |> 
  group_by(`0`) |> 
  mutate(prop = n / sum(n))



# effect size of -0.25 would give ~27% in group 2 down to 1
# around 30% in group 3 down to 2 or 1
# and around 60% of people in group 4 dropping down
# and an average difference of around 0.8 points


sims <- 
  tibble(
    sim_n = seq(1:1000)
  ) |> 
  mutate(sim = map(sim_n, ~ gen_soda_data(
    soda_effect_size = soda_effect_size,  
    miss_prop = miss_prop,
    study_n = study_n,
    clusters_n = 4,
    cluster_effect_size = 0.1,
    id_effect_size = 1,
    intervention_effect = -0.5
  )))


sims |> 
  unnest(sim) |> 
  group_by(sim_n, time) |> 
  summarise(mean_soda = mean(soda, na.rm = TRUE)) |> 
  summarise(diff = mean_soda[time == 0] - mean_soda[time == 1]) |> 
  ggplot(aes(x = diff)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(diff)))

sims |> 
  unnest(sim) |> 
  mutate(soda_group = case_when(
    soda <= 3 ~ "Group 1",
    soda >3 & soda <=7 ~ "Group 2",
    soda >7 & soda <=14 ~ "Group 3",
    soda > 14 ~ "Group 4"
  )) |> 
  filter(!is.na(soda)) |> 
  pivot_wider(id_cols = c(sim_n, id),
              names_from = time,
              values_from = soda_group) |> 
  count(`0`, `1`) |> 
  filter(!is.na(`1`)) |> 
  group_by(`0`) |> 
  mutate(prop = n / sum(n))



# effect size of -0.5 would give ~39% in group 2 down to 1
# around 42% in group 3 down to 2 or 1
# and around 70% of people in group 4 dropping down
# and an average difference of around 1.7 points



sims <- 
  tibble(
    sim_n = seq(1:1000)
  ) |> 
  mutate(sim = map(sim_n, ~ gen_soda_data(
    soda_effect_size = soda_effect_size,  
    miss_prop = miss_prop,
    study_n = study_n,
    clusters_n = 4,
    cluster_effect_size = 0.1,
    id_effect_size = 1,
    intervention_effect = -1
  )))


sims |> 
  unnest(sim) |> 
  group_by(sim_n, time) |> 
  summarise(mean_soda = mean(soda, na.rm = TRUE)) |> 
  summarise(diff = mean_soda[time == 0] - mean_soda[time == 1]) |> 
  ggplot(aes(x = diff)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(diff)))

sims |> 
  unnest(sim) |> 
  mutate(soda_group = case_when(
    soda <= 3 ~ "Group 1",
    soda >3 & soda <=7 ~ "Group 2",
    soda >7 & soda <=14 ~ "Group 3",
    soda > 14 ~ "Group 4"
  )) |> 
  filter(!is.na(soda)) |> 
  pivot_wider(id_cols = c(sim_n, id),
              names_from = time,
              values_from = soda_group) |> 
  count(`0`, `1`) |> 
  filter(!is.na(`1`)) |> 
  group_by(`0`) |> 
  mutate(prop = n / sum(n))

# effect size of -1 would give ~60% in group 2 down to 1
# around 70% in group 3 down to 2 or 1
# and around 87% of people in group 4 dropping down
# and around 3.3 points difference on average




# note that I'm using glm.nb - it actually had a larger SE
# and so larger p-value than the glmer.nb

start <- Sys.time()

res <- 
tidyr::crossing(
  effect_size = c(-0.1, -0.25, -0.5),
  miss_prop = c(0.1, 0.25, 0.5),
  study_n = c(500, 1000, 1500)
) |> 
  mutate(res = pmap(list(
    a = effect_size,
    b = miss_prop,
    c = study_n),
    \(a, b, c,  ...) 
    run_power_soda(
               sim_n = 1000,
               soda_effect_size = soda_effect_size,  
               miss_prop = b,
               study_n = c,
               clusters_n = 4,
               cluster_effect_size = 0.1,
               id_effect_size = 1,
             intervention_effect = a)))

saveRDS(res,
        here::here("results",
                   "soda-power-analysis_1000.rds"))


res |> 
  unnest(res) |> 
  group_by(effect_size, miss_prop, study_n) |> 
  summarise(power = weighted.mean(signif, n)) |> 
  ungroup() |> 
  mutate(achieved_n = study_n * miss_prop) |> 
  ggplot(aes(x = achieved_n, 
             y = power, 
             colour = factor(effect_size))) +
  geom_hline(yintercept = 0.8) +
  geom_point(aes(size = factor(study_n))) +
  geom_line(aes(group = interaction(effect_size)))
  
  
end <- Sys.time()

end - start

res |> 
  unnest(res) |> 
  group_by(effect_size, miss_prop, study_n) |> 
  summarise(power = weighted.mean(signif, n)) |> 
  ungroup() |> 
  mutate(achieved_n = study_n * miss_prop) |> 
  filter(achieved_n == 250)

# 22 mins for 1000 sims

# 4 mins for 200 sims