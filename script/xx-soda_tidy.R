library(tidyverse)


source(here::here("script", "xx-functions.R"))
# define parameters


soda_effect_size <- 0.1

miss_prop <- 0.11

study_n <- 1000

clusters_n <- 4

cluster_effect_size <- 0.1

id_effect_size <- 1

intervention_effect <- -1 # this is just change over time


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

gen_soda_data(
  soda_effect_size = soda_effect_size,  
  miss_prop = miss_prop,
  study_n = study_n,
  clusters_n = 4,
  cluster_effect_size = 0.1,
  id_effect_size = 1,
  intervention_effect = -1
)  |> 
  group_by(ctrl) |> 
  summarise(mean_t2 = mean(soda_t2, na.rm = TRUE))

# intervention effect is 0.5 points


start <- Sys.time()

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
    intervention_effect = intervention_effect
  )))


sims |> 
  unnest(sim) |> 
  group_by(sim_n, ctrl) |> 
  summarise(mean_t2 = mean(soda_t2, na.rm = TRUE)) |> 
  group_by(sim_n) |> 
  summarise(diff = mean_t2[ctrl == 1] - mean_t2[ctrl == 0]) |> 
  ggplot(aes(x = diff)) +
  geom_histogram()

sims |> 
  unnest(sim) |> 
  mutate(soda_t2_group = case_when(
    soda_t2 <= 3 ~ "Group 1",
    soda_t2 >3 & soda_t1 <=7 ~ "Group 2",
    soda_t2 >7 & soda_t1 <=14 ~ "Group 3",
    soda_t2 > 14 ~ "Group 4"
  )) |> 
  filter(!is.na(soda_t2_group)) |> 
  count(soda_t2_group) |> 
  mutate(prop = n / sum(n))
  
sims |> 
  unnest(sim) |> 
  filter(!is.na(soda_t2)) |> 
  count(soda_t1_group) |> 
  mutate(prop = n / sum(n))


start <- Sys.time()

sims |> 
  mutate(model = map(sim, model_fit)) |> 
  unnest(model) |> 
  filter(term == "ctrl") |> 
  mutate(signif = if_else(p.value < 0.05, 1, 0)) |> 
  count(signif)
  
# but only 34% power


sims |> 
  unnest(sim) |> 
  group_by(sim_n, ctrl) |> 
  summarise(mean_t2 = mean(soda_t2, na.rm = TRUE))

# effect size is around 0.4 of a point

end <- Sys.time()

end - start

# so... around 1 minute per condition?

# with 0.1 effect size and 11% retention we have 4 % power





sims_2 <- 
  tibble(
    sim_n = seq(1:1000)
  ) |> 
  mutate(sim = map(sim_n, ~ gen_soda_data(
    soda_effect_size = soda_effect_size,  
    miss_prop = 0.5,
    study_n = study_n,
    clusters_n = 4,
    cluster_effect_size = 0.1,
    id_effect_size = 1,
    intervention_effect = intervention_effect
  )))


sims_2 |> 
  mutate(model = map(sim, model_fit)) |> 
  unnest(model) |> 
  filter(term == "ctrl") |> 
  mutate(signif = if_else(p.value < 0.05, 1, 0)) |> 
  count(signif)

# with 50% retention and 0.1 effect size we get 10% power

run_power_soda(sim_n = 1000,
               soda_effect_size =  soda_effect_size,  
                miss_prop = 0.5,
                study_n = study_n,
                 clusters_n =  4,
                 cluster_effect_size =  0.1,
               id_effect_size =  1,
                intervention_effect = intervention_effect)


start <- Sys.time()

# add what these are back in

# set up parallel processing

plan(multisession, workers = 2)

res <- 
tibble(
  effect_size = c(0.1, 0.25, 0.5)
) |> 
  mutate(res = map(effect_size, 
             ~ run_power_soda(
               sim_n = 1000
               soda_effect_size = soda_effect_size,  
               miss_prop = 0.3,
               study_n = study_n,
               clusters_n = 4,
               cluster_effect_size = 0.1,
               id_effect_size = 1,
             .x)))

res |> 
  unnest(res)

end <- Sys.time()

end - start