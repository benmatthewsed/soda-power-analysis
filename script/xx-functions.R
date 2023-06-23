

gen_soda_data <- function(
    soda_effect_size,  
    miss_prop,
    study_n,
    clusters_n = 4,
    cluster_effect_size = 0.1,
    id_effect_size = 1,
    intervention_effect
){
  
  cluster_effects <- rnorm(study_n, 0, cluster_effect_size)
  
  
  dat <- 
    tibble(
      id = seq(1, study_n)
    ) |> 
    mutate(cluster_effect = sample(cluster_effects, study_n, replace = TRUE),
           id_effect = rnorm(study_n, 0, id_effect_size),
           soda_t1 = map2_dbl(cluster_effect, id_effect, ~ rbinom(1,
                                                                  18,
                                                                  plogis(-0.1 + .x + .y))))

  dat <- 
    dat |> 
    mutate(
      soda_t2 = pmap_dbl(list(a = cluster_effect,
                              b = id_effect,
                              c = intervention_effect),
                         \(a, b, c,  ...) 
                         rbinom(1,
                                18,
                                plogis(-0.1 + a + b + c)),
      ))
  
  
  dat <- 
    dat |> 
    mutate(miss_prob = runif(n = n()),
           soda_t2 = if_else(miss_prob > miss_prop, NA_real_, soda_t2))
  
  dat |> 
    select(id, contains("soda")) |> 
    pivot_longer(cols = -id,
                 values_to = "soda",
                 names_to = "time") |> 
    mutate(time = if_else(time == "soda_t1",
                          0,
                          1))

}



model_fit <- function(dat){
  
  MASS::glm.nb(soda ~ time, data = dat) |> broom::tidy()
  
}



run_power_soda <- function(
    sim_n,
    soda_effect_size,  
    miss_prop,
    study_n,
    clusters_n = 4,
    cluster_effect_size = 0.1,
    id_effect_size = 1,
    intervention_effect){
  
  sims <- 
    tibble(
      sim_n = seq(1:sim_n)
    ) |> 
    mutate(sim = map(sim_n, ~ gen_soda_data(
      soda_effect_size = soda_effect_size,  
      miss_prop = miss_prop,
      study_n = study_n,
      clusters_n = clusters_n,
      cluster_effect_size = cluster_effect_size,
      id_effect_size = id_effect_size,
      intervention_effect = intervention_effect
    )))
  
  
  sims |> 
    mutate(model = map(sim, model_fit)) |> 
    unnest(model) |> 
    filter(term == "time") |> 
    mutate(signif = if_else(p.value < 0.05, 1, 0)) |> 
    count(signif)
  
  
  
}



plot_effect_size <- function(sims){
  sims |> 
    unnest(sim) |> 
    group_by(sim_n, time) |> 
    summarise(mean_soda = mean(soda, na.rm = TRUE)) |> 
    summarise(diff = mean_soda[time == 0] - mean_soda[time == 1]) |> 
    ggplot(aes(x = diff)) +
    geom_histogram() +
    geom_vline(aes(xintercept = mean(diff))) +
    labs(caption = "Sold line is average effect size")
  
}



calc_group_change <- function(sims){
  
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
  
}