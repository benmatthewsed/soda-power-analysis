library(MASS)
library(tidyverse)
library(simstudy)


set.seed(nchar("soda") ^ 6)

# these were just trial and error

# at the moment there are too many in group 1 and not enough in group 4


settings <- c("community drug team",
              "prison estates",
              "primary care",
              "community pharmacy")

n_val <- 184

second_sweep <- 21

retention <- 21 / 184

tibble(
  soda = rnegbin(n = 184,
        mu = 6.5,
        theta = 5)
) |> 
  mutate(soda_t1 = if_else(soda > 18, 18, soda),
         group = case_when(
           soda <= 3 ~ "Group 1",
           soda >3 & soda <=7 ~ "Group 2",
           soda >7 & soda <=14 ~ "Group 3",
           soda > 14 ~ "Group 4"
         )) |> 
  select(-soda) |> 
  mutate(setting = sample(settings,
                          n_val,
                          replace = TRUE
                          ),
         treat = sample(c(0, 1),
                        n_val,
                        replace = TRUE),
         soda_t2 = exp(- 0.2 + treat * -1 + log(soda_t1) * 0.5 + treat * soda_t1 * -0.05))






def <- defData(varname = "nr", dist = "nonrandom", formula = 7, id = "idnum")
def <- defData(def, varname = "x1", dist = "uniform", formula = "10;20")
def <- defData(def, varname = "y1", formula = "nr + x1 * 2", variance = 8)
def <- defData(def, varname = "y2", dist = "poisson", formula = "nr - 0.2 * x1", 
               link = "log")
def <- defData(def, varname = "xCat", formula = "0.3;0.2;0.5", dist = "categorical")
def <- defData(def, varname = "g1", dist = "gamma", formula = "5+xCat", variance = 1, 
               link = "log")
def <- defData(def, varname = "a1", dist = "binary", formula = "-3 + xCat", 
               link = "logit")


dt <- genData(1000, def)


def <- defData(varname = "male", dist = "binary", formula = 0.5, id = "cid")
def <- defData(def, varname = "over65", dist = "binary", formula = "-1.7 + .8*male", 
               link = "logit")
def <- defData(def, varname = "baseDBP", dist = "normal", formula = 70, variance = 40)

dtstudy <- genData(330, def)


# setting
# baseline soda
# treatment


tdef <- defData(varname = "T", dist = "binary", formula = 0.5)
tdef <- defData(tdef, varname = "Y0", dist = "normal", formula = 10, variance = 1)
tdef <- defData(tdef, varname = "Y1", dist = "normal", formula = "Y0 + 5 + 5 * T", 
                variance = 1)

dtTrial <- genData(500, tdef)

dtTime <- addPeriods(dtTrial, nPeriods = 2, idvars = "id", timevars = c("Y0", 
                                                                        "Y1"),
                     timevarName = "Y")



