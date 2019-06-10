## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 5
)

## ---- eval=FALSE---------------------------------------------------------
#  library(rubias)
#  library(tidyverse)

## ---- echo=FALSE---------------------------------------------------------
# this is what we actually evaluate.
library(rubias)

# all the following libraries can be loaded with "library(tidyverse)"
# but then you have to put tidyverse in the Suggests because this is
# in the vignette, and that is bad practice, so, load the packages separately...
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

## ----test_cond, echo=FALSE-----------------------------------------------
set.seed(15)
suppressMessages(cond_model <- infer_mixture(
  reference = small_chinook_ref, 
  mixture = small_chinook_mix, 
  gen_start_col = 5, 
  method = "MCMC"
  ))

## ----test_br-------------------------------------------------------------
full_model <- infer_mixture(
  reference = small_chinook_ref, 
  mixture = small_chinook_mix, 
  gen_start_col = 5, 
  method = "BR"
  )

## ----view_theta----------------------------------------------------------
full_model$allele_frequencies

## ----compare-------------------------------------------------------------
set.seed(15)
cond_model <- infer_mixture(
  reference = small_chinook_ref, 
  mixture = small_chinook_mix, 
  gen_start_col = 5, 
  method = "MCMC"
  )

comppi <- cond_model$mixing_proportions %>%
  mutate(cond_pi = pi, full_pi = full_model$mixing_proportions$pi)

ggplot(comppi, aes(x = cond_pi, y = full_pi, colour = collection)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~mixture_collection) +
  theme(legend.position = "bottom")
  


## ----full_prelim---------------------------------------------------------

set.seed(15)
prelim_full_model <- infer_mixture(
  reference = small_chinook_ref, 
  mixture = small_chinook_mix, 
  gen_start_col = 5, 
  prelim_reps = 2000,
  prelim_burn_in = 100,
  method = "BR"
  )

prelimpi <-  prelim_full_model$mix_prop_traces %>%
  filter(sweep == 0) %>%
  select(-sweep) %>%
  mutate(prelim_pi = pi, full_pi = prelim_full_model$mixing_proportions$pi)

ggplot(prelimpi, aes(x = prelim_pi, y = full_pi, colour = collection)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~mixture_collection) +
  theme(legend.position = "bottom")


## ----setcores------------------------------------------------------------
RcppParallel::setThreadOptions(numThreads = 1)

full_model <- infer_mixture(
  reference = small_chinook_ref, 
  mixture = small_chinook_mix, 
  gen_start_col = 5, 
  method = "BR"
  )


## ----resetcores----------------------------------------------------------
RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())

