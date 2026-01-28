# Run function change_inequality_final.R and diff_ineq_final.R 

library(boot)
library(broom)

# Run models:
ineq_fun(dat = df, inds = 1:nrow(df), outcome = "mental_health", mediator = "outdoor_play", 
         ineq_group = "maternal_education", covars = c("sex", "ethnicity", "age", "history_mental_health"), interaction = TRUE,
         set_mediator_to = "outdoor_play>=14", 
         params = list(adjustment = c("age"), ineq_group = "maternal_education", outcome = "mental_health")) 

# Obtain 95% Confidence Intervals using bootstrapping
boot_out <- boot(data = df, statistic = ineq_fun, outcome = "mental_health", mediator = "outdoor_play",
                 ineq_group = "maternal_education", covars = c("sex", "ethnicity", "age", "history_mental_health"), interaction = TRUE,
                 set_mediator_to = "outdoor_play>=14", 
                 R = 1000, 
                 params = list(adjustment = c("age"), ineq_group = "maternal_education", outcome = "mental_health"))

# Combine estimates with 95% Confidence Intervals
tidy(boot_out, conf.int = TRUE)
