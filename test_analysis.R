analysis <- readRDS("C:/Users/jennifer.brussow/Desktop/group_aberrance_IRT_test.rds")

params_summary <- summary(analysis, pars = c("a_yr1", "b_yr1", "a_yr2", "b_yr2", 
                                             "theta1", "theta2", "corr", 
                                             "group_inc", "indiv_err"),
                          probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary
