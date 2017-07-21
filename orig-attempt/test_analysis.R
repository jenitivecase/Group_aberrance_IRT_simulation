library(rstan)

analysis <- readRDS("C:/Users/jennifer.brussow/Dropbox/Academic research projects/Group-level cheating detection/Group_aberrance_IRT_simulation/group_aberrance_IRT_test.rds")

params_summary <- summary(analysis, pars = c("a", "b", 
                                             "theta1", "theta2", "corr", 
                                             "group_inc", "indiv_err"),
                          probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary

mean(params_summary[grep("group_inc", rownames(params_summary)), 1])

mean(params_summary[grep("theta1", rownames(params_summary)), 1])
mean(params_summary[grep("theta2", rownames(params_summary)), 1])

mean(params_summary[grep("indiv_err", rownames(params_summary)), 1])

mean(params_summary[grep("corr", rownames(params_summary)), 1])

true_ability <- readRDS("C:/Users/jennifer.brussow/Dropbox/Academic research projects/Group-level cheating detection/Group_aberrance_IRT_simulation/true_ability_scores.rds")
true_items <- readRDS("C:/Users/jennifer.brussow/Dropbox/Academic research projects/Group-level cheating detection/Group_aberrance_IRT_simulation/true_item_params.rds")

yr1_items <- true_items[[1]]
yr2_items <- true_items[[2]]

all_items <- unique(rbind(yr1_items, yr2_items))

params <- extract(analysis, pars = c("a", "b", 
                                     "theta1", "theta2", "corr", 
                                     "group_inc", "indiv_err"))

a_params <- as.matrix(colMeans(params$a))
b_params <- as.matrix(colMeans(params$b))
theta1_params <- as.matrix(colMeans(params$theta1))
theta2_params <- as.matrix(colMeans(params$theta2))

all_a_params <- as.data.frame(cbind(all_items$itemid, all_items$a_param, a_params))
names(all_a_params) <- c("itemid", "true_param", "est_param")
all_a_params$type <- c(rep("test1", 50), rep("anchor", 10), rep("test2", 50))

all_b_params <- as.data.frame(cbind(all_items$itemid, all_items$b_param, b_params))
names(all_b_params) <- c("itemid", "true_param", "est_param")
all_b_params$type <- c(rep("test1", 50), rep("anchor", 10), rep("test2", 50))

all_theta_params <- as.data.frame(cbind(c(true_ability[, "yr1_ability"], true_ability[, "yr2_ability"]),
                          c(theta1_params, theta2_params)))
names(all_theta_params) <- c("true_ability", "est_ability")
all_theta_params$type <- c(rep("theta1", 6000), rep("theta2", 6000))

# pdf("bad_test_output.pdf")
ggplot(data = all_a_params, aes(x = true_param, y = est_param, color = type)) +
  geom_point() +
  ggtitle("A-parameter recovery")

ggplot(data = all_b_params, aes(x = true_param, y = est_param, color = type)) +
  geom_point() +
  ggtitle("B-parameter recovery")

ggplot(data = all_theta_params, aes(x = true_ability, y = est_ability, color = type)) +
  geom_point() +
  ggtitle("Theta recovery")

# dev.off()

cat("Max R-hat: ", max(params_summary[, "Rhat"]))
