setwd("C:/Users/jennifer.brussow/Documents/Group_aberrance_IRT_simulation")
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

library(tidyverse)
library(rstan)
library(ggplot2)


files <- as.vector(list.files(getwd(), recursive = TRUE))

fit_files <- files[grep("fit", files)]
true_files <- files[grep("true", files)]

files <- data.frame(file = c(fit_files, true_files))

for(i in 1:nrow(files)){
  filename_temp <- as.character(files[i, "file"])
  filename_temp <- gsub("^[^\\/]*\\/", "", filename_temp)
  info <- unlist(strsplit(filename_temp, "_"))
  len <- length(info)
  condition <- paste0(c(info[(len-4):(len-1)]), collapse = "_")
  type <- info[1]
  
  files[i, "condition"] <- condition
  files[i, "type"] <- type
}

conditions <- unique(files$condition)
types <- unique(files$type)

types_conditions <- expand.grid(conditions, types)
names(types_conditions) <- c("conditions", "types")
types_conditions <- apply(types_conditions, 2, as.character)
uniques <- unique(types_conditions[, "conditions"])

if(!dir.exists("combined")){dir.create("combined")}

for(i in 1:length(uniques)){
  condition_matches <- grep(types_conditions[i, "conditions"], files$file)
  type_matches <- grep(paste0(types_conditions[i, "types"]), files$file)
  indices <- condition_matches[which(condition_matches %in% type_matches)]
  set_files <- files[indices, "file"]
  
  out <- NA
  
  for(j in 1:length(set_files)){
    output <- readRDS(set_files[j])
    output <- output[!sapply(output, is.null)] 
    out <- append(out, output)
  }
  
  #removes the initial NA
  out <- out[c(2:length(out))]
  
  #discards down to 100 reps :(
  out <- out[1:100]

  fname <- paste0(types_conditions[i, "types"], "_", length(out), "reps_", types_conditions[i, "conditions"], ".rds")
  saveRDS(out, paste0("combined/", fname))
  
}
