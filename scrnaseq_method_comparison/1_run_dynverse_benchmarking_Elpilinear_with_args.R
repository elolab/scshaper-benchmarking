args <- commandArgs(trailingOnly=TRUE)

cat(args, sep = "\n")

benchmarking_file <- args[1]
output_folder <- args[2]

print(benchmarking_file)
print(output_folder)

library(dynwrap)
library(dyneval)
library(tidyverse)
library(dynmethods)
library(dyno)

dataset <- readRDS(benchmarking_file)

models <- infer_trajectory(dataset, ti_elpilinear(),verbose = TRUE)

message("1. trajectory inference... DONE")


dataset <- add_cell_waypoints(dataset)
models$model <- map(models$model, add_cell_waypoints)

message("2. add waypoints... DONE")

metric_ids <- dyneval::metrics %>% pull(metric_id)
metrics <- map_dfr(models$model, dyneval::calculate_metrics, dataset = dataset, metrics = metric_ids)

message("3. calculate metrics... DONE")

metrics$method <- c("Elpilinear")


file_name <- unlist(strsplit(benchmarking_file,"/"))[c(length(unlist(strsplit(benchmarking_file,"/")))-1,
                                          length(unlist(strsplit(benchmarking_file,"/"))))]

output_file <- paste0(output_folder,"/",paste(file_name,collapse = "_"))

dir.create(output_folder,recursive=TRUE)

saveRDS(metrics,file = output_file)










