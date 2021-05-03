library(dynwrap)
library(dyneval)
library(dynmethods)
library(tidyverse)
library(dyno)


set.seed(123456)


definition <- definition(
  method = def_method(
    id = "scShaper"
  ),
  parameters = def_parameters(
    dynparam::integer_parameter(
      id = "component",
      default = 1,
      distribution = dynparam::uniform_distribution(1, 10),
      description = "The nth component to use"
    )
  ),
  wrapper = def_wrapper(
    input_required = "expression",
    input_optional = "start_id"
  )
)


run_fun <- function(expression, priors, parameters, seed, verbose) {
  
  library(scShaper)
  library(SingleCellExperiment)
  
  suppressMessages(sce <- SingleCellExperiment(assays = list(counts = t(expression), logcounts = t(expression))))
  sce <- RunscShaper(sce,k.range = 2:100,nstart = 1,iter.max = 1000,pca.rank = 2,span = 0.10)
  
  
  pseudotime <- sce@metadata$scshaper$continuous.pseudotime
  
  names(pseudotime) <- rownames(expression)
  
  pseudotime <- (pseudotime-min(pseudotime))/(max(pseudotime)-min(pseudotime))
  
  # flip pseudotimes using start_id
  if (!is.null(priors$start_id)) {
    if(mean(pseudotime[start_id]) > 0.5) {
      pseudotime <- 1-pseudotime
    }
  }
  
  dynwrap::wrap_data(cell_ids = rownames(expression)) %>%
    dynwrap::add_linear_trajectory(pseudotime = pseudotime)
}


ti_scShaper <- create_ti_method_r(definition, run_fun, package_loaded = c("dplyr","scShaper","SingleCellExperiment"))



# 1000 cells

dataset <- dyntoy::generate_dataset(id="thousand_cells",model = "linear",num_features = 1000,num_cells = 1000)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_scShaper(),verbose = TRUE)


end.time <- Sys.time()
time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_1000_scshaper <- as.numeric(time.taken)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_comp1(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_1000_comp1 <- as.numeric(time.taken)

start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_elpilinear(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_1000_elpilinear <- as.numeric(time.taken)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_scorpius(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_1000_scorpius <- as.numeric(time.taken)




start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_embeddr(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_1000_embeddr <- as.numeric(time.taken)



barplot(c(run_time_1000_comp1,
          run_time_1000_elpilinear,
          run_time_1000_embeddr,
          run_time_1000_scorpius,
          run_time_1000_scshaper),names.arg = c("comp1","elpilinear","embeddr","scorpius","scshaper"))







# 5000 cells

dataset <- dyntoy::generate_dataset(id="five_thousand_cells",model = "linear",num_features = 1000,num_cells = 5000)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_scShaper(),verbose = TRUE)


end.time <- Sys.time()
time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_5000_scshaper <- as.numeric(time.taken)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_comp1(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_5000_comp1 <- as.numeric(time.taken)

start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_elpilinear(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_5000_elpilinear <- as.numeric(time.taken)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_scorpius(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_5000_scorpius <- as.numeric(time.taken)




start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_embeddr(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_5000_embeddr <- as.numeric(time.taken)



barplot(c(run_time_5000_comp1,
          run_time_5000_elpilinear,
          run_time_5000_embeddr,
          run_time_5000_scorpius,
          run_time_5000_scshaper),names.arg = c("comp1","elpilinear","embeddr","scorpius","scshaper"))







# 10000 cells

dataset <- dyntoy::generate_dataset(id="ten_thousand_cells",model = "linear",num_features = 1000,num_cells = 10000)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_scShaper(),verbose = TRUE)


end.time <- Sys.time()
time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_10000_scshaper <- as.numeric(time.taken)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_comp1(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_10000_comp1 <- as.numeric(time.taken)

start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_elpilinear(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_10000_elpilinear <- as.numeric(time.taken)


start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_scorpius(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_10000_scorpius <- as.numeric(time.taken)




start.time <- Sys.time()

model <- infer_trajectory(dataset, ti_embeddr(),verbose = TRUE)


end.time <- Sys.time()

time.taken <- round(difftime(end.time,start.time,units = "secs"),2)
run_time_10000_embeddr <- as.numeric(time.taken)



barplot(c(run_time_10000_comp1,
          run_time_10000_elpilinear,
          run_time_10000_embeddr,
          run_time_10000_scorpius,
          run_time_10000_scshaper),names.arg = c("comp1","elpilinear","embeddr","scorpius","scshaper"))



save(dataset,file = "dyntoy_dataset_10k_cells.RData")


save(run_time_1000_comp1,run_time_1000_elpilinear,
     run_time_1000_embeddr,run_time_1000_scorpius,run_time_1000_scshaper,
     run_time_5000_comp1,run_time_5000_elpilinear,
     run_time_5000_embeddr,run_time_5000_scorpius,run_time_5000_scshaper,
     run_time_10000_comp1,run_time_10000_scshaper,file = "scShaper_run_time_comparison.RData")
