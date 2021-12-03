args <- commandArgs(trailingOnly=TRUE)

cat(args, sep = "\n")

benchmarking_file <- args[1]
output_folder <- args[2]

print(benchmarking_file)
print(output_folder)

library(dynwrap)
library(dyneval)
library(tidyverse)

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
    
    set.seed(2)
    
    
    library(SingleCellExperiment)
    library(scShaper)
    # X <- Rtsne::Rtsne(as.matrix(expression),dims=3,perplexity = 30)$Y
    
    suppressMessages(sce <- SingleCellExperiment(assays = list(counts = t(expression),logcounts = t(expression))))
    sce <- RunscShaper(sce,k.range = 2:100,nstart = 1,iter.max = 1000,span=0.10,pca.rank = 2,dim.red.method = "tsne")
    pseudotime <- sce@metadata$scshaper$continuous.pseudotime
    
    names(pseudotime) <- rownames(expression)
    
    # flip pseudotimes using start_id
    if (!is.null(priors$start_id)) {
        if(mean(pseudotime[start_id]) > 0.5) {
            pseudotime <- 1-pseudotime
        }
    }
    
    dynwrap::wrap_data(cell_ids = rownames(expression)) %>%
        dynwrap::add_linear_trajectory(pseudotime = pseudotime)
}


ti_scShaper <- create_ti_method_r(definition, run_fun, package_loaded = c("dplyr","scShaper"))

dataset <- readRDS(benchmarking_file)

models <- infer_trajectories(dataset,list(ti_scShaper()
),verbose = TRUE)

message("1. trajectory inference... DONE")


dataset <- add_cell_waypoints(dataset)
models$model <- map(models$model, add_cell_waypoints)

message("2. add waypoints... DONE")

metric_ids <- dyneval::metrics %>% pull(metric_id)
metrics <- map_dfr(models$model, dyneval::calculate_metrics, dataset = dataset, metrics = metric_ids)

message("3. calculate metrics... DONE")

metrics$method <- c("scShaper")


file_name <- unlist(strsplit(benchmarking_file,"/"))[c(length(unlist(strsplit(benchmarking_file,"/")))-1,
                                          length(unlist(strsplit(benchmarking_file,"/"))))]

output_file <- paste0(output_folder,"/",paste(file_name,collapse = "_"))

dir.create(output_folder,recursive=TRUE)

saveRDS(metrics,file = output_file)










