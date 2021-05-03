
library(dyno)
library(tidyverse)



prosstt_linear_3 <- readRDS("~/Downloads/simulated_linear/prosstt/linear_3.rds")

dataset <- wrap_expression(
  counts = prosstt_linear_3$counts,
  expression = prosstt_linear_3$expression
)





set.seed(2)




model <- infer_trajectory(dataset, ti_scShaper())


model_1 <- model %>% add_dimred(dyndimred::dimred_tsne, expression_source = dataset$expression)
plot_dimred(
  model_1, 
  expression_source = dataset$expression,
  color_cells ="pseudotime"
)






model <- infer_trajectory(dataset, ti_tsneprincurve())



model_2 <- model %>% add_dimred(dyndimred::dimred_tsne, expression_source = dataset$expression)
plot_dimred(
  model_2, 
  expression_source = dataset$expression,
  color_cells ="pseudotime"
)












prosstt_linear_6 <- readRDS("~/Downloads/simulated_linear/prosstt/linear_6.rds")

dataset <- wrap_expression(
  counts = prosstt_linear_6$counts,
  expression = prosstt_linear_6$expression
)









model <- infer_trajectory(dataset, ti_scShaper())

# Use MDS here. The visualization looks better than t-SNE here.
model_1 <- model %>% add_dimred(dyndimred::dimred_mds, expression_source = dataset$expression)
plot_dimred(
  model_1, 
  expression_source = dataset$expression,
  color_cells ="pseudotime",
  waypoints = dynwrap::select_waypoints(model_1,n_waypoints = 100)
)






model <- infer_trajectory(dataset, ti_tsneprincurve())


# Use MDS here. The visualization looks better than t-SNE here.
model_2 <- model %>% add_dimred(dyndimred::dimred_mds, expression_source = dataset$expression)
plot_dimred(
  model_2, 
  expression_source = dataset$expression,
  color_cells ="pseudotime",
  waypoints = dynwrap::select_waypoints(model_2,n_waypoints = 100)
)


