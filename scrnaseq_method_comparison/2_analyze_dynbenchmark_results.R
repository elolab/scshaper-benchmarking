load("~/dynverse_comparison_results_/dynverse_comparison_results_comp1.RData")
results_comp1 <- results
results_comp1 <- lapply(names(results_comp1),function(x) {X <- results_comp1[[x]] ; X$file <- x ; if (unlist(strsplit(x,"/"))[6]=="real") {y <- "real"} else {y <- unlist(strsplit(x,"/"))[7]} ; X$type <- y ; return(X)})
results_comp1 <- lapply(1:length(results_comp1),function(x) {X <- results_comp1[[x]] ; y <- sqrt(X$correlation * X$featureimp_wcor) ; X$overall <- y ; return(X)})
results_comp1 <- do.call(rbind,results_comp1)
results_comp1 <- reshape2::melt(results_comp1)
load("~/dynverse_comparison_results_/dynverse_comparison_results_elpilinear.RData")
results_elpilinear <- results
results_elpilinear <- lapply(names(results_elpilinear),function(x) {X <- results_elpilinear[[x]] ; X$file <- x ; if (unlist(strsplit(x,"/"))[6]=="real") {y <- "real"} else {y <- unlist(strsplit(x,"/"))[7]} ; X$type <- y ; return(X)})
results_elpilinear <- lapply(1:length(results_elpilinear),function(x) {X <- results_elpilinear[[x]] ; y <- sqrt(X$correlation * X$featureimp_wcor) ; X$overall <- y ; return(X)})
results_elpilinear <- do.call(rbind,results_elpilinear)
results_elpilinear <- reshape2::melt(results_elpilinear)
load("~/dynverse_comparison_results_/dynverse_comparison_results_embeddr.RData")
results_embeddr <- results
results_embeddr <- lapply(names(results_embeddr),function(x) {X <- results_embeddr[[x]] ; X$file <- x ; if (unlist(strsplit(x,"/"))[6]=="real") {y <- "real"} else {y <- unlist(strsplit(x,"/"))[7]} ; X$type <- y ; return(X)})
results_embeddr <- lapply(1:length(results_embeddr),function(x) {X <- results_embeddr[[x]] ; y <- sqrt(X$correlation * X$featureimp_wcor) ; X$overall <- y ; X$method <- "embeddr" ; return(X)})
results_embeddr <- do.call(rbind,results_embeddr)
results_embeddr <- reshape2::melt(results_embeddr)
load("~/dynverse_comparison_results_/dynverse_comparison_results_scorpius.RData")
results_scorpius <- results
results_scorpius <- lapply(names(results_scorpius),function(x) {X <- results_scorpius[[x]] ; X$file <- x ; if (grepl("real",x)) {y <- "real"} else {y <- unlist(strsplit(x,"/"))[2] ; y <- unlist(strsplit(y,"_"))[1]} ; X$type <- y ; return(X)})
results_scorpius <- lapply(1:length(results_scorpius),function(x) {X <- results_scorpius[[x]] ; y <- sqrt(X$correlation * X$featureimp_wcor) ; X$overall <- y ; X$method <- "scorpius" ; return(X)})
results_scorpius <- do.call(rbind,results_scorpius)
results_scorpius <- reshape2::melt(results_scorpius)
load("~/dynverse_comparison_results_/dynverse_comparison_results_scshaper.RData")
results_scshaper <- results
results_scshaper <- lapply(names(results_scshaper),function(x) {X <- results_scshaper[[x]] ; X$file <- x ; if (grepl("real",x)) {y <- "real"} else {y <- unlist(strsplit(x,"/"))[2] ; y <- unlist(strsplit(y,"_"))[1]} ; X$type <- y ; return(X)})
results_scshaper <- lapply(1:length(results_scshaper),function(x) {X <- results_scshaper[[x]] ; y <- sqrt(X$correlation * X$featureimp_wcor) ; X$overall <- y ; X$method <- "scShaper" ; return(X)})
results_scshaper <- do.call(rbind,results_scshaper)
results_scshaper <- reshape2::melt(results_scshaper)


results <- rbind(results_comp1,results_elpilinear,results_embeddr,results_scorpius,results_scshaper)
results <- split(results,results$variable)


tests <- NULL
for (variable in names(results)) {
  results_variable <- results[[variable]]
  
  methods <- names(table(results_variable$method))
  methods <- methods[!grepl("scShaper",methods)]
  for (method in methods)
  {
    data1 <- results_variable[results_variable$method=="scShaper",]
    data2 <- results_variable[results_variable$method==method,]
    
    if (nrow(data2) < nrow(data1))
    {
      datasetnames2 <- data2$file
      datasetnames2[grepl("real",datasetnames2)] <- lapply(datasetnames2[grepl("real",datasetnames2)],function(x) paste(unlist(strsplit(x,"/"))[6:7],collapse = "_"))
      datasetnames2[!grepl("real",datasetnames2)] <- lapply(datasetnames2[!grepl("real",datasetnames2)],function(x) paste(unlist(strsplit(x,"/"))[7:8],collapse = "_"))
      
      datasetnames1 <- data1$file
      datasetnames1[grepl("real",datasetnames1)] <- lapply(datasetnames1[grepl("real",datasetnames1)],function(x) unlist(strsplit(x,"/"))[2])
      datasetnames1[!grepl("real",datasetnames1)] <- lapply(datasetnames1[!grepl("real",datasetnames1)],function(x) unlist(strsplit(x,"/"))[2])
      
      datasetnames2 <- unlist(datasetnames2)
      datasetnames1 <- unlist(datasetnames1)
      
      data1 <- data1[datasetnames1%in%datasetnames2,]
      
    }
    
    pwt_out <- wilcox.test(data1$value, data2$value, paired = TRUE, alternative = "greater")
    tests <- rbind(tests,c(method,variable,pwt_out$p.value))
  }
  
}
tests <- as.data.frame(tests)
View(tests)
tests$V3 <- as.numeric(tests$V3)


library(ggplot2)
p_list <- lapply(c("correlation","featureimp_wcor","overall"),function(x) ggplot(results[[x]],aes(x=method,y=value,fill=method))+geom_violin(trim=TRUE)+geom_boxplot(width=0.1,fill='white') + theme_classic(base_size = 20)+ggtitle(x)+ylab(x)+scale_fill_brewer(palette="Set3")+theme(axis.text.x = element_text(angle = 90)))
p <- cowplot::plot_grid(plotlist = p_list,ncol = 3)
cowplot::ggsave2(filename = "~/scShaper_violinplots.pdf",plot = p,width = 18,height = 5)



p_list <- lapply(c("overall"),function(x) ggplot(results[[x]],aes(x=type,y=value,fill=method))+geom_boxplot() + theme_classic(base_size = 20)+ggtitle(x)+ylab(x)+scale_fill_brewer(palette="Set1")+theme(axis.text.x = element_text(angle = 90)))
p <- cowplot::plot_grid(plotlist = p_list,ncol = 1)
cowplot::ggsave2(filename = "~/scShaper_violinplots_2.pdf",plot = p,width = 18,height = 5)



p_list <- lapply(names(results)[1:12],function(x) ggplot(results[[x]],aes(x=type,y=value,fill=method))+geom_boxplot() + theme_classic(base_size = 20)+ggtitle(x)+ylab(x)+scale_fill_brewer(palette="Set1")+theme(axis.text.x = element_text(angle = 90)))
p <- cowplot::plot_grid(plotlist = p_list,ncol = 3)
cowplot::ggsave2(filename = "~/scShaper_benchmarking_vnlplots_supplementary_part_1.pdf",plot = p,width = 18,height = 20)
cowplot::ggsave2(filename = "~/scShaper_benchmarking_vnlplots_supplementary_part_1.png",plot = p,width = 18,height = 20,dpi = 150,units = "in")

p_list <- lapply(names(results)[13:24],function(x) ggplot(results[[x]],aes(x=type,y=value,fill=method))+geom_boxplot() + theme_classic(base_size = 20)+ggtitle(x)+ylab(x)+scale_fill_brewer(palette="Set1")+theme(axis.text.x = element_text(angle = 90)))
p <- cowplot::plot_grid(plotlist = p_list,ncol = 3)
cowplot::ggsave2(filename = "~/scShaper_benchmarking_vnlplots_supplementary_part_2.pdf",plot = p,width = 18,height = 20)
cowplot::ggsave2(filename = "~/scShaper_benchmarking_vnlplots_supplementary_part_2.png",plot = p,width = 18,height = 20,dpi = 150,units = "in")

p_list <- lapply(names(results)[25:31],function(x) ggplot(results[[x]],aes(x=type,y=value,fill=method))+geom_boxplot() + theme_classic(base_size = 20)+ggtitle(x)+ylab(x)+scale_fill_brewer(palette="Set1")+theme(axis.text.x = element_text(angle = 90)))
p <- cowplot::plot_grid(plotlist = p_list,ncol = 3)
cowplot::ggsave2(filename = "~/scShaper_benchmarking_vnlplots_supplementary_part_3.pdf",plot = p,width = 18,height = 15)
cowplot::ggsave2(filename = "~/scShaper_benchmarking_vnlplots_supplementary_part_3.png",plot = p,width = 18,height = 15,dpi = 150,units = "in")




ggplot(tests, aes(V1, V2, fill= V3)) + 
  geom_tile(aes(fill = V3)) + 
  geom_text(aes(label = round(V3, 4))) +
  scale_fill_gradient(low = "white", high = "red") +
  ylab("P-value") +
  xlab("Method") +
  labs(fill = "P-value")


