

library(ggplot2)
methods <- c(1,2,3,4,'5a','5b','5c')

#plots pscale vs proprtion
for (method in methods) { 
  plot_data <- readRDS(paste0('pscale_analysis/output/',method,'_pscale_analysis.RDS'))
  next_plot <- ggplot(data=plot_data, aes(x=pscale, y=proportion, group=1)) +
    geom_line()+
    geom_point() + 
    ggtitle(paste0('Method ', method)) +
    ylim(0,1)
  ggsave(next_plot, file=paste0('pscale_analysis/output/plots/', method, '_proportion.png'))
}

#plots pscale vs first day out
for (method in methods) { 
  plot_data <- readRDS(paste0('pscale_analysis/output/',method,'_pscale_analysis.RDS'))
  next_plot <- ggplot(data=plot_data, aes(x=pscale, y=first_cluster, group=1)) +
    geom_line()+
    geom_point() +
    ggtitle(paste0('Method ', method)) + 
    ylim(0,70)
  ggsave(next_plot, file=paste0('pscale_analysis/output/plots/', method, '_first_cluster.png'))
}

