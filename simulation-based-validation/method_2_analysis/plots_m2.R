library(ggplot2)
library(ggtext)
method <- 2

final_RDS <- readRDS(paste0('method_',method,'_analysis/combined_results.RDS'))

#plots useless as different pscale don't affect convergence
lambda_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_2, y = lambda_con)
                    + geom_line()
                    + ggtitle(paste0('Lambda Convergence Sensitivity Analysis: Method ', method))
)
lambda_con_plot

kappa_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_2, y = kappa_con)
                    + geom_line()
                    + ggtitle(paste0('Kappa Convergence Sensitivity Analysis: Method ', method))
)
kappa_con_plot

pscale_proportion_plot <- (ggplot(final_RDS) 
            + aes(x=pscale, y=proportion, group=pobs_2, color= factor(pobs_2)) 
            + geom_line()
            + labs(color="Pobs 2", y='Proportion')
            + ggtitle(paste0('Method ', method, ' pscale vs proportion of points outside prediction interval'))
            + ylim(0,1)
            + theme(plot.title = element_textbox_simple())
)
pscale_proportion_plot

pscale_proportion_plot_aw <- (ggplot(final_RDS) 
                           + aes(x=pscale, y=proportion_after_wavesplit, group=pobs_2, color= factor(pobs_2)) 
                           + geom_line()
                           + labs(color="Pobs 2", y='Proportion after wavesplit')
                           + ggtitle(paste0('Method ', method, ' pscale vs proportion of points outside prediction interval AFTER wavesplit'))
                           + ylim(0,1)
                           + theme(plot.title = element_textbox_simple())
                           
)
pscale_proportion_plot_aw

cluster_proportion_plot <- (ggplot(final_RDS) 
                           + aes(x=pscale, y=date_first, group=pobs_2, color= factor(pobs_2)) 
                           + geom_line()
                           + labs(color="Pobs 2", y='Proportion')
                           + ggtitle(paste0('Method ', method, ' pscale vs first cluster of points outside prediction interval'))
                           + ylim(50,100)
                           + theme(plot.title = element_textbox_simple())
)
cluster_proportion_plot

cluster_proportion_plot_aw <- (ggplot(final_RDS) 
                              + aes(x=pscale, y=date_first_after_wavesplit, group=pobs_2, color= factor(pobs_2)) 
                              + geom_line()
                              + labs(color="Pobs 2", y='Proportion after wavesplit')
                              + ggtitle(paste0('Method ', method, ' pscale vs first cluster of points outside prediction interval AFTER wavesplit'))
                              + ylim(5,10)
                              + theme(plot.title = element_textbox_simple())
                              
)
cluster_proportion_plot_aw

pobs_2_proportion <- (ggplot(final_RDS) 
                         + aes(x=pobs_2, y=proportion, group=pscale, color= factor(pscale)) 
                         + geom_line()
                         + labs(color="Pscale", y='Proportion')
                         + ggtitle(paste0('Method ', method, ' pobs 2 vs proportion of points outside prediction interval'))
                         + ylim(0,1)
                         + theme(plot.title = element_textbox_simple())
                         
)
pobs_2_proportion


pobs_2_proportion_aw <- (ggplot(final_RDS) 
                               + aes(x=pobs_2, y=proportion_after_wavesplit, group=pscale, color= factor(pscale)) 
                               + geom_line()
                               + labs(color="Pscale", y='Proportion after wavesplit')
                               + ggtitle(paste0('Method ', method, ' pobs 2 vs proportion of points outside prediction interval AFTER wavesplit'))
                               + ylim(0,1)
                               + theme(plot.title = element_textbox_simple())
                               
)
pobs_2_proportion_aw

#Save plots
ggsave(pobs_2_proportion_aw , filename=paste0('method_',method,'_analysis/plots/pobs_2_proportion_aw.png'))
ggsave(pobs_2_proportion, filename=paste0('method_',method,'_analysis/plots/pobs_2_proportion.png'))
ggsave(pscale_proportion_plot_aw, filename=paste0('method_',method,'_analysis/plots/pscale_proportion_aw.png'))
ggsave(pscale_proportion_plot, filename=paste0('method_',method,'_analysis/plots/pscale_proportion.png'))
ggsave(lambda_con_plot, filename=paste0('method_',method,'_analysis/plots/lambda_convergence.png'))
ggsave(kappa_con_plot, filename=paste0('method_',method,'_analysis/plots/kappa_convergence.png'))
ggsave(cluster_proportion_plot_aw, filename=paste0('method_',method,'_analysis/plots/cluster_proportion_aw.png'))
ggsave(cluster_proportion_plot, filename=paste0('method_',method,'_analysis/plots/cluster_proportion.png'))

