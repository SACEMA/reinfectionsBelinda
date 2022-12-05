library(ggplot2)
library(ggtext)
method <- 3

load(paste0('method_',method,'_analysis/combined_results.RDS'))


lambda_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_2, y = pobs_1, fill=lambda_con)
                    + geom_tile()
                    + scale_fill_gradientn(colours = terrain.colors(10))
                    + ggtitle(paste0('Lambda Convergence Sensitivity Analysis: Method ', method))
)
ggsave(lambda_con_plot, filename=paste0('method_',method,'_analysis/plots/lambda_convergence.png'))


kappa_con_plot <- (ggplot(final_RDS)
                   + aes(x = pobs_2, y = pobs_1, fill=kappa_con)
                   + geom_tile()
                   + scale_fill_gradientn(colours = terrain.colors(10))
                   + ggtitle(paste0('Kappa Convergence Sensitivity Analysis: Method ', method))
)
ggsave(kappa_con_plot, filename=paste0('method_',method,'_analysis/plots/kappa_convergence.png'))

pobs1_pobs_2_proportion <- (ggplot(final_RDS) 
  + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
  + geom_tile()
  + facet_wrap(~pscale)
  + ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs proportion of points outside prediction interval'))
  + theme(plot.title = element_textbox_simple())
)
ggsave(pobs1_pobs_2_proportion, filename=paste0('method_',method,'_analysis/plots/pobs1_pobs_2_proportion.png'))


pobs1_pobs2_first_cluster <- (ggplot(final_RDS) 
  + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit) 
  + geom_tile()
  + facet_wrap(~pscale)
  + ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs first cluster of points outside prediction interval AFTER wavesplit'))
  + theme(plot.title = element_textbox_simple())

)
ggsave(pobs1_pobs2_first_cluster, filename=paste0('method_',method,'_analysis/plots/pobs1_pobs2_first_cluster.png'))



pobs1_pscale_proportion <- (ggplot(final_RDS) 
                            + aes(x=pobs_1, y=pscale, fill=proportion_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pobs_2)
                            + ggtitle(paste0('Method ', method, ': pobs 1 and pscale vs proportion of points outside prediction interval'))
                            + theme(plot.title = element_textbox_simple())
)
ggsave(pobs1_pscale_proportion, filename=paste0('method_',method,'_analysis/plots/pobs1_pscale_proportion.png'))



pobs1_pscale_first_cluster <- (ggplot(final_RDS) 
                            + aes(x=pobs_1, y=pscale, fill=date_first_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pobs_2)
                            + ggtitle(paste0('Method ', method, ': pobs 1 and pscale vs  first cluster of points outside prediction interval AFTER wavesplit'))
                            + theme(plot.title = element_textbox_simple())
)
ggsave(pobs1_pscale_first_cluster, filename=paste0('method_',method,'_analysis/plots/pobs1_pscale_first_cluster.png'))


pobs2_pscale_proportion <- (ggplot(final_RDS) 
                            + aes(x=pobs_2, y=pscale, fill=proportion_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pobs_1)
                            + ggtitle(paste0('Method ', method, ': pobs 2 and pscale vs proportion of points outside prediction interval'))
                            + theme(plot.title = element_textbox_simple())
)
ggsave(pobs2_pscale_proportion, filename=paste0('method_',method,'_analysis/plots/pobs2_pscale_proportion.png'))



pobs2_pscale_first_cluster <- (ggplot(final_RDS) 
                               + aes(x=pobs_2, y=pscale, fill=date_first_after_wavesplit) 
                               + geom_tile()
                               + facet_wrap(~pobs_1)
                               + ggtitle(paste0('Method ', method, ': pobs 2 and pscale vs  first cluster of points outside prediction interval AFTER wavesplit'))
                               + theme(plot.title = element_textbox_simple())
)
ggsave(pobs2_pscale_first_cluster, filename=paste0('method_',method,'_analysis/plots/pobs2_pscale_first_cluster.png'))


