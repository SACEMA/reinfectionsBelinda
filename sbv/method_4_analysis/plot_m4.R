library(ggplot2)
library(ggtext)
method <- 4

dir <- paste0('sbv/method_', method,'_analysis/plots')

final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/combined_results.RDS'))

dir.create(dir)


styling_layers <- 
  list(
    #scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
    theme(panel.border = element_rect(colour = "black", size = 0.25)
    , panel.grid.minor = element_blank()) 
    , theme_minimal() 
    , scale_colour_brewer(palette = "Set2") 
    , scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
    , scale_x_continuous(breaks=seq(0.1, 0.5, 0.2))
    , scale_y_continuous(breaks=seq(0.1, 0.5, 0.2))
  )

lambda_con_plot <- (ggplot(final_RDS)
           + aes(x = pobs_1, y = pobs_2, fill = lambda_con)
           + geom_tile()
           + facet_wrap(~dprob)
           + ggtitle('Lambda Convergence Sensitivity Analysis')
           + labs(
             y='Observation probability reinfections',
             x='Observation probability primary infections',
             fill='Lambda convergence'
           )
           + styling_layers
           
)
ggsave(lambda_con_plot, filename=paste0(dir,'/lambda_density.png'), device="png")


kappa_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_1, y = pobs_2, fill = kappa_con)
                    + geom_tile()
                    + facet_wrap(~dprob)
                    + ggtitle('Kappa Convergence Sensitivity Analysis')
                    + labs(
                     y='Observation probability reinfections',
                     x='Observation probability primary infections',
                     fill='Kappa convergence'
                    )
                    + styling_layers
)
ggsave(kappa_con_plot, filename=paste0(dir,'/kappa_density.png'), device="png")

pobs1_pobs_2_proportion_0.001 <- (ggplot(final_RDS[final_RDS$dprob==0.001,]) 
                            + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pscale)
                            + ggtitle(paste0('Method ', method, ':  Observation probabilities and proportion \noutside projection interval for death probability 0.001'))
                            + theme(plot.title = element_textbox_simple())
                            + labs(
                              y='Observation probability reinfections',
                              x='Observation probability primary infections',
                              fill='Proportion'
                            )
                            + styling_layers
                            
)
ggsave(pobs1_pobs_2_proportion_0.001, filename=paste0(dir,'/pobs1_pobs_2_proportion_0.001.png'))

pobs1_pobs_2_proportion_0.01 <- (ggplot(final_RDS[final_RDS$dprob==0.01,]) 
                                  + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
                                  + geom_tile()
                                  + facet_wrap(~pscale)
                                  + ggtitle(paste0('Method ', method, ': Observation probabilities and proportion \noutside projection interval for death probability 0.01'))
                                  + theme(plot.title = element_textbox_simple())
                                  + labs(
                                   y='Observation probability reinfections',
                                   x='Observation probability primary infections',
                                   fill='Proportion'
                                 )
                                   + styling_layers
)
ggsave(pobs1_pobs_2_proportion_0.01, filename=paste0(dir,'/pobs1_pobs_2_proportion_0.01.png'))

pobs1_pobs_2_proportion_0.05 <- (ggplot(final_RDS[final_RDS$dprob==0.05,]) 
                                 + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
                                 + geom_tile()
                                 + facet_wrap(~pscale)
                                 + ggtitle(paste0('Method ', method, ': Observation probabilities and proportion \noutside projection interval for death probability 0.05'))
                                 + theme(plot.title = element_textbox_simple())
                                 + labs(
                                   y='Observation probability reinfections',
                                   x='Observation probability primary infections',
                                   fill='Proportion'
                                 )
                                 + styling_layers
)
ggsave(pobs1_pobs_2_proportion_0.05, filename=paste0(dir,'/pobs1_pobs_2_proportion_0.05.png'))



pobs1_pobs_2_date_first_0.001 <- (ggplot(final_RDS[final_RDS$dprob==0.001,]) 
                                  + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit) 
                                  + geom_tile()
                                  + facet_wrap(~pscale)
                                  + ggtitle(paste0('Method ', method, ': Observation probabilities and first date \noutside projection interval for death probability 0.001'))
                                  + theme(plot.title = element_textbox_simple())
                                  + labs(
                                    y='Observation probability reinfections',
                                    x='Observation probability primary infections',
                                    fill='First day'
                                  )
                                  + styling_layers
)
ggsave(pobs1_pobs_2_date_first_0.001, filename=paste0(dir,'/pobs1_pobs_2_date_first_0.001.png'))

pobs1_pobs_2_date_first_0.01 <- (ggplot(final_RDS[final_RDS$dprob==0.01,]) 
                                 + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit) 
                                 + geom_tile()
                                 + facet_wrap(~pscale)
                                 + ggtitle(paste0('Method ', method, ': Observation probabilities and first date \noutside projection interval for death probability 0.01'))
                                 + theme(plot.title = element_textbox_simple())
                                 + labs(
                                   y='Observation probability reinfections',
                                   x='Observation probability primary infections',
                                   fill='First day'
                                 )
                                 + styling_layers
)
ggsave(pobs1_pobs_2_date_first_0.01, filename=paste0(dir,'/pobs1_pobs_2_date_first_0.01.png'))

pobs1_pobs_2_date_first_0.05 <- (ggplot(final_RDS[final_RDS$dprob==0.05,]) 
                                 + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit) 
                                 + geom_tile()
                                 + facet_wrap(~pscale)
                                 + ggtitle(paste0('Method ', method, ': Observation probabilities and first date \noutside projection interval for death probability 0.05'))
                                 + theme(plot.title = element_textbox_simple())
                                 + labs(
                                   y='Observation probability reinfections',
                                   x='Observation probability primary infections',
                                   fill='First day'
                                 )
                                 + styling_layers
)
ggsave(pobs1_pobs_2_date_first_0.05, filename=paste0(dir, '/pobs1_pobs_2_date_first_0.05.png'))


pobs1_pobs_2_proportion_pscale_1 <- (ggplot(final_RDS[final_RDS$pscale==1,]) 
                                 + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
                                 + geom_tile()
                                 + facet_wrap(~dprob)
                                 + ggtitle(paste0('Method ', method, ': Observation probabilities and proportion \noutside projection interval for pscale 1'))
                                 + theme(plot.title = element_textbox_simple())
                                 + labs(
                                   y='Observation probability reinfections',
                                   x='Observation probability primary infections',
                                   fill='Proportion'
                                 )
                                 + styling_layers
)

ggsave(pobs1_pobs_2_proportion_pscale_1, filename=paste0(dir,'/pobs1_pobs_2_proportion_pscale_1.png'))


pobs1_pobs_2_proportion_pscale_1.5 <- (ggplot(final_RDS[final_RDS$pscale==1.5,]) 
                                     + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
                                     + geom_tile()
                                     + facet_wrap(~dprob)
                                     + ggtitle(paste0('Method ', method, ': Observation probabilities and proportion \noutside projection interval for pscale 1.5'))
                                     + theme(plot.title = element_textbox_simple())
                                     + labs(
                                       y='Observation probability reinfections',
                                       x='Observation probability primary infections',
                                       fill='Proportion'
                                     )
                                     + styling_layers
)
ggsave(pobs1_pobs_2_proportion_pscale_1.5, filename=paste0(dir,'/pobs1_pobs_2_proportion_pscale_1.5.png'))

pobs1_pobs_2_proportion_pscale_2.5 <- (ggplot(final_RDS[final_RDS$pscale==2.5,]) 
                                       + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
                                       + geom_tile()
                                       + facet_wrap(~dprob)
                                       + ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs proportion of points outside prediction interval for pscale 2.5'))
                                       + theme(plot.title = element_textbox_simple())
                                       + labs(
                                         y='Observation probability reinfections',
                                         x='Observation probability primary infections',
                                         fill='Proportion'
                                       )
                                       + styling_layers

)
ggsave(pobs1_pobs_2_proportion_pscale_2.5, filename=paste0(dir,'/pobs1_pobs_2_proportion_pscale_2.5.png'))


