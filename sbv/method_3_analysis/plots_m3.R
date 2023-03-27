library(ggplot2)
library(ggtext)
method <- 3

dir <- paste0('sbv/method_', method,'_analysis/plots')

final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/combined_results.RDS'))

dir.create(dir)

#Lambda convergence plot 
lambda_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_2, y = pobs_1, fill=lambda_con)
                    + geom_tile()
                    + labs(fill="Lambda Convergence"
                           , y='Observation probability\nPrimary infections'
                           , x='Observation probability\nReinfections'
                    )
                    + scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=1.15)
                    + ggtitle(paste0('Lambda Convergence Sensitivity Analysis: Method ', method))
                    + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                            , panel.grid.minor = element_blank()) 
                    + theme_minimal() 
                    +  scale_colour_brewer(palette = "Set2") 
                    #+  theme(axis.title.x=element_blank())  
                    #+  theme(legend.position = "none")
)
ggsave(lambda_con_plot, filename=paste0(dir, '/lambda_convergence.png'))

#Kappa convergence plot
kappa_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_2, y = pobs_1, fill=kappa_con)
                    + geom_tile()
                    + labs(fill="Kappa Convergence"
                           , y='Observation probability\nPrimary infections'
                           , x='Observation probability\nReinfections'
                    )
                    + scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=1.08)
                    + ggtitle(paste0('Kappa Convergence Sensitivity Analysis: Method ', method))
                    + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                            , panel.grid.minor = element_blank()) 
                    + theme_minimal() 
                    +  scale_colour_brewer(palette = "Set2") 
                    #+  theme(axis.title.x=element_blank())  
                    #+  theme(legend.position = "none")
)
ggsave(kappa_con_plot, filename=paste0(dir, '/kappa_convergence.png'))

#Proportion outside for each pobs1 and pobs2
pobs1_pobs_2_proportion <- (ggplot(final_RDS) 
  + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
  + geom_tile()
  + facet_wrap(~pscale)
  + ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs proportion \nof points outside prediction interval'))
  + labs(fill='Proportion'
         , y='Reinfections observation probability'
         , x='Primary infections observation probability'
         )
  + theme(plot.title = element_textbox_simple())
  + scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
  + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
          , panel.grid.minor = element_blank()) 
  + theme_minimal() 
  +  scale_colour_brewer(palette = "Set2") 
  #+  theme(axis.title.x=element_blank())  
  #+  theme(legend.position = "none")
)
ggsave(pobs1_pobs_2_proportion, filename=paste0(dir, '/pobs1_pobs_2_proportion.png'))


#First cluster outside for each pobs1 and pobs2
pobs1_pobs2_first_cluster <- (ggplot(final_RDS) 
  + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit) 
  + geom_tile()
  + facet_wrap(~pscale)
  + ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs first cluster of points outside prediction interval AFTER wavesplit'))
  + theme(plot.title = element_textbox_simple())
  + labs(fill='First day'
         , y='Reinfections observation probability'
         , x='Primary infections observation probability'
  )
  + scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=5)
  + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
          , panel.grid.minor = element_blank()) 
  + theme_minimal() 
  +  scale_colour_brewer(palette = "Set2") 
  #+  theme(axis.title.x=element_blank())  
  #+  theme(legend.position = "none")
)
ggsave(pobs1_pobs2_first_cluster, filename=paste0(dir, '/pobs1_pobs2_first_cluster.png'))



pobs1_pscale_proportion <- (ggplot(final_RDS) 
                            + aes(x=pobs_1, y=pscale, fill=proportion_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pobs_2)
                            + ggtitle(paste0('Method ', method, ': pobs 1 and pscale vs proportion of points outside prediction interval'))
                            + theme(plot.title = element_textbox_simple())
                            + labs(fill='Proportion'
                                   , y='Scale'
                                   , x='Primary infections observation probability'
                            )
                            + scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
                            + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                    , panel.grid.minor = element_blank()) 
                            + theme_minimal() 
                            +  scale_colour_brewer(palette = "Set2") 
                            #+  theme(axis.title.x=element_blank())  
                            #+  theme(legend.position = "none")
)
ggsave(pobs1_pscale_proportion, filename=paste0(dir, '/pobs1_pscale_proportion.png'))


#Pobs1 vs pscale first cluster
pobs1_pscale_first_cluster <- (ggplot(final_RDS) 
                            + aes(x=pobs_1, y=pscale, fill=date_first_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pobs_2)
                            + ggtitle(paste0('Method ', method, ': pobs 1 and pscale vs  first cluster of points outside prediction interval AFTER wavesplit'))
                            + theme(plot.title = element_textbox_simple())
                            + labs(fill='First day'
                                   , y='Scale'
                                   , x='Primary infections observation probability'
                            )
                            + scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=10)
                            + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                    , panel.grid.minor = element_blank()) 
                            + theme_minimal() 
                            +  scale_colour_brewer(palette = "Set2") 
                            #+  theme(axis.title.x=element_blank())  
                            #+  theme(legend.position = "none")
)
ggsave(pobs1_pscale_first_cluster, filename=paste0(dir, '/pobs1_pscale_first_cluster.png'))


#Rinfections obs and pscale with proportion
pobs2_pscale_proportion <- (ggplot(final_RDS) 
                            + aes(x=pobs_2, y=pscale, fill=proportion_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pobs_1)
                            + ggtitle(paste0('Method ', method, ': pobs 2 and pscale vs proportion of points outside prediction interval'))
                            + theme(plot.title = element_textbox_simple())
                            + labs(fill='First day'
                                   , y='Scale'
                                   , x='Reinfections observation probability'
                            )
                            + scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
                            + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                    , panel.grid.minor = element_blank()) 
                            + theme_minimal() 
                            +  scale_colour_brewer(palette = "Set2") 
                            #+  theme(axis.title.x=element_blank())  
                            #+  theme(legend.position = "none")
)
ggsave(pobs2_pscale_proportion, filename=paste0(dir, '/pobs2_pscale_proportion.png'))



pobs2_pscale_first_cluster <- (ggplot(final_RDS) 
                               + aes(x=pobs_2, y=pscale, fill=date_first_after_wavesplit) 
                               + geom_tile()
                               + facet_wrap(~pobs_1)
                               + ggtitle(paste0('Method ', method, ': pobs 2 and pscale vs  first cluster of points outside prediction interval AFTER wavesplit'))
                               + theme(plot.title = element_textbox_simple())
                               + labs(fill='First day'
                                      , y='Scale'
                                      , x='First day'
                               )
                               + scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=10)
                               + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                       , panel.grid.minor = element_blank()) 
                               + theme_minimal() 
                               +  scale_colour_brewer(palette = "Set2") 
                               #+  theme(axis.title.x=element_blank())  
                               #+  theme(legend.position = "none")
)
ggsave(pobs2_pscale_first_cluster, filename=paste0(dir, '/pobs2_pscale_first_cluster.png'))


