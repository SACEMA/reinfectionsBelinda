library(ggplot2)
library(ggtext)
library(data.table)

method <- 3

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
    , scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)))
    , scale_x_continuous(breaks=seq(0.1, 0.5, 0.2))
    , scale_y_continuous(breaks=seq(0.1, 0.5, 0.2))
  )

cluster_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0,50))
    )

con_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(1,1.5))
  )

prop_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0, 1))
  )

kappa_RDS <- final_RDS[, c("kappa_con", "pobs_1", "pobs_2") ]
names(kappa_RDS)[1] <- "convergence"
lambda_RDS <- final_RDS[, c("lambda_con", "pobs_1", "pobs_2") ]
names(lambda_RDS)[1] <- "convergence"


kappa_RDS$convergence_title <- 'Kappa'
lambda_RDS$convergence_title <- 'Lambda'

adjusted_data <- rbind(kappa_RDS, lambda_RDS)

con_plot_s3 <- (ggplot(adjusted_data)
                    + aes(x=pobs_1, y=pobs_2, fill=convergence)
                    + facet_wrap(~convergence_title)
                    + geom_tile()
                    + labs(fill="Convergence"
                          , y='Observation probability\nReinfections'
                          , x='Observation probability\nPrimary infections'
                    )
                    + styling_layers
                    + con_style
)

ggsave(con_plot_s3, filename=paste0(dir, '/con_plot_s3.png'))

#Proportion outside for each pobs1 and pobs2
pobs1_pobs_2_proportion <- (ggplot(final_RDS) 
  + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
  + geom_tile()
  + facet_wrap(~pscale)
  #+ ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs proportion \nof points outside prediction interval'))
  + labs(fill='Proportion'
         , y='Reinfections observation probability'
         , x='Primary infections observation probability'
         )
  + theme(plot.title = element_textbox_simple())
  + styling_layers
)
ggsave(pobs1_pobs_2_proportion, filename=paste0(dir, '/pobs1_pobs_2_proportion.png'))

#Proportion outside for each pobs1 and pobs2 for pscale=1, 1.5, 2, 2.5
proportion_m3 <- (ggplot(final_RDS[final_RDS$pscale %in% c(1, 1.5, 2, 2.5),]) 
                            + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pscale)
                            #+ ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs proportion \nof points outside prediction interval'))
                            + labs(fill='Proportion'
                                   , y='Observation probability\nReinfections'
                                   , x='Observation probability\nPrimary infections'
                            )
                            
                            + styling_layers
                            + prop_style
                            + theme(plot.title = element_textbox_simple()
                             , axis.title=element_text(size=9)
                             , legend.title = element_text( size=9)
                             , legend.text = element_text( size=7)
                             , legend.spacing.y = unit(0.3, 'cm')
                             , axis.text = element_text(size=9)
                            )
)
ggsave(proportion_m3, filename=paste0(dir, '/pobs1_pobs_2_proportion_select.png'))


#Proportion outside for each pobs1 and pobs2
pobs1_pobs_2_first_day <- (ggplot(final_RDS) 
                            + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit) 
                            + geom_tile()
                            + facet_wrap(~pscale)
                            #+ ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs proportion \nof points outside prediction interval'))
                            + labs(fill='First day'
                                   , y='Reinfections observation probability'
                                   , x='Primary infections observation probability'
                            )
                            + theme(plot.title = element_textbox_simple())
                            + styling_layers
                            + cluster_style
                          )
ggsave(pobs1_pobs_2_first_day, filename=paste0(dir, '/pobs1_pobs_2_first_day.png'))

#date_first outside for each pobs1 and pobs2 for pscale=1, 1.5, 2, 2.5
cluster_m3 <- (ggplot(final_RDS[final_RDS$pscale %in% c(1, 1.5, 2, 2.5),]) 
                                   + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit) 
                                   + geom_tile()
                                   + facet_wrap(~pscale)
                                   #+ ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs date_first \nof points outside prediction interval'))
                                   + labs(fill='First day'
                                          , y='Observation probability\nReinfections'
                                          , x='Observation probability\nPrimary infections'
                                   )
                                  + styling_layers
                                  + cluster_style
                                  + theme(plot.title = element_textbox_simple()
                                    , axis.title=element_text(size=9)
                                    , legend.title = element_text( size=9)
                                    , legend.text = element_text( size=7)
                                    , legend.spacing.y = unit(0.3, 'cm')
                                    , axis.text = element_text(size=7)
                                  )

)
ggsave(cluster_m3, filename=paste0(dir, '/pobs1_pobs_2_date_first_select.png'))
