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


kappa_RDS <- final_RDS[, c("kappa_con", "pobs_1", "pobs_2", "dprob") ]
names(kappa_RDS)[1] <- "convergence"
lambda_RDS <- final_RDS[, c("lambda_con", "pobs_1", "pobs_2", "dprob") ]
names(lambda_RDS)[1] <- "convergence"


kappa_RDS$convergence_title <- 'Kappa'
lambda_RDS$convergence_title <- 'Lambda'

adjusted_data <- rbind(kappa_RDS, lambda_RDS)

con_plot_s4 <- (ggplot(adjusted_data)
                + aes(x=pobs_1, y=pobs_2, fill=convergence)
                + facet_grid(convergence_title ~ dprob)
                + geom_tile()
                + labs(fill="Convergence"
                       , y='Observation probability\nReinfections'
                       , x='Observation probability\nPrimary infections'
                       #, title="Death probability"
                )
                + styling_layers
                + ggtitle('Death probability')
                + theme(
                  plot.title = element_text(hjust = 0.5, size = 11)
                )
                + con_style
)




ggsave(con_plot_s4, filename=paste0(dir,'/con_plot_s4.png'), device="png")



proportion_m4 <- (ggplot(final_RDS[final_RDS$pscale %in% c(1,1.5,2,2.5),])
                + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit)
                + facet_grid(dprob ~ pscale)
                + geom_tile()
                + labs(fill="Proportion"
                       , y='Observation probability\nReinfections'
                       , x='Observation probability\nPrimary infections'
                )
                + styling_layers
                + ggtitle('Scale')
                + theme(
                   plot.title = element_text(hjust = 0.5
                                             , size = 9
                                             , margin = margin(0,0,0,0))
                  , axis.title = element_text(size=9)
                  , legend.title = element_text( size=9)
                  , legend.text = element_text( size=7)
                  , legend.spacing.y = unit(0.3, 'cm')
                  , axis.text = element_text(size=9)
                )
                + scale_y_continuous(breaks=seq(0.1, 0.5, 0.2)
                                     , sec.axis = sec_axis(~ ., name = "Death probability"
                                                           , breaks=NULL))
                + prop_style
                
)

ggsave(proportion_m4, filename=paste0(dir,'/proportion_s4.png'), device="png")



cluster_m4 <- (ggplot(final_RDS[final_RDS$pscale %in% c(1,1.5,2,2.5),])
                  + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit)
                  + facet_grid(dprob ~ pscale)
                  + geom_tile()
                  + labs(fill="First day"
                         , y='Reinfections\nObservation probability'
                         , x='Primary infections\nObservation probability'
                  )
                  + styling_layers
                  + ggtitle('Scale')
                  + theme(
                    plot.title = element_text(hjust = 0.5
                                           , size = 9
                                           , margin = margin(0,0,0,0))
                    , axis.title = element_text(size=9)
                    , legend.title = element_text( size=9)
                    , legend.text = element_text( size=7)
                    , legend.spacing.y = unit(0.3, 'cm')
                    , axis.text = element_text(size=7)
                    )
                  + scale_y_continuous(breaks=seq(0.1, 0.5, 0.2)
                                       , sec.axis = sec_axis(~ ., name = "Death probability"
                                                             , breaks=NULL))
                  + cluster_style
)

ggsave(cluster_m4, filename=paste0(dir,'/date_first_s4.png'), device="png")

