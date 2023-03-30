library(ggplot2)
library(ggtext)
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
    , scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
    , scale_x_continuous(breaks=seq(0.1, 0.5, 0.2))
    , scale_y_continuous(breaks=seq(0.1, 0.5, 0.2))
  )


#Lambda convergence plot 
lambda_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_2, y = pobs_1, fill=lambda_con)
                    + geom_tile()
                    + labs(fill="Lambda Convergence"
                           , y='Observation probability\nPrimary infections'
                           , x='Observation probability\nReinfections'
                    )
                    + ggtitle(paste0('Lambda Convergence Sensitivity Analysis: Method ', method))
                    + styling_layers
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
                    + ggtitle(paste0('Kappa Convergence Sensitivity Analysis: Method ', method))
                    + styling_layers
)
ggsave(kappa_con_plot, filename=paste0(dir, '/kappa_convergence.png'))

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
pobs1_pobs_2_proportion_select <- (ggplot(final_RDS[final_RDS$pscale %in% c(1, 1.5, 2, 2.5),]) 
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
ggsave(pobs1_pobs_2_proportion_select, filename=paste0(dir, '/pobs1_pobs_2_proportion_select.png'))


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
  + styling_layers
)
ggsave(pobs1_pobs2_first_cluster, filename=paste0(dir, '/pobs1_pobs2_first_cluster.png'))
