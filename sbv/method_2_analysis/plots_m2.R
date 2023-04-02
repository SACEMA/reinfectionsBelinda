library(ggplot2)
library(ggtext)
method <- 2

dir <- paste0('sbv/method_', method,'_analysis/plots')

final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/combined_results.RDS'))

dir.create(dir)


styling_layers <- 
  list(
    #scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
    theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank()) 
    , theme_minimal() 
   # , scale_colour_brewer(palette = "Set2") 
  )


#plots useless as different pscale don't affect convergence
lambda_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_2, y = lambda_con)
                    + geom_line()
                    + ggtitle(paste0('Lambda Convergence Sensitivity Analysis: Method ', + method)) 
                    + ylab('Convergence diagnostic')
                    + xlab('Reinfections observation probability')
                    + styling_layers
)

ggsave(lambda_con_plot, filename=paste0(dir, '/lambda_con_plot.png'))


#plots useless as different pscale don't affect convergence
kappa_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_2, y = kappa_con)
                    + geom_line()
                    + ggtitle(paste0('Kappa Convergence Sensitivity Analysis: Method ', + method)) 
                    + ylab('Convergence diagnostic')
                    + xlab('Reinfections observation probability')
                    + styling_layers
)
ggsave(kappa_con_plot, filename=paste0(dir, '/kappa_con_plot.png'))


#S2 convergence plto
colors <- c("Kappa" = "green", "Lambda" = "blue")
con_plot <- (ggplot(final_RDS, aes(x = pobs_2))
                   + geom_line(aes(y = kappa_con, color="Kappa"))
                   + geom_line(aes(y = lambda_con, color="Lambda"))
                   + geom_hline(yintercept=c(1.2), linetype="dotted")

                 #  + ggtitle(paste0('Kappa Convergence Sensitivity Analysis: Method ', + method)) 
                   + labs(
                     x = "Reinfections observation probability"
                     , y = "Convergence diagnostic"
                     , color = ""
                   )
                   + styling_layers
                   + scale_color_manual(values = colors)
                   + ylim(1, 1.5)
)
                    

ggsave(con_plot, filename=paste0(dir, '/con_plot.png'))


#Pscale vs proportion plot after wavesplit
proportion_m2 <- (ggplot(final_RDS) 
                           + aes(x=pscale, y=proportion_after_wavesplit, group=pobs_2, color= factor(pobs_2)) 
                           + geom_line()
                           + labs(color="Reinfections\nobservation\nprobability"
                                  , y='Proportion'
                                  , x='Scale')
                           + ylim(0,1)
                           + styling_layers
                          # + guides(fill = guide_legend(byrow = TRUE))
                           + theme(legend.title = element_text( size=8)
                                   , legend.text=element_text(size=8, )
                                   , legend.spacing.y = unit(0.1, 'cm')
                                   , legend.key.height = unit(0.3, "cm")
                                   , axis.text = element_text(size=9)
                                    , axis.title=element_text(size=9))
                    ## important additional element
)
ggsave(proportion_m2, filename=paste0(dir, '/pscale_propotion_aw.png'))


#Pscale vs proportion plot after wavesplit
cluster_m2 <- (ggplot(final_RDS) 
                              + aes(x=pscale, y=date_first_after_wavesplit, group=pobs_2, color= factor(pobs_2)) 
                              + geom_line()
                              + labs(color="Reinfections\nobservation\nprobability", y='First day', x='Scale')
                              #+ ggtitle(paste0('Method ', method, ' pscale vs proportion of points outside\nprediction interval AFTER wavesplit'))
                              + ylim(0,70)
                              + styling_layers
                              +  theme(legend.title = element_text( size=8)
                                , legend.text=element_text(size=8, )
                                , legend.spacing.y = unit(0.1, 'cm')
                                , legend.key.height = unit(0.3, "cm")
                                , axis.text = element_text(size=9)
                                , axis.title=element_text(size=9))
                              
)
ggsave(cluster_m2, filename=paste0(dir, '/pscale_cluster_m2.png'))


