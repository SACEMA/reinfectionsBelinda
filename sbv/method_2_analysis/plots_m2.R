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
                   + ylim(1, 1.4)

)
                    

ggsave(con_plot, filename=paste0(dir, '/con_plot.png'))



pscale_proportion_plot <- (ggplot(final_RDS) 
            + aes(x=pscale, y=proportion, group=pobs_2, color= factor(pobs_2)) 
            + geom_line()
            + labs(color="Pobs 2", y='Proportion')
            + ggtitle(paste0('Method ', method, ' pscale vs\n proportion of points outside prediction interval'))
            + ylim(0,1)
            + styling_layers
)
ggsave(pscale_proportion_plot, filename=paste0(dir, '/pscale_propotion.png'))







#Pscale vs proportion plot after wavesplit
pscale_proportion_plot_aw <- (ggplot(final_RDS) 
                           + aes(x=pscale, y=proportion_after_wavesplit, group=pobs_2, color= factor(pobs_2)) 
                           + geom_line()
                           + labs(color="Reinfections\nobservation\nprobability", y='Proportion', x='Scale')
                           #+ ggtitle(paste0('Method ', method, ' pscale vs proportion of points outside\nprediction interval AFTER wavesplit'))
                           + ylim(0,1)
                           + styling_layers
)
ggsave(pscale_proportion_plot_aw, filename=paste0(dir, '/pscale_propotion_aw.png'))


cluster_proportion_plot <- (ggplot(final_RDS) 
                           + aes(x=pscale, y=date_first, group=pobs_2, color= factor(pobs_2)) 
                           + geom_line()
                           + labs(color="Pobs 2", y='Proportion')
                           + ggtitle(paste0('Method ', method, ' pscale vs first cluster of points outside prediction interval'))
                           + ylim(50,100)
                           + theme(plot.title = element_textbox_simple())
                           + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                   , panel.grid.minor = element_blank()) 
                           + theme_minimal() 
                           +  scale_colour_brewer(palette = "Set2") 
                           +  theme(axis.title.x=element_blank())  
                           +  theme(legend.position = "none")
)
ggsave(cluster_proportion_plot, filename=paste0(dir, '/cluster_proportion.png'))


cluster_proportion_plot_aw <- (ggplot(final_RDS) 
                              + aes(x=pscale, y=date_first_after_wavesplit, group=pobs_2, color= factor(pobs_2)) 
                              + geom_line()
                              + labs(color="Reinfections\nobservation\nprobability", y='First day after split')
                              + ggtitle(paste0('Method ', method, ' pscale vs first cluster of points\noutside prediction interval AFTER wavesplit'))
                              + ylim(5,10)
                              + theme(plot.title = element_textbox_simple())
                              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                      , panel.grid.minor = element_blank()) 
                              + theme_minimal() 
                              +  scale_colour_brewer(palette = "Set2") 
                              #+  theme(axis.title.x=element_blank())  
                             # +  theme(legend.position = "none")
)
ggsave(cluster_proportion_plot_aw, filename=paste0(dir, '/cluster_proportion_aw.png'))


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
