library(ggplot2)
library(ggtext)
method <- 2

dir <- paste0('sbv/method_', method,'_analysis/plots')

# get data
final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/output/all_data.RDS'))
summarised <- readRDS(paste0('sbv/method_',method,'_analysis/output/summarised_results.RDS'))
excluded_results <- readRDS(paste0('sbv/method_',method,'_analysis/output/all_data_excluded.RDS'))
summarised_all <- readRDS(paste0('sbv/method_',method,'_analysis/output/summarised_results_all.RDS'))
result <- readRDS('sbv/method_2_analysis/output/specificity_matrix.RDS')
final_RDS_con <-  readRDS(paste0('sbv/method_',method,'_analysis/output/summarised_results_med_con.RDS'))


dir.create(dir)


styling_layers <- 
  list(
    theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank()) 
    , theme_minimal() 
  )


#S2 convergence plto
colors <- c("Kappa" = "green", "Lambda" = "blue")
con_plot <- (ggplot(final_RDS, aes(x = pobs_2))
                   + geom_point(aes(y = kappa_con, color="Kappa"))
                   + geom_point(aes(y = lambda_con, color="Lambda"))
                   + geom_hline(yintercept=c(1.2), linetype="dotted", color="grey")
                    + geom_hline(yintercept=c(1.1), linetype="dotted", color="red")
                   + labs(
                     x = bquote(P[2])
                     , y = "Convergence diagnostic"
                     , color = ""
                   )
                   + styling_layers
                   + scale_color_manual(values = colors)
                   + ylim(1, 1.5)
)
                    

ggsave(con_plot, filename=paste0(dir, '/con_plot.png'))


#Pscale vs proportion plot after wavesplit
proportion_m2 <- (ggplot(summarised) 
                           + aes(x=pscale, y=proportion_above, group=pobs_2, color= factor(pobs_2)) 
                           + geom_line()
                           + labs(color=bquote(P[2])
                                  , y='Proportion'
                                  , x=bquote(sigma))
                           + ylim(0,1)
                           + styling_layers
                           + theme(legend.title = element_text( size=8)
                                   , legend.text=element_text(size=8, )
                                   , legend.spacing.y = unit(0.1, 'cm')
                                   , legend.key.height = unit(0.3, "cm")
                                   , axis.text = element_text(size=9)
                                    , axis.title=element_text(size=9))
)
ggsave(proportion_m2, filename=paste0(dir, '/pscale_propotion_aw.png'))


#Pscale vs proportion plot after wavesplit
cluster_m2 <- (ggplot(summarised) 
                              + aes(x=pscale, y=date_first_above, group=pobs_2, color= factor(pobs_2)) 
                              + geom_line()
                              + labs(color=bquote(P[2]), y='First day', x=bquote(sigma))
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


