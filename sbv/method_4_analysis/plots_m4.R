library(ggplot2)
library(ggtext)
library(gtools)
load('utils/cleanup_methods.RData')
method <- 4

dir <- paste0('sbv/method_', method,'_analysis/plots')

final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/output/all_data.RDS'))

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
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0,80))
  )

con_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)))
  )

prop_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0, 1))
  )

specifity_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0, 1))
  , scale_x_continuous(breaks=seq(0.1, 0.5, 0.2))
  , scale_y_continuous(breaks=seq(0.1, 0.5, 0.2))
  )


summarised <- readRDS(paste0('sbv/method_',method,'_analysis/output/summarised_results.RDS'))
excluded_results <- readRDS(paste0('sbv/method_',method,'_analysis/output/all_data_excluded.RDS'))
summarised_all <- readRDS(paste0('sbv/method_',method,'_analysis/output/summarised_results_all.RDS'))
result <- readRDS('sbv/method_4_analysis/output/specificity_matrix.RDS')


con_plot_s4 <- (ggplot(summarised_all)
                + aes(x=pobs_1, y=pobs_2, fill=convergence)
                + facet_grid(~dprob)
                + geom_tile()
                + labs(fill="Proportion runs \nconverged"
                       , y=bquote(P[2])
                       , x=bquote(P[1])
                       #, title="Death probability"
                )
                + styling_layers
                + ggtitle(bquote(d[1]))
                + theme(
                  plot.title = element_text(hjust = 0.5, size = 11)
                )
                + con_style
)

ggsave(con_plot_s4, filename=paste0(dir,'/con_plot_s4.png'), device="png", width=6, height=5)
ggsave(con_plot_s4, file=paste0(dir,'/con_plot_s4.tiff'), dpi=1200, compression = "lzw")



############# SPECIFICITY PLOT ###############

  

# plot
result <- result[!is.na(result$pscale),]
specificity_m4 <- (ggplot(result)
                   + aes(x=pobs_1, y=pobs_2, fill=specificity)
                   + facet_wrap(~dprob)
                   + geom_tile()
                   + labs(fill="Specificity"
                          , y=bquote(P[2])
                          , x=bquote(P[1])
                   )
                   + ggtitle(bquote(d[1]))
                   + theme(
                     , plot.title = element_text(hjust = 0.5, size = 11)
                   )
                   +specifity_style
                   + geom_text(
                     aes(label = count_kappa_lambda_lt_1.1),
                     position = position_dodge(width = 0.2), size = 3, 
                     vjust = -0.5)  
)


ggsave(specificity_m4, filename=paste0('sbv/method_4_analysis/plots/specificity_m4.png'))
ggsave(specificity_m4, file='sbv/method_4_analysis/plots/specificity_m4.tiff', dpi=1200, compression = "lzw")


############ Cluster 


cluster_m4 <- (ggplot(excluded_results[excluded_results$pscale %in% c(1.2,1.5,2.5),])
               + aes(x=pobs_1, y=pobs_2, fill=date_first_after_wavesplit)
               + facet_grid(dprob ~ pscale)
               + geom_tile()
               + labs(fill="First day"
                      , y=bquote(P[2])
                      , x=bquote(P[1])
               )
               + styling_layers
               + ggtitle(bquote(sigma))
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
                                    , sec.axis = sec_axis(~ ., name = bquote(d[1])
                                                          , breaks=NULL))
               + cluster_style
)

ggsave(cluster_m4, filename=paste0(dir,'/date_first_s4.png'), device="png")



proportion_m4 <- (ggplot(excluded_results[excluded_results$pscale %in% c(1, 1.2,1.5,2.5),])
                + aes(x=pobs_1, y=pobs_2, fill=proportion_after_wavesplit)
                + facet_grid(dprob ~ pscale)
                + geom_tile()
                + labs(fill="Proportion"
                       , y=bquote(P[2])
                       , x=bquote(P[1])
                )
                + styling_layers
                + ggtitle(bquote(sigma))
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
                                     , sec.axis = sec_axis(~ ., name = bquote(d[1])
                                                           , breaks=NULL))
                + prop_style
)

ggsave(proportion_m4, filename=paste0(dir,'/proportion_s4.png'), device="png")

