library(ggplot2)
library(ggtext)
library(data.table)
library(gtools)
method <- 3

load('utils/cleanup_methods.RData')

dir <- paste0('sbv/method_', method,'_analysis/plots')

final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/output/all_data.RDS'))

dir.create(dir)


styling_layers <- 
  list(
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
  )
################### CONVERGENCE PLOT ###################


summarised <- readRDS(paste0('sbv/method_',method,'_analysis/output/summarised_results.RDS'))
excluded_results <- readRDS(paste0('sbv/method_',method,'_analysis/output/all_data_excluded.RDS'))
summarised_all <- readRDS(paste0('sbv/method_',method,'_analysis/output/summarised_results_all.RDS'))
result <- readRDS('sbv/method_3_analysis/output/specificity_matrix.RDS')


con_plot_s3 <- (ggplot(summarised_all)
                    + aes(x=pobs_1, y=pobs_2, fill=convergence)
                    + geom_tile()
                    + labs(fill="Proportion runs \nconverged"
                          , y=bquote(P[2])
                          , x=bquote(P[1])
                    )
                    + styling_layers

)

ggsave(con_plot_s3, filename=paste0(dir, '/con_plot_s3.png'))

#Proportion outside for each pobs1 and pobs2
pobs1_pobs_2_proportion <- (ggplot(summarised) 
                    + aes(x=pobs_1, y=pobs_2, fill=proportion_outside) 
                    + geom_tile()
                    + facet_wrap(~pscale)
                    + labs(fill='Proportion'
                           , y=bquote(P[2])
                           , x=bquote(P[1])
                           )
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
                    + prop_style  
                    + ggtitle(bquote(sigma))
                    + scale_x_continuous(breaks = c(0.1, 0.3, 0.5))
                    + scale_y_continuous(breaks = c(0.1, 0.3, 0.5))
)
ggsave(pobs1_pobs_2_proportion, filename=paste0(dir, '/pobs1_pobs_2_proportion.png'))



specificity_m3 <- (ggplot(result)
                   + aes(x=pobs_1, y=pobs_2, fill=specificity)
                   + geom_tile()
                   + labs(fill="Specificity"
                          , y=bquote(P[2])
                          , x=bquote(P[1])
                   )
                   +specifity_style
                   + geom_text(
                     aes(label = count_kappa_lambda_lt_1.1),
                     position = position_dodge(width = 0.2),  
                     vjust = -0.5)  
                   + scale_x_continuous(breaks=seq(0.1, 0.5, 0.1))
                   + scale_y_continuous(breaks=seq(0.1, 0.5, 0.1))
)

ggsave(specificity_m3, filename=paste0('sbv/method_3_analysis/plots/specificty_m3.png'))




#Proportion outside for each pobs1 and pobs2
pobs1_pobs_2_first_day <- (ggplot(summarised) 
                            + aes(x=pobs_1, y=pobs_2, fill=date_first_above) 
                            + geom_tile()
                            + facet_wrap(~pscale)
                            #+ ggtitle(paste0('Method ', method, ': pobs 1 and pobs 2 vs proportion \nof points outside prediction interval'))
                            + labs(fill='First day'
                                   , y=bquote(P[2])
                                   , x=bquote(P[1])
                            )
                            + ggtitle(bquote(sigma))
                            + theme(plot.title = element_textbox_simple())
                            + styling_layers
                            + cluster_style
                          )
ggsave(pobs1_pobs_2_first_day, filename=paste0(dir, '/pobs1_pobs_2_first_day.png'))


#date_first outside for each pobs1 and pobs2 for pscale=1, 1.5, 2, 2.5
cluster_m3 <- (ggplot(summarised[summarised$pscale %in% c(1.2, 1.5, 2.5),]) 
                                   + aes(x=pobs_1, y=pobs_2, fill=date_first_above) 
                                   + geom_tile()
                                   + facet_wrap(~pscale)
                                   + labs(fill='First day'
                                          , y=bquote(P[2])
                                          , x=bquote(P[1])
                                   )
                                  + styling_layers
                                  + cluster_style
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
                                  + ggtitle(expression(sigma))
)

ggsave(cluster_m3, filename=paste0(dir, '/pobs1_pobs_2_date_first_select.png'))


proportion_m3 <- (ggplot(summarised[summarised$pscale %in% c(1, 1.2, 1.5, 2.5),]) 
                      + aes(x = pobs_1, y = pobs_2, fill = proportion_outside) 
                      + geom_tile() 
                      + facet_wrap(~pscale, ncol=4) 
                      + labs(
                        fill = 'Proportion'
                        , y=bquote(P[2])
                        , x=bquote(P[1])
                      ) 
                      +  styling_layers
                      +  theme(
                        plot.title = element_text(hjust = 0.5
                                                  , size = 9
                                                  , margin = margin(0,0,0,0))
                        , axis.title = element_text(size=9)
                        , legend.title = element_text( size=9)
                        , legend.text = element_text( size=7)
                        , legend.spacing.y = unit(0.3, 'cm')
                        , axis.text = element_text(size=9)
                      ) 
                      + prop_style  
                      + ggtitle(expression(sigma))
)

ggsave(proportion_m3, filename=paste0(dir, '/pobs1_pobs_2_proportion_select.png'))


