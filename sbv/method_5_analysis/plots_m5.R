library(ggplot2)
library(ggtext)
library(scales)
library(dplyr)
library(patchwork)
library(gridExtra)
library(grid)

method <- 5

dir <- paste0('sbv/method_', method,'_analysis/plots')

dir.create(dir)

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

#DEFINE THE STYLES
con_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0, 1))
  )

styling_layers <- 
  list(
    #scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
    theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank(), panel.background = element_rect(fill = "black")) 
    , theme_minimal() 
    , scale_colour_brewer(palette = "Set2") 
    , scale_fill_gradientn(colours =  rev(colorspace::terrain_hcl(100, c=200)), limits=c(0,1))
  )

cluster_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0,80))
  )

prop_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0, 1))
  )

specifity_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0, 1))
  )


final_RDS <- readRDS('sbv/method_5_analysis/output/all_data.RDS')
excluded_results <- readRDS('sbv/method_5_analysis/output/all_data_excluded.RDS')


summarised <- readRDS('sbv/method_5_analysis/output/summarised_results.RDS')
summarised_all <- readRDS('sbv/method_5_analysis/output/summarised_results_all.RDS')


############################# CONVERGENCE ####################################

pobs_2_values <- expand.grid(pobs_2_min=c(0.1, 0.2, 0.3, 0.4, 0.5), pobs_2_max=c(0.2, 0.3, 0.4, 0.5))
pobs_2_values <- pobs_2_values[pobs_2_values$pobs_2_max > pobs_2_values$pobs_2_min,]


for (i in 1:nrow(pobs_2_values)){
  tryCatch({
    plot_name <- paste0('convergence_', pobs_2_values$pobs_2_min[i],'_',pobs_2_values$pobs_2_max[i])
    con_plot_s5 <- (ggplot(summarised_all[summarised_all$pobs_2_min==pobs_2_values$pobs_2_min[i] 
                                      &  summarised_all$pobs_2_max==pobs_2_values$pobs_2_max[i], ])
                + aes(x=xm/10^4, y=steep/10^(-5), fill=convergence)
                + facet_grid(factor(pobs_1_min, levels=c(0.4, 0.3, 0.2, 0.1)) ~ pobs_1_max)
                + geom_tile()
                + con_style
                + labs(fill="Proportion runs\nconvergence"
                       , y=expression(paste('Steepness x', 10^-5),')')
                       , x=expression(paste('Midpoint x', 10^4),')')
                )
                + ggtitle(bquote(P[1]^max))
                + theme(
                  legend.position = "none"
                   , plot.title = element_text(hjust = 0.5, size = 11)
                )
                + scale_y_continuous(breaks=c(5,10), sec.axis = sec_axis(~ ., name = bquote(P[1]^min)
                                                         , breaks=NULL))
                + scale_x_continuous(breaks=c(3, 5))

                
    )
    assign(plot_name, con_plot_s5)
    ggsave(con_plot_s5, file=paste0(dir
                                    , '/convergence_'
                                    , pobs_2_values$pobs_2_min[i]
                                    , '_'
                                    , pobs_2_values$pobs_2_max[i]
                                    , '.png'), device = 'png' 
                                    , width=6
                                    , height=4.8
           )
    }, error =  function(d) {print(d)})
}

#combine convergence plots in one
convergence_0.1_0.4 <- convergence_0.1_0.4 + theme(legend.position = "right")            
convergence_0.2_0.5 <- convergence_0.2_0.5 + theme(legend.position = "right")      
convergence_0.3_0.4 <- convergence_0.3_0.4 + theme(legend.position = "right")   
convergence_0.4_0.5 <- convergence_0.4_0.5  + theme(legend.position = "right")

combined_top <- grid.arrange(
      arrangeGrob(convergence_0.1_0.2, top = grid::textGrob("0.2", x = 0, hjust = 0))
      , arrangeGrob(convergence_0.1_0.5, top = grid::textGrob("0.5", x = 0, hjust = 0))
      , nrow=1
      , widths=c(1,1.5)
)


combined_bottom <- grid.arrange(
  arrangeGrob(convergence_0.1_0.3, top = grid::textGrob("0.3", x = 0, hjust = 0))
  , arrangeGrob(convergence_0.1_0.4, top = grid::textGrob("0.4", x = 0, hjust = 0))
  , nrow=1
  , widths=c(1,1.2)
)

convergence_pobs_min_0.1 <- grid.arrange(combined_top, combined_bottom, nrow=2)

ggsave(convergence_pobs_min_0.1, file='sbv/method_5_analysis/plots/convergence_pobs_min_0.1.png', width = 796/72, height=570/72, type='cairo')

convergence_pobs_min_0.2_top <- grid.arrange(
  grid.arrange(
    arrangeGrob(convergence_0.2_0.3, top = grid::textGrob("0.3", x = 0, hjust = 0))
    , arrangeGrob(convergence_0.2_0.4, top = grid::textGrob("0.4", x = 0, hjust = 0))
    , ncol=2
    )
)

convergence_pobs_min_0.2_bottom <- grid.arrange(
   arrangeGrob(convergence_0.2_0.5, top = grid::textGrob("0.5", x = 0, hjust = 0))
  )

convergence_pobs_min_0.2 <- grid.arrange(convergence_pobs_min_0.2_top, convergence_pobs_min_0.2_bottom)

ggsave(convergence_pobs_min_0.2, file='sbv/method_5_analysis/plots/convergence_pobs_min_0.2.png', width = 796/72, height=570/72, type='cairo')


convergence_pobs_min_0.3 <- grid.arrange(
  arrangeGrob(convergence_0.3_0.4, top = grid::textGrob("0.4", x = 0, hjust = 0))
  , arrangeGrob(convergence_0.3_0.5, top = grid::textGrob("0.5", x = 0, hjust = 0))
  , widths = c(1, 0.9)
)

ggsave(convergence_pobs_min_0.3, file='sbv/method_5_analysis/plots/convergence_pobs_min_0.3.png', width = 796/72, height=570/72, type='cairo')

ggsave(convergence_0.4_0.5, file='sbv/method_5_analysis/plots/convergence_pobs_min_0.4.png', width = 796/72, height=570/72, type='cairo')

ggsave(convergence_0.4_0.5, file=paste0(dir, '/m5_con.tiff'), dpi=1500, compression = "lzw")

############################# SPECIFICITY ####################################


result <- readRDS('sbv/method_5_analysis/output/specificity_matrix.RDS')

pobs_2_values <- expand.grid(pobs_2_min=c(0.1, 0.2, 0.3, 0.4, 0.5), pobs_2_max=c(0.2, 0.3, 0.4, 0.5))
pobs_2_values <- pobs_2_values[pobs_2_values$pobs_2_max > pobs_2_values$pobs_2_min,]

for (i in 1:nrow(pobs_2_values)){
  tryCatch({
    plot_name <- paste0('specificity_', pobs_2_values$pobs_2_min[i],'_',pobs_2_values$pobs_2_max[i])
    specificity <- (ggplot(result[result$pobs_2_min==pobs_2_values$pobs_2_min[i] 
                                  &  result$pobs_2_max==pobs_2_values$pobs_2_max[i], ])
                    + aes(x=xm/10^4, y=steep/10^(-5), fill=specificity)
                    + facet_grid(factor(pobs_1_min, levels=c(0.4, 0.3, 0.2, 0.1)) ~ pobs_1_max)
                    + geom_tile()
                    + labs(fill="Specificity"
                           , y=expression(paste('Steepness x', 10^-5),')')
                           , x=expression(paste('Midpoint x', 10^4),')')
                    )
                    + ggtitle(bquote(P[1]^max))
                    + specifity_style
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
                    + scale_y_continuous(breaks=c(5,10), sec.axis = sec_axis(~ ., name = bquote(P[1]^min)
                                                                             , breaks=NULL))
                    + scale_x_continuous(breaks=c(3, 4, 5))
                    + geom_text(
                      aes(label = count_kappa_lambda_lt_1.1),
                      position = position_dodge(width = 0.9),  # Adjust position if needed
                      vjust = -0.5  # Adjust vertical position of labels
                    )
    )
    assign(plot_name, specificity)
    ggsave(specificity, file=paste0(dir
                                    , '/5_specificity_'
                                    , pobs_2_values$pobs_2_min[i]
                                    , '_'
                                    , pobs_2_values$pobs_2_max[i]
                                    , '.png'), device = 'png')
  }, error = function(d){print(d)})
}

ggsave(specificity_0.4_0.5, file=paste0(dir, '/m5_spec.tiff'), dpi=1500, compression = "lzw")


#########################
#################### CLUSTER #############################
summarised <- readRDS('sbv/method_5_analysis/output/summarised_results.RDS')
summarised_plots <- summarised[summarised$pscale > 1, ]



pobs_2_values <- expand.grid(pobs_2_min=c(0.1, 0.2, 0.3, 0.4, 0.5), pobs_2_max=c(0.2, 0.3, 0.4, 0.5))
pobs_2_values <- pobs_2_values[pobs_2_values$pobs_2_max > pobs_2_values$pobs_2_min,]

for (i in 1:nrow(pobs_2_values)){
  tryCatch({
    plot_name <- paste0('cluster_1_5_', pobs_2_values$pobs_2_min[i],'_',pobs_2_values$pobs_2_max[i])
    
    cluster <- (ggplot(summarised_plots[summarised_plots$pobs_2_min==pobs_2_values$pobs_2_min[i] 
                                        &  summarised_plots$pobs_2_max==pobs_2_values$pobs_2_max[i]
                                        & summarised_plots$pscale==1.5, ])
                + aes(x=xm/10^4, y=steep/10^(-5), fill=date_first_above)
                + facet_grid(factor(pobs_1_min, levels=c(0.4, 0.3, 0.2, 0.1)) ~ pobs_1_max)
                + geom_tile()
                + labs(fill="Timing of \nfirst cluster"
                       , y=expression(paste('Steepness x', 10^-5),')')
                       , x=expression(paste('Midpoint x', 10^4),')')
                )
                + ggtitle(bquote(P[1]^max))
                + cluster_style
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
                + scale_y_continuous(breaks=c(5,10), sec.axis = sec_axis(~ ., name = bquote(P[1]^min)
                                                                         , breaks=NULL))
                + scale_x_continuous(breaks=c(3, 4, 5))
    )
    assign(plot_name, cluster)
    ggsave(cluster, file=paste0(dir
                                , '/1_5_cluster_'
                                , pobs_2_values$pobs_2_min[i]
                                , '_'
                                , pobs_2_values$pobs_2_max[i]
                                , '.png'), device = 'png')
    plot_name <- paste0('cluster_1_2_', pobs_2_values$pobs_2_min[i],'_',pobs_2_values$pobs_2_max[i])
    
    cluster <- (ggplot(summarised_plots[summarised_plots$pobs_2_min==pobs_2_values$pobs_2_min[i] 
                                        &  summarised_plots$pobs_2_max==pobs_2_values$pobs_2_max[i]
                                        & summarised_plots$pscale==1.2, ])
                + aes(x=xm/10^4, y=steep/10^(-5), fill=date_first_above)
                + facet_grid(factor(pobs_1_min, levels=c(0.4, 0.3, 0.2, 0.1)) ~ pobs_1_max)
                + geom_tile()
                + labs(fill="Timing of \nfirst cluster"
                       , y=expression(paste('Steepness x', 10^-5),')')
                       , x=expression(paste('Midpoint x', 10^4),')')
                )
                + ggtitle(bquote(P[1]^max))
                + cluster_style
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
                + scale_y_continuous(breaks=c(5,10), sec.axis = sec_axis(~ ., name = bquote(P[1]^min)
                                                                         , breaks=NULL))
                + scale_x_continuous(breaks=c(3, 4, 5))
    )
    assign(plot_name, cluster)
    ggsave(cluster, file=paste0(dir
                                , '/1_2_cluster_'
                                , pobs_2_values$pobs_2_min[i]
                                , '_'
                                , pobs_2_values$pobs_2_max[i]
                                , '.png'), device = 'png')
  }, error = function(d){print(d)})
}


for (i in 1:nrow(pobs_2_values)){
  tryCatch({
    plot_name <- paste0('proportion_1_5_', pobs_2_values$pobs_2_min[i],'_',pobs_2_values$pobs_2_max[i])
    
    proportion <- (ggplot(summarised_plots[summarised_plots$pobs_2_min==pobs_2_values$pobs_2_min[i] 
                                        &  summarised_plots$pobs_2_max==pobs_2_values$pobs_2_max[i]
                                        & summarised_plots$pscale==1.5, ])
                + aes(x=xm/10^4, y=steep/10^(-5), fill=proportion_above)
                + facet_grid(factor(pobs_1_min, levels=c(0.4, 0.3, 0.2, 0.1)) ~ pobs_1_max)
                + geom_tile()
                + labs(fill="Proportion"
                       , y=expression(paste('Steepness x', 10^-5),')')
                       , x=expression(paste('Midpoint x', 10^4),')')
                )
                + ggtitle(bquote(P[1]^max))
                + prop_style
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
                + scale_y_continuous(breaks=c(5,10), sec.axis = sec_axis(~ ., name = bquote(P[1]^min)
                                                                         , breaks=NULL))
                + scale_x_continuous(breaks=c(3, 4, 5))
    )
    assign(plot_name , proportion)
    ggsave(proportion, file=paste0(dir
                                , '/1_5_proportion_'
                                , pobs_2_values$pobs_2_min[i]
                                , '_'
                                , pobs_2_values$pobs_2_max[i]
                                , '.png'), device = 'png')
  }, error = function(d){print(d)})
}

for (i in 1:nrow(pobs_2_values)){
  tryCatch({
    plot_name <- paste0('proportion_1_2_', pobs_2_values$pobs_2_min[i],'_',pobs_2_values$pobs_2_max[i])
    
    proportion <- (ggplot(summarised_plots[summarised_plots$pobs_2_min==pobs_2_values$pobs_2_min[i] 
                                           &  summarised_plots$pobs_2_max==pobs_2_values$pobs_2_max[i]
                                           & summarised_plots$pscale==1.2, ])
                   + aes(x=xm/10^4, y=steep/10^(-5), fill=proportion_above)
                   + facet_grid(factor(pobs_1_min, levels=c(0.4, 0.3, 0.2, 0.1)) ~ pobs_1_max)
                   + geom_tile()
                   + labs(fill="Proportion"
                          , y=expression(paste('Steepness x', 10^-5),')')
                          , x=expression(paste('Midpoint x', 10^4),')')
                   )
                   + ggtitle(bquote(P[1]^max))
                   + prop_style
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
                   + scale_y_continuous(breaks=c(5,10), sec.axis = sec_axis(~ ., name = bquote(P[1]^min)
                                                                            , breaks=NULL))
                   + scale_x_continuous(breaks=c(3, 4, 5))
    )
    assign(plot_name , proportion)
    
    ggsave(proportion, file=paste0(dir
                                   , '/1_2_proportion_'
                                   , pobs_2_values$pobs_2_min[i]
                                   , '_'
                                   , pobs_2_values$pobs_2_max[i]
                                   , '.png'), device = 'png')
  }, error = function(d){print(d)})
}

for (i in 1:nrow(pobs_2_values)){
  tryCatch({
    plot_name <- paste0('proportion_1_', pobs_2_values$pobs_2_min[i],'_',pobs_2_values$pobs_2_max[i])
    
    proportion <- (ggplot(summarised[summarised$pobs_2_min==pobs_2_values$pobs_2_min[i] 
                                           &  summarised$pobs_2_max==pobs_2_values$pobs_2_max[i]
                                           & summarised$pscale==1, ])
                   + aes(x=xm/10^4, y=steep/10^(-5), fill=proportion_above)
                   + facet_grid(factor(pobs_1_min, levels=c(0.4, 0.3, 0.2, 0.1)) ~ pobs_1_max)
                   + geom_tile()
                   + labs(fill="Proportion"
                          , y=expression(paste('Steepness x', 10^-5),')')
                          , x=expression(paste('Midpoint x', 10^4),')')
                   )
                   + ggtitle(bquote(P[1]^max))
                   + prop_style
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
                   + scale_y_continuous(breaks=c(5,10), sec.axis = sec_axis(~ ., name = bquote(P[1]^min)
                                                                            , breaks=NULL))
                   + scale_x_continuous(breaks=c(3, 4, 5))
    )
    assign(plot_name , proportion)
    
    ggsave(proportion, file=paste0(dir
                                   , '/1_proportion_'
                                   , pobs_2_values$pobs_2_min[i]
                                   , '_'
                                   , pobs_2_values$pobs_2_max[i]
                                   , '.png'), device = 'png')
  }, error = function(d){print(d)})
}


