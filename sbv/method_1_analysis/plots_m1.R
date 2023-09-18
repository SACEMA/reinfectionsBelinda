library(ggplot2)
library(ggtext)
load('utils/cleanup_methods.RData')
library(gtools)
library(data.table)
library(dplyr)
method <- 1

final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/combined_results.RDS'))

dir <- paste0('sbv/method_',method,'_analysis/plots')

dir.create(dir)

final_RDS <- readRDS('sbv/method_1_analysis/output/all_data.RDS')

summarised <- readRDS('sbv/method_1_analysis/output/summarised_results.RDS')


styling_layers <- 
  list(
     theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank()) 
    , theme_minimal() 
    , scale_colour_brewer(palette = "Set2") 
    , scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
  )


proportion_m1 <- (ggplot(summarised) 
                           + aes(x=pscale, y=proportion_after_wavesplit) 
                           + geom_line()
                           + labs(x=bquote(sigma), y='Proportion')
                           + ylim(0,1)
                           + styling_layers      
                           + theme(plot.title = element_textbox_simple()
                                   , axis.title=element_text(size=9)
                                   , axis.text = element_text(size=9))
                          
)
proportion_m1

ggsave(proportion_m1, filename=paste0(dir,'/pscale_proportion_aw.png'), device="png")



cluster_m1 <- (ggplot(summarised) 
                              + aes(x=pscale, y=date_first_after_wavesplit) 
                              + geom_line()
                              + labs(x=bquote(sigma), y='First day')
                               + styling_layers
                              + theme(plot.title = element_textbox_simple()
                                      , axis.title= element_text(size=9)
                                      , axis.text = element_text(size=9))
                              
)
cluster_m1

ggsave(cluster_m1, filename=paste0(dir,'/pscale_firstcluster_aw.png'), device="png")

