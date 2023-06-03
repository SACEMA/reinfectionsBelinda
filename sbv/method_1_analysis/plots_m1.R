library(ggplot2)
library(ggtext)
method <- 1

final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/combined_results.RDS'))

dir <- paste0('sbv/method_',method,'_analysis/plots')

dir.create(dir)


styling_layers <- 
  list(
    #scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
     theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank()) 
    , theme_minimal() 
    , scale_colour_brewer(palette = "Set2") 
    , scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
  )


proportion_m1 <- (ggplot(final_RDS) 
                           + aes(x=pscale, y=proportion_after_wavesplit) 
                           + geom_line()
                           + labs(x="Scale", y='Proportion')
                           + ylim(0,1)
                           + styling_layers      
                           + theme(plot.title = element_textbox_simple()
                                   , axis.title=element_text(size=9)
                                   , axis.text = element_text(size=9))
                          
)
proportion_m1

ggsave(pscale_proportion_plot_aw, filename=paste0(dir,'/pscale_proportion_aw.png'), device="png")



cluster_m1 <- (ggplot(final_RDS) 
                              + aes(x=pscale, y=date_first_after_wavesplit) 
                              + geom_line()
                              + labs(x="Reinfections observation probability", y='First day')
                               + styling_layers
                              + theme(plot.title = element_textbox_simple()
                                      , axis.title= element_text(size=9)
                                      , axis.text = element_text(size=9))
                              
)
cluster_m1

ggsave(cluster_m1, filename=paste0(dir,'/pscale_firstcluster_aw.png'), device="png")

