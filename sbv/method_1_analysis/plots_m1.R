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

pscale_proportion_plot <- (ggplot(final_RDS) 
            + aes(x=pscale, y=proportion) 
            + geom_line()
            + labs(x="Reinfections observation probability", y='Proportion')
            + ggtitle(paste0('Method ', method, ' pscale vs proportion of points outside prediction interval'))
            + ylim(0,1)
            + theme(plot.title = element_textbox_simple())
            + styling_layers
)
pscale_proportion_plot

ggsave(pscale_proportion_plot, filename=paste0(dir,'/pscale_proportion.png'), device="png")


pscale_proportion_plot_aw <- (ggplot(final_RDS) 
                           + aes(x=pscale, y=proportion_after_wavesplit) 
                           + geom_line()
                           + labs(x="Reinfections observation probability", y='Proportion')
                           #+ ggtitle(paste0('Method ', method, ' pscale vs proportion of points outside prediction interval AFTER wavesplit'))
                           + ylim(0,1)
                           + theme(plot.title = element_textbox_simple())
                           + styling_layers
)
pscale_proportion_plot_aw

ggsave(pscale_proportion_plot_aw, filename=paste0(dir,'/pscale_proportion_aw.png'), device="png")


firstcluster_proportion_plot <- (ggplot(final_RDS) 
                           + aes(x=pscale, y=date_first_after_wavesplit) 
                           + geom_line()
                           + labs(x="Reinfections observation probability", y='First day')
                           + theme(plot.title = element_textbox_simple())
                           + styling_layers
)
firstcluster_proportion_plot

ggsave(firstcluster_proportion_plot, filename=paste0(dir,'/cluster_proportion.png'), device="png")

