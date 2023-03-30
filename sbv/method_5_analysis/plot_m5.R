library(ggplot2)
library(ggtext)
method <- 5

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
  )

adjusted_RDS <- final_RDS[final_RDS$pscale %in% c(1, 1.5, 2, 2.5),]
adjusted_RDS <- adjusted_RDS[adjusted_RDS$pobs_1_min==0.05,]
adjusted_RDS <- adjusted_RDS[adjusted_RDS$pobs_1_max==0.2,]


proportion_s5 <- (ggplot(adjusted_RDS)
                  + aes(x=xm, y=steep, fill=proportion_after_wavesplit)
                  + facet_grid(pscale ~ multiplier)
                  + geom_tile()
                  + labs(fill="Proportion"
                         , y='Steepness'
                         , x='Mid-point'
                  )
                  + styling_layers
                  + ggtitle('Scale')
                  + theme(
                    plot.title = element_text(hjust = 0.5, size = 11)
                  )
                  + scale_y_continuous(sec.axis = sec_axis(~ ., name = "Death probability"
                                                             , breaks=NULL))
                  + scale_fill_continuous(drop=TRUE)
)

ggsave(proportion_s5, filename=paste0(dir,'/proportion_s5.png'), device="png")



