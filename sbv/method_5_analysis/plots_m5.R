library(ggplot2)
library(ggtext)
library(scales)
method <- 5

dir <- paste0('sbv/method_', method,'_analysis/plots')

final_RDS <- readRDS(paste0('sbv/method_',method,'_analysis/combined_results.RDS'))

dir.create(dir)

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

con_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(1,1.2))
  )

styling_layers <- 
  list(
    #scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
    theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank()) 
    , theme_minimal() 
    , scale_colour_brewer(palette = "Set2") 
    , scale_fill_gradientn(colours =  rev(colorspace::terrain_hcl(100, c=200)))
  )

cluster_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0,80))
  )

prop_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0, 1))
  )


adjusted_RDS <- final_RDS[final_RDS$pscale %in% c(1, 1.5, 2, 2.5),]
adjusted_RDS <- adjusted_RDS[adjusted_RDS$pobs_1_min==0.05,]
adjusted_RDS <- adjusted_RDS[adjusted_RDS$pobs_1_max==0.2,]


proportion_m5 <- (ggplot(adjusted_RDS)
                  + aes(x=xm/10^4, y=steep/10^(-5), fill=proportion_after_wavesplit)
                  + facet_grid(pscale ~ multiplier)
                  + geom_tile()
                  + labs(fill="Proportion"
                         , y=expression(paste('Steepness x', 10^-5),')')
                         , x=expression(paste('Midpoint x', 10^4),')')
                  )
                  + styling_layers
                  + ggtitle('Multiplier')
                  + theme(
                    plot.title = element_text(hjust = 0.5
                                              , size = 9
                                              , margin = margin(0,0,0,0))
                    , axis.title= element_text(size=9)
                    , legend.title = element_text( size=9)
                    , legend.text = element_text( size=7)
                    , legend.spacing.y = unit(0.3, 'cm')
                  )
                  + scale_y_continuous(sec.axis = sec_axis(~ ., name = "Scale"
                                                             , breaks=NULL))
                  + scale_x_continuous( breaks=c(1, 3, 5))
                  + prop_style
)

ggsave(proportion_m5, filename=paste0(dir,'/proportion_s5.png'), device="png")


cluster_m5 <- (ggplot(adjusted_RDS)
                  + aes(x=xm/10^4, y=steep/10^(-5), fill=date_first_after_wavesplit)
                  + facet_grid(pscale ~ multiplier)
                  + geom_tile()
                  + labs(fill="First day"
                         , y=expression(paste('Steepness x', 10^-5),')')
                         , x=expression(paste('Midpoint x', 10^4),')')
                  )
                  + styling_layers
                  + ggtitle('Multiplier')
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
                  + scale_y_continuous(sec.axis = sec_axis(~ ., name = "Scale"
                                                           , breaks=NULL))
                  + scale_x_continuous(breaks=c(1, 3, 5))
                  
                  
)

ggsave(cluster_m5, filename=paste0(dir,'/date_first_s5.png'), device="png")


#adjusted_RDS <- final_RDS[final_RDS$pscale %in% c(1, 1.5, 2, 2.5),]
#adjusted_RDS <- final_RDS[final_RDS$pobs_1_min==0.05,]
#adjusted_RDS <- final_RDS[final_RDS$multiplier==2,]



kappa_RDS <- final_RDS[, c("kappa_con", "pobs_1_min", "pobs_1_max", "xm", "multiplier", "steep") ]
names(kappa_RDS)[1] <- "convergence"
lambda_RDS <- final_RDS[, c("lambda_con", "pobs_1_min", "pobs_1_max", "xm", "multiplier", "steep") ]
names(lambda_RDS)[1] <- "convergence"


kappa_RDS$convergence_title <- 'Kappa'
lambda_RDS$convergence_title <- 'Lambda'

adjusted_data <- rbind(kappa_RDS, lambda_RDS)
adjusted_data <- adjusted_data[adjusted_data$pobs_1_min==0.05,]
adjusted_data <- adjusted_data[adjusted_data$pobs_1_max==0.2,]

con_plot_s5 <- (ggplot(adjusted_data)
                  + aes(x=xm/10^4, y=steep/10^(-5), fill=convergence)
                  + facet_grid(convergence_title ~ multiplier)
                  + geom_tile()
                  + labs(fill="Convergence"
                         , y=expression(paste('Steepness x', 10^-5),')')
                         , x=expression(paste('Midpoint x', 10^4),')')
                  )
                  + styling_layers
                  + ggtitle('Multiplier')
                  + theme(
                    plot.title = element_text(hjust = 0.5, size = 11)
                  )
                  + scale_y_continuous(sec.axis = sec_axis(~ ., name = "Min observation probability \nfor primary infections"
                                                           , breaks=NULL))
                  + scale_x_continuous(breaks=c(1, 3, 5))
                  + con_style
)

ggsave(con_plot_s5, filename=paste0(dir,'/con_plot_s5.png'), device="png")

