library(ggplot2)
library(ggtext)
library(patchwork)

styling_layers <- 
  list(
    #scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
    theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank()) 
    , theme_minimal() 
    #, scale_colour_brewer(palette = "Set2") 
    , scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
  )


summarised_results_all <- readRDS('sbv/third_infections/output/summarised_results_all.RDS')

convergence_l2 <- (ggplot(summarised_results_all)
                          + aes(x=pscale)
                          + styling_layers
                   
                          + geom_line(aes(y=lambda_con, color="lambda_con"))
                          + geom_line(aes(y = kappa_con, color="kappa_con"))
                          + geom_line(aes(y = lambda_2_con, color="lambda_2_con"))
                          + geom_line(aes(y = convergence, color="convergence"))
                          + scale_color_manual(values = c( lambda_con = "blue", kappa_con = "red",  lambda_2_con= "green", convergence = "pink")
                                               , labels = c ( 
                                                      kappa_con = parse(text = expression(kappa[2]))
                                                    , lambda_con = parse(text = expression(lambda[2]))
                                                    , lambda_2_con = parse(text = expression(lambda[2]^"'"))
                                                    , convergence = 'All'
                                                 )
                                               , breaks = c('kappa_con', 'lambda_con', 'lambda_2_con', 'convergence' )
                                               )
                          + labs(x=expression(sigma[1]), y='Proportion runs that converged', color='')
                          + theme(plot.title = element_textbox_simple()
                                  , axis.title= element_text(size=9)
                                  , axis.text = element_text(size=9))
                          + ylim(c(0,1))
)

ggsave(convergence_l2, file ='sbv/third_infections/plots/convergence_sbv.png', width=5, height=5)

#plot for median of cluster above projection interval
summarised_results <- readRDS('sbv/third_infections/output/summarised_results.RDS')

summarised_results_inc <- readRDS('sbv/third_infections/output/summarised_results_inc.RDS')

summarised_results <- summarised_results %>% mutate(
  pscale2 = 1, 
)
colnames(summarised_results)[1] ="pscale1"

combined_results <- rbind(summarised_results, summarised_results_inc)
#excluded_results_inc <- readRDS('sbv/third_infections/output/all_data_excluded_inc.RDS')

cluster_third <- (ggplot(combined_results[combined_results$pscale1==2.8,]) 
               + aes(x=pscale2, y=date_first_above) 
               + geom_point()
               + geom_segment(aes(x=pscale2, xend=pscale2, y=0, yend= date_first_above), linetype=2)
               + labs(x=bquote(sigma[2]), y=paste0('Timing of first cluster above'))
               + styling_layers
               + theme(plot.title = element_textbox_simple()
                       , axis.title= element_text(size=9)
                       , axis.text = element_text(size=9))
              # + xlim(1, 2)
               + scale_x_continuous(breaks=c( 1, 1.2, 1.5, 2), limits=1:2)
)

ggsave(cluster_third,file = 'sbv/third_infections/plots/cluster_third_inc.png')


proportion_third <- (ggplot(combined_results[combined_results$pscale1==2.8,]) 
                  + aes(x=pscale2, y=proportion_above) 
                  + geom_point()
                  + geom_segment(aes(x=pscale2, xend=pscale2, y=0, yend= proportion_above), linetype=2)
                  + labs(x=bquote(sigma[2]), y=paste0('Proportion above'))
                  + styling_layers
                  + theme(plot.title = element_textbox_simple()
                          , axis.title= element_text(size=9)
                          , axis.text = element_text(size=9))
                  # + xlim(1, 2)
                  + scale_x_continuous(breaks=c( 1, 1.2, 1.5, 2), limits=1:2)
)

ggsave(proportion_third, file = 'sbv/third_infections/plots/proportion_third_inc.png')

combined_inc <- cluster_third / proportion_third + plot_annotation(tag_levels='A')
ggsave(combined_inc, file ='sbv/third_infections/plots/combined_inc.tiff', dpi=300)

