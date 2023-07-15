library(ggplot2)

# Assuming you have a ts frame called 'ts' with 'date' and 'ma_third' columns

# Convert 'date' column to a Date object
ts$date <- as.Date(ts$date)

# Create the ggplot with the d--esired aesthetics
ggplot(ts, aes(x = date, y = ma_reinf)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(x = "Date", y = "Third") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(fit_through), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.Date(wave_split), linetype = "dashed", color = "red") +
  geom_line(aes(y=observed, color='pink', width=0.1))

final_RDS_l2 <- readRDS(paste0('sbv/third_infections/l2.RDS'))
final_RDS <- readRDS(paste0('sbv/third_infections/withoutl2.RDS'))

styling_layers <- 
  list(
    #scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
    theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank()) 
    , theme_minimal() 
    , scale_colour_brewer(palette = "Set2") 
    , scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
  )

proportion <- (ggplot(final_RDS) 
                  + aes(x=pscale, y=proportion) 
                  + geom_line()
                  + labs(x="Scale", y='Proportion')
                  + ylim(0,1)
                  + theme(plot.title = element_textbox_simple()
                          , axis.title=element_text(size=9)
                          , axis.text = element_text(size=9))
                  
)
proportion


cluster <- (ggplot(final_RDS) 
               + aes(x=pscale, y=date_first) 
               + geom_line()
               + labs(x="Reinfections observation probability", y='First day')
               + styling_layers
               + theme(plot.title = element_textbox_simple()
                       , axis.title= element_text(size=9)
                       , axis.text = element_text(size=9))
               
)
cluster


converence_kappa_l2 <- (ggplot(final_RDS_l2) 
            + aes(x=pscale, y=kappa_con) 
            + geom_line()
            + labs(x="Scale", y='Kappa convergence')
            + styling_layers
            + theme(plot.title = element_textbox_simple()
                    , axis.title= element_text(size=9)
                    , axis.text = element_text(size=9))
            + geom_hline(yintercept = 1.1, color="red", linetype="dashed")
            
)

convergence_lambda_l2 <- (ggplot(final_RDS_l2) 
                          + aes(x=pscale, y=lambda_con) 
                          + geom_line()
                          + labs(x="Scale", y='Lambda convergence')
                          + styling_layers
                          + theme(plot.title = element_textbox_simple()
                                  , axis.title= element_text(size=9)
                                  , axis.text = element_text(size=9))
                          + geom_hline(yintercept = 1.1, color="red", linetype="dashed")
)

convergence_lambda2_l2 <- (ggplot(final_RDS_l2) 
                          + aes(x=pscale, y=lambda_2_con) 
                          + geom_line()
                          + labs(x="Scale", y='Lambda2 Convergence')
                          + styling_layers
                          + theme(plot.title = element_textbox_simple()
                                  , axis.title= element_text(size=9)
                                  , axis.text = element_text(size=9))
                          + geom_hline(yintercept = 1.1, color="red", linetype="dashed")
                          
)

