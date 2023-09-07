library(ggplot2)
library(ggtext)
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


#actual
data <- readRDS('data/ts_data_for_analysis.RDS')
# Create the ggplot with the d--esired aesthetics
ggplot(data, aes(x = date, y = ma_third)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(x = "Date", y = "Third") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(fit_through), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.Date(wave_split), linetype = "dashed", color = "red") +
  geom_line(aes(y=third, color='pink', width=0.1))



final_RDS_l2 <- readRDS(paste0('sbv/third_infections/l2_combined_results.RDS'))

styling_layers <- 
  list(
    #scale_fill_gradient2(low='green', mid="white", high='yellow', midpoint=0.5)
    theme(panel.border = element_rect(colour = "black", size = 0.25)
          , panel.grid.minor = element_blank()) 
    , theme_minimal() 
    , scale_colour_brewer(palette = "Set2") 
    , scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
  )


final_RDS <- getAllResults('third', 'sbv/third_infections/output/ml2third/')
load('utils/cleanup_methods.RData')
summarised_results <- final_RDS %>% 
                      group_by(pscale)  %>%
                      summarise(
                        convergence = proportion_convergence_three(lambda_con, kappa_con, lambda_2_con)
                        ,  kappa_con = proportion_converence(kappa_con)
                        , lambda_2_con = proportion_converence(lambda_2_con)
                        , lambda_con = proportion_converence(lambda_con)
                        , proportion = median(proportion)
                        , date_first = median_cluster(date_first)
                        , .groups = 'drop')
                
  

convergence_l2 <- (ggplot(summarised_results)
                          + aes(x=pscale)
                          + geom_line(aes(y=lambda_con, color="Lambda"))
                          + geom_line(aes(y = kappa_con, color="Kappa"))
                          + geom_line(aes(y = lambda_2_con, color="Lambda 2"))
                          + geom_line(aes(y = convergence, color="All"))
                          + labs(x="Scale", y='Proportion runs that converged', color='')
                          + scale_color_manual(values = c("Kappa" = "blue", "Lambda" = "red", "Lambda 2" = "green", "All"="Pink"))
                          + styling_layers
                          + theme(plot.title = element_textbox_simple()
                                  , axis.title= element_text(size=9)
                                  , axis.text = element_text(size=9))
)

ggsave('sbv/third_infections/plots/convergence_sbv.png')

#add column for converge
final_RDS <- final_RDS %>% mutate(converges = (kappa_con<=1.1 & lambda_con<=1.1 & lambda_2_con<=1.1) )

excluded_results <- excludeResultsNonConverging('third', final_RDS )

result <- excluded_results %>%
  group_by(pscale) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con <= 1.1 & lambda_con <= 1.1 & lambda_2_con<=1.1),
            count_date_not_na = sum(!is.na(date_first) & kappa_con <= 1.1 & lambda_con <= 1.1 & lambda_2_con<=1.1)) %>%
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, 'sbv/third_infections/output/specificity_matrix.RDS')
write.csv(result,'sbv/third_infections/output/specificity_matrix.csv' )
