#convergence for psacale = 1 vs metrics

get_all_data <- function(method){
  files <- list.files(path=paste0("sbv/method_",method,"_analysis/output/final_output_data"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  
  
  final_RDS <- data.frame()
  
  for (file in files) {
    final_RDS <- rbind.data.frame(final_RDS, readRDS(file))
  }
  return (final_RDS)
  
}

library(ggplot2)
library(ggtext)
library(dplyr)


dir <- paste0('sbv/plots')

specifity_style <- 
  list ( 
    scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100, c=200)), limits=c(0, 1))
  )


# METHOD 1

method <- 1

RDS <- get_all_data(1)
RDS <- RDS[RDS$pscale==1,]

result <- RDS %>%
  group_by(pscale) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con < 1.1 & lambda_con < 1.1),
            count_date_not_na = sum(!is.na(date_first_after_wavesplit) & kappa_con < 1.1 & lambda_con < 1.1)) %>%
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, file=paste0(dir, '/data_for_plots/method_1_specificity.RDS'))


# METHOD 2

method <- 2

RDS <- get_all_data(method)
RDS <- RDS[RDS$pscale==1,]

result <- RDS %>%
  group_by(pscale, pobs_2) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con < 1.1 & lambda_con < 1.1),
            count_date_not_na = sum(!is.na(date_first_after_wavesplit) & kappa_con < 1.1 & lambda_con < 1.1)) %>%
  # Calculate the result of [ 1 - (count_date_not_na / count_kappa_lambda_lt_1.1) ]
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, file=paste0(dir, '/data_for_plots/method_', method, '_specificity.RDS'))


# METHOD 3

method <- 3

RDS <- get_all_data(method)
RDS <- RDS[RDS$pscale==1,]

result <- RDS %>%
  group_by(pscale, pobs_2, pobs_1) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con < 1.1 & lambda_con < 1.1),
            count_date_not_na = sum(!is.na(date_first_after_wavesplit) & kappa_con < 1.1 & lambda_con < 1.1)) %>%
  # Calculate the result of [ 1 - (count_date_not_na / count_kappa_lambda_lt_1.1) ]
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, file=paste0(dir, '/data_for_plots/method_', method, '_specificity.RDS'))


# plot

specificity_m3 <- (ggplot(result)
                + aes(x=pobs_1, y=pobs_2, fill=specificity)
                + geom_tile()
                + labs(fill="Specificity"
                       , y='Observation probability\nReinfections'
                       , x='Observation probability\nPrimary infections'
                )
                +specifity_style
)

ggsave(specificity_m3, filename=paste0(dir, '/subplots/specificty_m3.png'))

## METHOD 4

method <- 4

RDS <- get_all_data(method)
RDS <- RDS[RDS$pscale==1,]

result <- RDS %>%
  group_by(pscale, pobs_2, pobs_1, dprob) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con < 1.1 & lambda_con < 1.1 ),
            count_date_not_na = sum(!is.na(date_first_after_wavesplit) & kappa_con < 1.1 & lambda_con < 1.1)) %>%
  # Calculate the result of [ 1 - (count_date_not_na / count_kappa_lambda_lt_1.1) ]
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, file=paste0(dir, '/data_for_plots/method_', method, '_specificity.RDS'))

# plot

specificity_m4 <- (ggplot(result)
                   + aes(x=pobs_1, y=pobs_2, fill=specificity)
                   + facet_wrap(~dprob)
                   + geom_tile()
                   + labs(fill="Specificity"
                          , y='Observation probability\nReinfections'
                          , x='Observation probability\nPrimary infections'
                   )
                   +specifity_style
)

ggsave(specificity_m4, filename=paste0(dir, '/subplots/specificty_m4.png'))


## METHOD 5

method <- 5

RDS <- get_all_data(method)
RDS <- RDS[RDS$pscale==1,]

result <- RDS %>%
  group_by(pscale, pobs_1_max, pobs_1_min, steep, xm, multiplier) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con < 1.1 & lambda_con < 1.1 ),
            count_date_not_na = sum(!is.na(date_first_after_wavesplit) & kappa_con < 1.1 & lambda_con < 1.1)) %>%
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

pobs_1_min_ <- 0.1

result <- result[result$pobs_1_min==pobs_1_min_,]

#code for discrete case
specificty_m5 <- (ggplot(result)
                + aes(x = xm / 10^4, y = steep / 10^(-5), fill = specificity)
                + facet_grid(pobs_1_max ~ multiplier)
                + geom_tile()
                + labs(fill = "Pobs 1 maximum"
                       , y = expression(paste('Steepness x', 10^-5), ')')
                       , x = expression(paste('Midpoint x', 10^4), ')')
                )
                + ggtitle('Multiplier')
                + theme(
                  plot.title = element_text(hjust = 0.5, size = 11)
                )
                + scale_y_continuous(sec.axis = sec_axis(~ ., name = "Max observation probability \nfor primary infections"
                                                         , breaks = NULL))
                + scale_x_continuous(breaks = c(1, 3, 5))
                + specifity_style
)

ggsave(specificty_m5, filename=paste0(dir, '/subplots/specificty_m5.png'))


saveRDS(result, file=paste0(dir, '/data_for_plots/method_', method, '_specificity_',pobs_1_min_,'.RDS'))
