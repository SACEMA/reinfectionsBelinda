library(ggplot2)
load('method_4_analysis/results_400.RDS')


lambda_con_plot <- (ggplot(final_RDS)
           + aes(x = pobs_1, y = pobs_2, fill = lambda_con)
           + geom_tile()
           + ggtitle('Lambda Convergence Sensitivity Analysis')
           + scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
)
lambda_con_plot

kappa_con_plot <- (ggplot(final_RDS)
                    + aes(x = pobs_1, y = pobs_2, fill = kappa_con)
                    + geom_tile()
                    + ggtitle('Kappa Convergence Sensitivity Analysis')
                   + scale_fill_gradientn(colours = rev(colorspace::terrain_hcl(100)))
)
kappa_con_plot
ggsave(lambda_con_plot, filename='method_3_analysis/plots/lambda_density.png', device="png")
ggsave(kappa_con_plot, filename='method_3_analysis/plots/kappa_density.png', device="png")