library(gridExtra)

#run plot scripts in environment

for (i in 1:5) {
  get_median_values(i)
  source(paste0('sbv/method_',i,'_analysis/plots_m',i,'.R'))
}

#proportion_m3 <- proportion_m3 
proportion_m4 <- proportion_m4 + guides(fill = "none")
proportion_m5 <- proportion_m5 + guides(fill = "none")

method_3_final <- grid.arrange(nrow=1
                               , arrangeGrob(proportion_m3, top = grid::textGrob("C", x = 0, hjust = 0))
                               )

bottom_left <- grid.arrange(nrow=2
                         , method_3_final
                         , arrangeGrob(proportion_m4, top = grid::textGrob("D", x = 0, hjust = 0))
                         , heights = c(0.8, 1)
)

bottom <- grid.arrange(
                      ncol = 2
                      , bottom_left
                      , arrangeGrob(proportion_m5, top = grid::textGrob("E", x = 0, hjust = 0))
                        , widths = c(1, 1)
)

ggsave(bottom, file='sbv/plots/m3_4_5.png', width = 796/72, height=570/72, type='cairo')


top <- grid.arrange(nrow=1
                  , arrangeGrob(proportion_m1, top = grid::textGrob("A", x = 0, hjust = 0))
                  , arrangeGrob(proportion_m2, top = grid::textGrob("B", x = 0, hjust = 0))
)


proportion_all <- grid.arrange(nrow=2
             , top
             , bottom
             , heights = c(0.4, 1))

ggsave(proportion_all, file='sbv/plots/proportion_all.png', width = 796/72, height=570/72, type='cairo')



top <- grid.arrange(nrow=1
                    , arrangeGrob(proportion_m1, top = grid::textGrob("A", x = 0, hjust = 0))
                    , arrangeGrob(proportion_m2, top = grid::textGrob("B", x = 0, hjust = 0))
)

ggsave(top, file='sbv/plots/m1_2.png', width = 796/72, height=570/72, type='cairo')

