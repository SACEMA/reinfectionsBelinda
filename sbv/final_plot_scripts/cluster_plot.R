library(gridExtra)

#run plot scripts in environment

for (i in 1:5) {
  source(paste0('sbv/method_',i,'_analysis/plots_m',i,'.R'))
}

cluster_m3 <- cluster_m3 +  guides(fill = "none")
cluster_m4 <- cluster_m4 + guides(fill = "none")
cluster_m5 <- cluster_1_5_0.4_0.5 

method_3_final <- grid.arrange(nrow=1
                               , arrangeGrob(cluster_m3, top = grid::textGrob("C", x = 0, hjust = 0))
)

bottom_left <- grid.arrange(nrow=2
                            , method_3_final
                            , arrangeGrob(cluster_m4, top = grid::textGrob("D", x = 0, hjust = 0))
                            , heights = c(0.6, 1)
)

bottom <- grid.arrange(
  ncol = 2
  , bottom_left
  , arrangeGrob(cluster_m5, top = grid::textGrob("E", x = 0, hjust = 0))
  , widths = c(0.8, 1)
)

ggsave(bottom, file='sbv/plots/m3_4_5.png', width = 796/72, height=570/72, type='cairo')


top <- grid.arrange(nrow=1
                    , arrangeGrob(cluster_m1, top = grid::textGrob("A", x = 0, hjust = 0))
                    , arrangeGrob(cluster_m2, top = grid::textGrob("B", x = 0, hjust = 0))
)


cluster_all <- grid.arrange(nrow=2
                               , top
                               , bottom
                               , heights = c(0.4, 1))

ggsave(cluster_all, file='sbv/plots/cluster_all.png', width = 796/72, height=570/72, type='cairo')
#ggsave(cluster_all, file='sbv/plots/cluster_all.tiff',  width = 796/72, height=570/72, dpi=1500, compression = "lzw")

#CLUSTER plot for S5
cluster_1_2_0.1_0.2 <- cluster_1_2_0.1_0.2 + guides(fill = "none")
top_1 <- grid.arrange(nrow=1
                      , arrangeGrob(cluster_1_2_0.1_0.2
                                    , top = grid::textGrob(expression(sigma=1.2), x = 0, hjust = 0))
                      , arrangeGrob(cluster_1_5_0.1_0.2
                                    , top = grid::textGrob(expression(sigma=1.5), x = 0, hjust = 0))

                      , widths=c(0.92, 1.18)
                      )
cluster_1_2_0.2_0.3 <- cluster_1_2_0.2_0.3 + guides(fill = "none")
cluster_1_5_0.2_0.3 <- cluster_1_5_0.2_0.3 + guides(fill = "none")

top_2 <- grid.arrange(nrow=1
                      , arrangeGrob(cluster_1_2_0.2_0.3
                                    , top = grid::textGrob(expression(sigma=1.2), x = 0, hjust = 0))
                      , arrangeGrob(cluster_1_5_0.2_0.3
                                    , top = grid::textGrob(expression(sigma=1.5), x = 0, hjust = 0))
                      
                      , widths=c(1,1)
)

cluster_1_2_0.3_0.4 <- cluster_1_2_0.3_0.4 + guides(fill = "none")
cluster_1_5_0.3_0.4 <- cluster_1_5_0.3_0.4 + guides(fill = "none")

top_3 <- grid.arrange(nrow=1
                      , arrangeGrob(cluster_1_2_0.3_0.4
                                    , top = grid::textGrob(expression(sigma=1.2), x = 0, hjust = 0))
                      , arrangeGrob(cluster_1_5_0.3_0.4
                                    , top = grid::textGrob(expression(sigma=1.5), x = 0, hjust = 0))
                      
                      , widths=c(1,1)
)

cluster_1.5 <- grid.arrange(nrow=3
                            , arrangeGrob(top_1, top = grid::textGrob("A", x = 0, hjust = 0))
                            , arrangeGrob(top_2, top = grid::textGrob("B", x = 0, hjust = 0))
                            , arrangeGrob(top_3, top = grid::textGrob("C", x = 0, hjust = 0)) 
                            , heights = c(1, 1, 1))

ggsave(cluster_1.5, file='sbv/plots/cluster_1.5.png', width = 570/72, height=796/72, type='cairo')
