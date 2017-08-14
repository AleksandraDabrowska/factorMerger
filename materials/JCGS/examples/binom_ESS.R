rm(list = ls())

library(dplyr)
library(factorMerger)

data("ess")
# Time for FM (this takes quite a bit)
happyFM <- mergeFactors(ess$happy, ess$country,
                        family = "binomial",
                        method = "fast-fixed")

# GIC as AIC
p1 <- plot(happyFM, panel = "GIC",
           title = "",
           panelGrid = FALSE)

# GIC as BIC
p2 <- plot(happyFM, panel = "GIC",
           penalty = log(NROW(ess$country)),
           title = "",
           panelGrid = FALSE)

p3 <- plot(happyFM, panel = "GIC",
           penalty = 500,
           title = "",
           panelGrid = FALSE)

gicComparisons <- ggpubr::ggarrange(p1, p2, p3, ncol = 3, labels = c("AIC", "BIC", "GIC"))
ggsave(gicComparisons, file = "./materials/JCGS/examples/ess_gic.pdf",
       width = 17, height = 10)

p4 <- plot(happyFM, panel = "tree", penalty = 500,
     title = "European Social Study - happiness proportion",
     panelGrid = FALSE, nodesSpacing = "effects")

ggsave(p4, file = "./materials/JCGS/examples/ess_tree.pdf",
       width = 11, height = 7)


p5 <- plot(happyFM, panel = "response", penalty = 500,
           title = "European Social Study - happiness proportion",
           panelGrid = FALSE)

ggsave(p5, file = "./materials/JCGS/examples/ess_proportion.pdf",
       width = 11, height = 7)
