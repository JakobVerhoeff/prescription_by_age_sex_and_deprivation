#### Figure 1 wrapper 

library(patchwork)

# Generate bar plot Females (g1a)
source("Figure 1A&B.R")

source("Figure 1C and 1D.R")

(g1a + g1b )/ (g1c + g1d + plot_layout(guides='collect') & theme(legend.position = 'bottom')) +
  plot_layout(heights = c(1, 2))

ggsave("plots/figure1.jpeg")
