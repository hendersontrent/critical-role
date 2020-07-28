#---------------------------------------
# This script calls a range of other
# scripts to synthesise them into
# 1 data visualisation
#---------------------------------------

#----------------------------------------
# Author: Trent Henderson, 28 July 2020
#----------------------------------------

# Load scripts

source("data-vis/ridgeplot.R")
source("data-vis/damage-and-heals.R")
source("statistical-modelling/multinom-and-acf.R")

# Produce the visual

CairoPNG("output/second-cut.png", 1000, 800)
ggarrange(the_mod,
          ggarrange(the_dens, dam_heal_plot, ncol = 2, nrow = 1),
          nrow = 2)
dev.off()