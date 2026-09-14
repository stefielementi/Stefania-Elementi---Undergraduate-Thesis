####MAIN SCRIPT FOR FINAL PAPER REPLICATION###  

# ==================================================
# 0) SET UP
# ==================================================
rm(list = ls())
gc()

# ==================================================
# 1) CLEAN NARRATIVE-BASED DATASETS
# ==================================================

source("Code/Exogenous/clean_data_OECD.R")
source("Code/Exogenous/clean_macrodata.R")
source("Code/Exogenous/clean_episodes_data.R")
source("Code/Exogenous/merge_raw_data.R")
source("Code/Exogenous/generate_variables.R")


# ==================================================
# 2) CLEAN CAPB-BASED DATASETS  
# ==================================================

source("Code/OECD/clean_data_OECD.R") 
source("Code/OECD/gen_episode_OECD.R") 
source("Code/OECD/gen_full_OECD_dataset.R") 

# ==================================================
# 3) REGRESSION ANALYSIS  
# ==================================================

source("Code/all regressions.R")

# ==================================================
# 4) TABLES  
# ==================================================

source("Code/Exogenous/episodes_table.R")
source("Code/OECD/OECD_episodes_table.R")

# ==================================================
# 5) FIGURES 
# ==================================================

source("Code/Exogenous/exo_fig3.R")
source("Code/Exogenous/exo_fig4.R")
source("Code/OECD/OECD_fig3.R")
source("Code/OECD/OECD_fig4.R")
source("Code/episode_comparison.R")




print("End of Replication")





