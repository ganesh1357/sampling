# Sampling for CEGIS KPI Survey
# Language: R
# Author: Ganesh Rao (GR), ganesh@cegis.org
# Date: 18-04-2020
# Modified on: ...
# References: For DeclareDesign, read https://declaredesign.org/r/declaredesign/

# Part 0: Clear

  rm(list=ls())

# Part 1: Load packages

  # Enter 1 if you want to install packages
  PACKAGES             <- 1
  
  packages <- c("readstata13","foreign",
               "doBy", "broom", "dplyr",
               "stargazer",
               "ggplot2", "plotly", "ggrepel",
               "RColorBrewer",
               "sp", "rgdal", "rgeos", "raster", "velox",
               "ggmap", "rasterVis", "leaflet",
               "htmlwidgets", "geosphere",
               "DeclareDesign")

  # If you selected the option to install packages, install them
  if (PACKAGES) {
    install.packages(packages,
                     dependencies = TRUE)
  }
  
  # Load all packages -- this is equivalent to using library(package) for each 
  # package listed before
  invisible(sapply(packages, library, character.only = TRUE))
  
  library("DeclareDesign")
  library("fabricatr")
  
# Part 3: Set folder paths
  
  # Add your username and folder path here (for Windows computers)
  # To find out what your username is, type Sys.getenv("USERNAME")
  if (Sys.getenv("USERNAME") == "Ganesh Rao") {
    
    projectFolder <- "C:/Users/Ganesh Rao/Documents/sampling/R"
    
  }
  
  # Project subfolders

  Input             <- file.path(projectFolder,"Input")
  Output            <- file.path(projectFolder,"Output")
  
  taskdate = "200418" # update task date
  project = "OM_KPI" # update project name
  purpose = "Sampling" # update purpose 
  
  outputfile1 <- file.path(Output,paste0(paste(taskdate, project, purpose, sep="_"), ".csv"))
  outputfile2 <- file.path(Output,paste0(paste(taskdate, project, purpose, sep="_"), ".RDS"))
  
# Part 4: Begin analysis

  # Population
    N_blocks <- 1
    N_clusters_in_block <- 589*50
    N_i_in_cluster <- 100
  # Sample
    n_clusters_in_block <- 589*25
    n_i_in_cluster <- 4
    icc <- 0.1

fixed_pop <- declare_population(block = add_level(N = N_blocks),
                                cluster = add_level(N = N_clusters_in_block), 
                                subject = add_level(N = N_i_in_cluster,
                                          latent = draw_normal_icc(mean = 0, N = N,
                                                  clusters = cluster, ICC = icc),
                                                  Y = draw_ordered(x = latent, 
                                                                   breaks = qnorm(seq(0, 1, length.out = 8)))))()


population <- declare_population(data = fixed_pop)
estimand <- declare_estimand(mean(Y), label = "Ybar")
stage_1_sampling <- declare_sampling(strata = block, clusters = cluster, 
                                     n = n_clusters_in_block, sampling_variable = "Cluster_Sampling_Prob")
stage_2_sampling <- declare_sampling(strata = cluster, n = n_i_in_cluster, 
                                     sampling_variable = "Within_Cluster_Sampling_Prob")
clustered_ses <- declare_estimator(Y ~ 1, model = lm_robust, 
                                   clusters = cluster, estimand = estimand, label = "Clustered Standard Errors")
cluster_sampling_design <- population + estimand + stage_1_sampling + stage_2_sampling + clustered_ses

# Diagnosis
diagnosis <- diagnose_design(cluster_sampling_design)

diagnosis # Need to print this table separately

# Save data as .RDS
saveRDS(diagnosis, file = outputfile2) 

### THE END