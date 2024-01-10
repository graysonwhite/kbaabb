# Adaptation of kbaabb_summer2023_jerzy to make the KBAABB
# population generation process into one function
kbaabb <- function(survey_data, # dataframe (to be coerced into a matrix)
                   population_data, # dataframe (to be coerced into a matrix)
                   formula, # formula
                   k = 10, # positive integer
                   strata = NULL, # NULL or character 
                   center_scale = TRUE, # logical
                   seed = NULL) { # numeric
  # initial checks (TODO)
  ## make sure classes are correct
  ## make sure y_var, x_vars, and strata columns are numeric 
  ## (so that the filtered data.frames can be coerced into matrices)
  
  # set seed if specified 
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # set up data
  y_var <- all.vars(formula[-3])
  x_vars <- all.vars(formula[-2])
  all_vars <- c(y_var, x_vars, strata)
  X_vars <- c(x_vars, strata)
  
  survey_data <- survey_data %>%
    dplyr::select(all_vars)
  
  population_data <- population_data %>%
    dplyr::select(X_vars)
  
  # stratify if a strata variable is specified
  if (!is.null(strata)) {
    # get levels of strata
    strata_levels <- unique(population_data[[strata]])
    # loop through levels to create stratified datasets
    population_data.list <- list()
    survey_data.list <- list()
    for (i in 1:length(strata_levels)) {
      # filter population for a particular strata
      strata_pos_pop <- which(colnames(population_data) == strata)
      strata_pop <- population_data[,strata_pos_pop] %>% pull()
      population_data.temp <- population_data[strata_pop == strata_levels[i],]
      
      # filter survey data for a particular strata
      strata_pos_surv <- which(colnames(survey_data) == strata)
      strata_surv <- survey_data[,strata_pos_surv] %>% pull()
      # this step assumes that the survey data and population data have the same
      # strata levels, which might is true in general..., but very likely given
      # a decent sample size and reasonable number of levels in the strata. 
      # ok for our purposes for now, fix later
      survey_data.temp <- survey_data[strata_surv == strata_levels[i],]
      
      population_data.list[[i]] <- population_data.temp
      survey_data.list[[i]] <- survey_data.temp 
      rm(population_data.temp, survey_data.temp)
    }
    stratified <- TRUE
  } else {
    stratified <- FALSE
    # add a "dummy" strata variable for consistency with the stratified case
    population_data <- population_data %>%
      dplyr::mutate(strata_indicator = 1)
    
    sample_data <- sample_data %>%
      dplyr::mutate(strata_indicator = 1)
    
    strata_levels <- 1
    
    # lists of length 1
    population_data.list <- list(population_data)
    survey_data.list <- list(survey_data)
  }
  
  # center and scale all X's if specified (the default)
  if (center_scale) {
    population_data.justx.list <- list()
    survey_data.justx.list <- list()
    for (i in 1:length(strata_levels)) {
      # select just the X's to center and scale
      population_data.justx.list[[i]] <- population_data.list[[i]][ , x_vars] 
      survey_data.justx.list[[i]] <- survey_data.list[[i]][ , x_vars]
      
      # then center and scale
      population_data.justx.list[[i]] <- scale(population_data.justx.list[[i]])
      survey_data.justx.list[[i]] <- scale(survey_data.justx.list[[i]],
                                           center = attr(population_data.justx.list[[i]],
                                                         "scaled:center"),
                                           scale = attr(population_data.justx.list[[i]],
                                                        "scaled:scale"))
    }
  }
  
  # do KBAABB
  ## first, set up our probability of neighbor selection for a given k
  boot_p <- 1 - exp(-1)
  KBAABB_probs <- (1 - boot_p)^((1:k)-1) * boot_p
  KBAABB_probs[k] <- 1 - sum(KBAABB_probs[1:(k-1)])
  
  ## next, find donors, choose NNs, and impute
  nns_subset <- list()
  nrecip <- list()
  which_knn <- list()
  donating_rows <- list()
  donating_df <- list()
  imputed_df <- list()
  for (i in 1:length(strata_levels)) {
    # find donors
    nns_subset[[i]] <- FNN::get.knnx(survey_data.justx.list[[i]],
                                     population_data.justx.list[[i]],
                                     k = k)
    # choose NNs
    nrecip[[i]] <- nrow(population_data.justx.list[[i]])
    which_knn[[i]] <- sample.int(k, 
                                 size = nrecip[[i]],
                                 prob = KBAABB_probs,
                                 replace = TRUE)
    donating_rows[[i]] <- nns_subset[[i]]$nn.index[cbind(1:nrecip[[i]], which_knn[[i]])]
    # impute
    donating_df[[i]] <- survey_data.list[[i]] %>%
      # I think we do not need these lines for now... might want to specify a domain
      # column though and if so we could do something like this. 
      # rename(SUBSECTION_donor = SUBSECTION) %>%
      # select(SUBSECTION_donor, BA:CARBON) %>% 
      dplyr::select(y_var) %>%
      slice(donating_rows[[i]])
    imputed_df[[i]] <- cbind(population_data.list[[i]][1:nrecip[[i]], ], donating_df[[i]])
  }
  # turn from list into df
  imputed_df <- do.call(rbind, imputed_df) # %>% 
  # arrange(cmbid) # don't have a unique id floating around yet.... 
  
  # eventually, we can return a list with parameter values, sample dataset etc. for more info,
  # currently just returning the imputed population
  return(imputed_df)
}
