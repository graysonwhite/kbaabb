#' @title kbaabb
#'
#' @description \code{kbaabb()} generates an imputed population dataset based on
#' methodology introduced in White et al. (2025).
#'
#' @param survey_data A dataframe containing the survey data to be used for the 
#' imputation (i.e. "the sample"). This dataframe must contain all variables 
#' listed in \code{formula} and \code{strata}.
#' @param population_data A dataframe containing the population data for
#' imputation to occur on (i.e. "the population"). This dataframe must contain 
#' all auxiliary variables listed in \code{formula} and \code{strata}.
#' @param formula The formula specified for imputation, taking the form 
#' \code{y ~ x_1 + x_2 + ... + x_n} where \{x_1, x_2, ..., x_n\} is the set of 
#' auxiliary variables used for imputation and y is the response. 
#' @param k Integer. The number of neighbors used in the \code{k} nearest neighbors
#' imputation
#' @param strata Character. The name of a variable to be used for 
#' stratification. If \code{NULL} (default), imputation is performed without
#' stratification. Otherwise, stratification occurs based on the variable
#' specified in \code{strata}. It is advised to provide strata.
#' @param center_scale Logical. If \code{TRUE} (default), auxiliary variables are
#' centered and scaled (mean = 0, variance = 1) based on the population data. 
#' Otherwise, the original sample and population dataframes supplied by the user
#' are used in an unmodified form. 
#' @param seed A seed to be set for reproducibility. 
#' @param ... Currently ignored. For extendability.
#'
#' @return A \code{kbaabb} object including:
#' \itemize{
#'  \item \code{imputed_population_data}: The population data
#'  \item \code{survey_data}: Original sample data
#'  \item \code{k}: Value for \code{k}
#'  \item \code{stratified}: Indicator of if stratifying occured, as well as
#'  \item \code{strata}: The stratifying variable
#'  \item \code{center_scale}: If centering and scaling occured
#'  \item \code{formula}: The formula used for population imputation
#' }
#' 
#' @examples
#' # KBAABB imputation for k = 5, stratifying by `tnt`:
#' kbaabb(survey_data = SJC_sample,
#'        population_data = SJC_population,
#'        formula = biomass ~ tcc + elev,
#'        k = 5,
#'        strata = "tnt", 
#'        center_scale = TRUE,
#'        seed = 37)
#'        
#' # and without stratification
#' kbaabb(survey_data = SJC_sample,
#'        population_data = SJC_population,
#'        formula = biomass ~ tcc + elev,
#'        k = 5,
#'        center_scale = TRUE,
#'        seed = 37)
#' @export
kbaabb <- function(survey_data, # dataframe (to be coerced into a matrix)
                   population_data, # dataframe (to be coerced into a matrix)
                   formula, # formula
                   k = 10, # positive integer; nearest neighbors
                   strata = NULL, # NULL or character 
                   center_scale = TRUE, # logical
                   seed = NULL,  # numeric
                   ...) {
  validate_parameters(
    survey_data,
    population_data,
    formula,
    k,
    strata,
    center_scale
  )

  # set seed if specified
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # make sure only (object) class present is a data.frame (not a tibble or sf etc)
  survey_data <- as.data.frame(survey_data)
  population_data <- as.data.frame(population_data)

  # set up data
  y_var <- all.vars(formula[-3])
  x_vars <- all.vars(formula[-2])
  all_vars <- c(y_var, x_vars, strata)
  X_vars <- c(x_vars, strata)
  
  validate_variables(x_vars, y_var, survey_data, population_data)
  
  survey_data <- survey_data[,all_vars]
  # will trim population data shortly, want to retain part, though,
  # and we should trim NAs first
  
  if (any(is.na(population_data[,X_vars]))) {
    message("NAs present in population data. Removing these rows.")
    valid_indices = rownames(population_data)
    for (i in 1:length(X_vars)) {
      valid_indices = intersect(valid_indices, 
                                which(!is.na(population_data[,X_vars[i]])))
    }
    population_data = population_data[valid_indices,]
  }
  
  if (any(is.na(survey_data))) {
    message("NAs present in survey data. Removing these rows.")
    valid_indices = rownames(survey_data)
    for (i in 1:length(all_vars)) {
      valid_indices = intersect(valid_indices, 
                                which(!is.na(survey_data[,all_vars[i]])))
    }
    survey_data = survey_data[valid_indices,]
  }

  population_data_og <- population_data
  population_data <- population_data[,X_vars]
  
  # stratify if a strata variable is specified
  if (!is.null(strata)) {
    # get levels of strata
    strata_levels <- unique(survey_data[[strata]])
    # loop through levels to create stratified datasets
    population_data.list <- list()
    survey_data.list <- list()

    # pre-compute the levels in both strata for error handling
    strata_pop <- population_data[[strata]]
    strata_surv <- survey_data[[strata]]

    # strata_levels are unique(strata_surv)
    if ((length(setdiff(strata_levels, unique(strata_pop))) != 0) ||
        (length(setdiff(unique(strata_pop), strata_levels)) != 0)) {
      warning("Nonequal strata between survey data and population data. ",
              "Subseting population data to only use common strata.")
      # subset strata levels as described above
      strata_levels <- intersect(strata_levels, unique(strata_pop))
      subset_idx <- population_data[[strata]] %in% strata_levels
      population_data <- population_data[subset_idx,]
      population_data_og <- population_data_og[subset_idx,]
      survey_data <- survey_data[survey_data[[strata]] %in% strata_levels,]
      
      strata_pop <- population_data[[strata]]
      strata_surv <- survey_data[[strata]]
    }

    for (i in 1:length(strata_levels)) {
      # filter population for a particular strata
      population_data.list[[i]] <- population_data[population_data[[strata]] == strata_levels[i],]
      
      # filter survey data for a particular strata
      survey_data.list[[i]] <- survey_data[strata_surv == strata_levels[i],]
    }
    stratified <- TRUE
  } else {
    message("No stratifying variables => pooling all data together.")
    stratified <- FALSE
    # add a "dummy" strata variable for consistency with the stratified case
    population_data$strata_indicator <- 1
    
    survey_data$strata_indicator <- 1
    
    strata_levels <- 1
    
    # lists of length 1 mimicing the structure of nontrivial stratification
    population_data.list <- list(population_data)
    survey_data.list <- list(survey_data)
  }
  
  # get just the covariates specified
  population_data.justx.list <- list()
  survey_data.justx.list <- list()
  for (i in 1:length(strata_levels)) {
    # select just the X's to center and scale
    population_data.justx.list[[i]] <- population_data.list[[i]][ , x_vars] 
    survey_data.justx.list[[i]] <- survey_data.list[[i]][ , x_vars]
    # center and scale all X's if specified (the default)
    if (center_scale) {
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
  # we don't need the probabilities to sum to one for sample
  # sample will automatically normalize probabilities.
  
  # alternatively we could impute the last probability as the difference between
  # the preceeding probabilities and 1, like below.
  # KBAABB_probs[k] <- 1 - sum(KBAABB_probs[1:(k-1)])
  
  ## next, find donors, choose NNs, and impute
  out_len <- length(strata_levels)
  nns_subset <- nrecip <- which_knn <- donating_rows <- donating_df <- imputed_df <-
    vector(mode = "list", length = out_len)
  
  for (i in 1:length(strata_levels)) {
    replace_k = FALSE 
    # if we have very small samples (<k) in some strata, to avoid errors
    # with FNN::get.knnx(), we reduce the size of k so that k is at most
    # the size of the observations. When we do this we preserve the value 
    # of k so we can replace it, and this is a logical flag noting if
    # we've done that.

    # ensure FNN::get.knnx() will work
    if (nrow(survey_data.justx.list[[i]]) < k) {
      warning("strata level ", i, " has ", 
              nrow(survey_data.justx.list[[i]]), " observations. This is",
              " less than k. k is temporarily reduced to this number.")
      replace_k = TRUE
      old_k = k
      old_probs = KBAABB_probs
      k = nrow(survey_data.justx.list[[i]])
      KBAABB_probs = KBAABB_probs[1:k] # it doesn't matter that they don't add to one
    }
    
    # find donors
    nns_subset[[i]] <- FNN::get.knnx(
      survey_data.justx.list[[i]],
      population_data.justx.list[[i]],
      k = k
    )
    # choose NNs
    nrecip[[i]] <- nrow(population_data.justx.list[[i]])
    # get which KNN index to impute each unit in receiving dataset with
    which_knn[[i]] <- sample.int(k, 
                                 size = nrecip[[i]],
                                 prob = KBAABB_probs,
                                 replace = TRUE)
    # get.knnx() gives the NNs for survey data, so get this for donating rows
    # and index out by which_knn. The below indexes out every row as well as just
    # the NN index computed above.
    donating_rows[[i]] <- nns_subset[[i]]$nn.index[cbind(1:nrecip[[i]], which_knn[[i]])]
    # aggregate the imputed response variables into one data.frame
    
    donating_df[[i]] <- as.data.frame(survey_data.list[[i]][donating_rows[[i]],y_var])
    colnames(donating_df[[i]]) <- y_var
    
    # add the imputed values to the observed data in the recieving data
    imputed_df[[i]] <- cbind(population_data.list[[i]][1:nrecip[[i]], ],
                             donating_df[[i]])
    if (replace_k) {
      k = old_k
      KBAABB_probs = old_probs
    }
  }
  # turn from list into df
  imputed_df <- do.call(rbind, imputed_df)
  
  # we have finished KBAABB, but as a user-facing feature, we would like to 
  # return the data-frame provided similarly to the form it was provided in
  # that is, we will add the original columns back in.
  # Well actually, we add the imputed data back to the original, but same thing
  rows = rownames(imputed_df) # first get the columns that we imputed (i.e., not NA)
  imputed_pop = population_data_og[rows,] # and subset the original provided data 
  imputed_pop[[y_var]] = imputed_df[[y_var]] # the last thing we need to do is add the y_var
  
  
  # eventually, we can return a list with parameter values, sample dataset etc. for more info,
  # currently just returning the imputed population
  outlst <- list(imputed_population_data = imputed_pop,
                 survey_data = survey_data,
                 k = k,
                 stratified = stratified,
                 strata = strata,
                 center_scale = center_scale,
                 formula = formula
                 )
  if (!is.null(seed)) {
    outlst$seed <- seed
  }
  class(outlst) <- "kbaabb"
  return(outlst)
}

#' Validate initial parameters
#' @noRd
validate_parameters <- function(survey_data,
                                population_data,
                                formula,
                                k,
                                strata,
                                center_scale) {
  if (!is.numeric(k)) {
    stop(paste0("Must provide a numeric value for k."))
  }
  if (!(is.character(strata) || is.null(strata))) {
    stop(paste0("Must provide character or null for strata"))
  } else if (!is.null(strata)) {
    if (length(strata) > 1) {
      stop(
        paste0("Can only supply one stratifying variable.",
               " If you want to use multiple stratifying variables, consider",
               " using pivot_longer().")
      )
    } else if (!(strata %in% colnames(survey_data))) {
      stop(
        paste0("Supplied strata is not in your donor dataset. Please ensure that",
               " your data include the stratifying variable.")
      )
    } else if (!(strata %in% colnames(population_data))) {
      stop(
        paste0("Supplied strata not in your recieving dataset. Please ensure that",
               " your data include the stratifying variable.")
      )
    }
  }
  if (!center_scale) {
    warning(
      paste0("You have chosen not to center or scale your variables. This may",
             " lead to biased KNN draws.")
    )
  }
  if (!inherits(formula, "formula")) {
    tryCatch({formula = stats::as.formula(formula)},
             error = function(e) {
               stop("formula ", formula, " is not a formula and ",
                    "cannot be coerced into a formula.\n", e)
             })
  }
  
}

#' Validate covariates/response
#' @noRd
validate_variables = function(
    x_vars,
    y_var,
    survey_data,
    population_data
) {
  if (length(x_vars) == 0) {
    stop(paste("Supplied no auxiliaries variables. Please supply auxiliary",
               "variables."))
  }
  if (length(y_var) == 0) {
    stop(paste("Supplied no response variables. Cannot impute data without",
               "a response variable."))
  } 
  
  if (!(y_var %in% colnames(survey_data))) {
    stop("Response variable not in survey data.")
  }
  
  temp_setdiff_varnames = setdiff(x_vars, colnames(survey_data))
  if (length(temp_setdiff_varnames) > 0) {
    stop(paste("Variable", temp_setdiff_varnames, "is not in survey data."))
  }
  
  temp_setdiff_varnames = setdiff(x_vars, colnames(population_data))
  if (length(setdiff(x_vars, colnames(population_data))) > 0) {
    stop(paste("Variable", temp_setdiff_varnames, "is not in survey data."))
  }
}
