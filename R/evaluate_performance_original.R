evaluate_performance <- function(data, problem_name, n_resamples = 30){
  
  message(paste0("Doing: ", problem_name, "\n"))
  
  # tmp <- data %>%
  #   filter(problem == problem_name) %>%
  #   mutate(group = as.factor(as.character(group))) %>%
  #   dplyr::select(c(id, group, set_split, names, values)) %>%
  #   pivot_wider(id_cols = c(id, group, set_split), names_from = "names", values_from = "values") %>%
  #   dplyr::select_if(~sum(!is.na(.)) > 0) %>% # Delete features that are all NaNs
  #   dplyr::select(where(~dplyr::n_distinct(.) > 1)) %>% # Delete features with constant values
  #   pivot_longer(cols = -c(id, group, set_split), names_to = "names", values_to = "values")
  
  # Replace above chunk with fn piping from line to line -- This version of the code performs the same operations but without using the pipe operator. Each step is
  
  # Filter the data
  tmp <- filter(data, problem == problem_name)
  
  # Mutate the group column
  tmp <- mutate(tmp, group = as.factor(as.character(group)))
  
  # Select specific columns
  tmp <- select(tmp, id, group, set_split, names, values)
  
  # Pivot the data to a wider format
  tmp <- pivot_wider(tmp, id_cols = c(id, group, set_split), names_from = "names", values_from = "values")
  
  # Delete features that are all NaNs
  tmp <- select_if(tmp, ~ sum(!is.na(.)) > 0)
  
  # Delete features with constant values
  tmp <- select(tmp, where(~ n_distinct(.) > 1))
  
  # Pivot the data back to a longer format
  tmp <- pivot_longer(tmp, cols = -c(id, group, set_split), names_to = "names", values_to = "values")
  
  #------------------ Find good features to retain across resamples ---------------
  
  # Get number of cases in each set
  
  train_rows <- nrow(unique(tmp[tmp$set_split == "Train", 1]))
  test_rows <- nrow(unique(tmp[tmp$set_split == "Test", 1]))
  
  # Get proportion per class in train and test to use for resample procedure
  
  train_props <- tmp %>%
    dplyr::filter(set_split == "Train") %>%
    dplyr::select(c(id, group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  test_props <- tmp %>%
    dplyr::filter(set_split == "Test") %>%
    dplyr::select(c(id, group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  #-------------------------------------------------
  # Keep all features that have enough unique values
  # to not ruin models with resampling
  #-------------------------------------------------
  
  # Generate resamples
  
  res_data <- 1:n_resamples %>%
    purrr::map(~ resample_data(tmp, train_rows = train_rows, test_rows = test_rows, train_props, test_props, .x))
  
  # Find only features across all resamples that have SD > 0
  
  good_features <- 1:n_resamples %>%
    purrr::map(~ find_good_features(res_data, .x)) %>%
    unlist()
  
  good_features <- data.frame(names = good_features) %>%
    group_by(names) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    filter(counter == max(counter)) %>%
    pull(names)
  
  # Filter each resample by the new "good" feature vector
  
  res_data <- 1:n_resamples %>%
    purrr::map(~ filter_good_features(res_data, .x, good_features = good_features))
  
  #---------------- Model fitting ----------------
  
  outs <- 1:n_resamples %>%
    purrr::map_dfr(~ fit_models(res_data, .x)) %>%
    mutate(problem = problem_name)
  
  return(outs)
}
