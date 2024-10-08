#' Helper function to create a resampled dataset
#' 
#' @param data \code{data.frame} containing time-series features
#' @param train_rows \code{integer} denoting the number of cases in the train set
#' @param test_rows \code{integer} denoting the number of cases in the test set
#' @param train_groups \code{data.frame} containing proportions of each class in original train split
#' @param test_groups \code{data.frame} containing proportions of each class in original test split
#' @param seed \code{integer} denoting fixed value for R's pseudorandom number generator
#' @return \code{list} containing new train and test data
#' @author Trent Henderson
#' 

resample_data <- function(data, train_rows, test_rows, train_groups, test_groups, seed){
  
  if(seed == 1){
    
    # Use pre-designated train-test split
    
    train <- data %>%
      filter(set_split == "Train") %>%
      pivot_wider(id_cols = c(id, set_split, group), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(id, set_split))
    
    test <- data %>%
      filter(set_split == "Test") %>%
      pivot_wider(id_cols = c(id, set_split, group), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(id, set_split))
    
  } else{
    
    set.seed(seed)
    
    # Create resampled dataset
    
    newdata <- list()
    
    # Randomly allocate the correct number of each class to train and test as manual stratified sample
    
    resampled <- list()
    
    for(i in unique(data$group)){
      feasible_ids <- data %>%
        dplyr::filter(group == i) %>%
        dplyr::select(c(id)) %>%
        dplyr::distinct() %>%
        dplyr::pull(id)
      
      n <- train_groups %>%
        dplyr::filter(group == i) %>%
        pull(counter)
      
      traindata <- data.frame(id = sample(feasible_ids, size = n)) %>%
        dplyr::mutate(set_split_new = "Train")
      
      testdata <- data.frame(id = feasible_ids[!feasible_ids %in% traindata$id]) %>%
        dplyr::mutate(set_split_new = "Test")
      
      stopifnot((nrow(traindata) + nrow(testdata)) == length(feasible_ids))
      
      joined <- dplyr::bind_rows(traindata, testdata)
      resampled[[i]] <- joined
    }
    
    resampled <- do.call(rbind, resampled)
    rownames(resampled) <- c()
    
    # Properly set up train and test data
    
    newdata <- data %>%
      dplyr::left_join(resampled, by = c("id" = "id"))
    
    train <- newdata %>%
      # dplyr::filter(.data$set_split_new == "Train") %>% # Original
      # dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>% # Original
      dplyr::filter("set_split_new" == "Train") %>% # Modified all instances .data$xyz to "xyz" where "xyz" = a data field name.
      dplyr::select(c("id","group","names","values")) %>% # Modified all instances .data$xyz to "xyz" where "xyz" = a data field name.
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      # dplyr::select(-c(.data$id)) # Original
      dplyr::select(-c("id")) # Modified all instances .data$xyz to "xyz" where "xyz" = a data field name.
    
    test <- newdata %>%
      # dplyr::filter(.data$set_split_new == "Test") %>% # Original
      # dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>% # Original
      dplyr::filter("set_split_new" == "Test") %>% # Modified all instances .data$xyz to "xyz" where "xyz" = a data field name.
      dplyr::select(c("id", "group", "names", "values")) %>% # Modified all instances .data$xyz to "xyz" where "xyz" = a data field name.
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      # dplyr::select(-c(.data$id)) # Original
      dplyr::select(-c("id")) # Modified all instances .data$xyz to "xyz" where "xyz" = a data field name.
  }
  
  data_list <- list(train, test)
  names(data_list) <- c("Train", "Test")
  return(data_list)
}
