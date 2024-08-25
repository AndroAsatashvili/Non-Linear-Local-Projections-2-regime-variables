#' @name create_nl_data
#' @title Compute data for nonlinear model with instrument variable approach
#' @description Function to create data for nonlinear model with instrument variable approach.
#' @param specs A \link{list}(). Inputs are created in \link{lp_nl_iv}.
#' @param endog_data A \link{data.frame} with dependent variables.
#' @return List with filled matrices of lagged left (y_nl) and right hand side (x_nl) variables.
#' @import dplyr
#' @keywords internal
#' @author Philipp Adämmer
#' 
#' ##########################################
#' Esta función despúes de las líneas de Specs
#' ###########################################
#' 


create_nl_data_prueba <- function(specs, endog_data){
  
  # Check whether lag lengths have to be determined
  if (is.nan(specs$lags_criterion)) {
    
    # Get transition probabilities by logistic function for switching_var1?
    if(isTRUE(specs$use_logistic_var1)){
      
      fz_var1      <- get_vals_switching_var1(specs$switching_var1, specs)    
      
    } else {
      
      fz_var1      <- specs$switching_var1
      
      # Use first lag of value from switching function?
      #if(isTRUE(specs$lag_switching_var1)){
        
        #fz_var1      <-  dplyr::lag(fz_var1, 1)
        
        
      #}
      
    }
    
    # Get transition probabilities by logistic function for switching_var2?
    if(isTRUE(specs$use_logistic_var2)){
      
      fz_var2      <- get_vals_switching_var2(specs$switching_var2, specs)
      
    } else {
      
      fz_var2      <- specs$switching_var2
      
      # Use first lag of value from switching function?
      #if(isTRUE(specs$lag_switching_var2)){
        
        #fz_var2      <-  dplyr::lag(fz_var2, 1)
        
        
      #}
      
    }
    
    
    
    # Select data for endogenous variables       
    y_nl    <- endog_data
    
    # Make exogenous lagged data
    if(specs$lags_endog_nl == 0){
      x_nl <- data.frame(x = rep(Inf, nrow(endog_data)))
    } else {
      x_nl <- create_lags(endog_data, specs$lags_endog_nl)
    }
    
    # Save names of exogenous variables
    linear_names <- names(x_nl)
    
    # Create tibbles with exogenous regime data and combine them to one data set (for both switching vars)
    x_nl_s1      <- x_nl %>%
      dplyr::mutate_all(list(s1 = ~.*((1 - fz_var1)*(1 - fz_var2))))  %>%            ### OJO## Creo que aquí se definen los regimenes
      dplyr::select(-one_of(linear_names))
    
    x_nl_s2      <- x_nl %>%
      dplyr::mutate_all(list(s2 = ~.*((1-fz_var1)*(fz_var2))))  %>%
      dplyr::select(-one_of(linear_names))
    
    x_nl_s3      <- x_nl %>%
      dplyr::mutate_all(list(s3 = ~.*((fz_var1)*(1 - fz_var2))))  %>%
      dplyr::select(-one_of(linear_names))
    
    x_nl_s4      <- x_nl %>%
      dplyr::mutate_all(list(s4 = ~.*((fz_var1)*(fz_var2))))  %>%
      dplyr::select(-one_of(linear_names))
    
    
    
    
    
    
    x_nl         <- cbind(x_nl_s1, x_nl_s2, x_nl_s3, x_nl_s4)
    
    
    # Check whether model type is 'iv'.
    # 0 = Normal model, 1 = IV type model
    # Prepare identified shock variable and add to exogenous data
    if(specs$model_type == 1){
      # Prepare shock variable
      shock           <- specs$shock
      colnames(shock) <- 'shock'
      shock_name      <- colnames(shock)
      
      # Make states of shock
      
      shock_s1       <- shock %>%
        dplyr::mutate_all(list(shock_s1 = ~.*((1 - fz_var1)*(1 - fz_var2))))  %>%
        dplyr::select(-one_of(shock_name))
      
      shock_s2       <- shock %>%
        dplyr::mutate_all(list(shock_s2 = ~.*((1-fz_var1)*(fz_var2))))        %>%
        dplyr::select(-one_of(shock_name))
      
      shock_s3       <- shock %>%
        dplyr::mutate_all(list(shock_s3 =~.*((fz_var1)*(1 - fz_var2))))  %>%
        dplyr::select(-one_of(shock_name))
      
      shock_s4       <- shock %>%
        dplyr::mutate_all(list(shock_s4 =~.*((fz_var1)*(fz_var2))))        %>%
        dplyr::select(-one_of(shock_name))
      
      
      
      
      # Add shock variable
      x_nl              <- cbind(shock_s1, shock_s2, shock_s3, shock_s4,x_nl)
    }
    
    # Include no trend, trend or quadratic trend
    if(specs$trend == 0){
      
      # Only constant
      x_nl             <-   x_nl
      
    } else if (specs$trend == 1){
      
      # Constant and trend
      x_nl            <-   x_nl %>%
        dplyr::mutate(trend = row_number())
      
    }  else {
      
      x_nl             <-   x_nl %>%
        dplyr::mutate(trend = row_number())     %>%
        dplyr::mutate(sq_trend = trend^2)
    }
    
    # Construct (lagged) exogenous data and merge it with lagged endogenous data
    if(!(is.null(specs$exog_data))){
      
      # Create lagged exogenous data and merge with endogenous data
      x_exog    <- create_lags(specs$exog_data, specs$lags_exog)     %>%
        `rownames<-`(NULL)
      
      
      x_nl      <- cbind(x_nl, x_exog)
      
    }
    
    # Add contemporaneous data if supplied
    if(!(is.null(specs$contemp_data))){
      
      x_nl                         <- cbind(x_nl , specs$contemp_data)
      
    }
    
    
    # Combine endogenous and exogenous data                          #### OJO: Aquí tener cuidado####
    yx_all    <- cbind(y_nl, fz_var1,fz_var2, x_nl) %>%
      stats::na.omit()
    
    yx_all    <- yx_all[, !(colSums(yx_all) == Inf)]
    
    y_nl      <- yx_all[, 1:ncol(endog_data)]  %>%
      as.matrix()
    
    fz_var1        <- yx_all[, (1 + ncol(endog_data))]
    
    fz_var2        <- yx_all[, (2 + ncol(endog_data))]
    
    x_nl      <- yx_all[, (3 + ncol(endog_data)): dim(yx_all)[2]]   %>%
      as.matrix()
    
    
    
    ################################################################################
  } else {
    ################################################################################
    
    # Create list to store matrices
    y_nl_store     <- rep(list(NaN), specs$max_lags)
    x_nl_store     <- rep(list(NaN), specs$max_lags)
    fz_store_var1  <- rep(list(NaN), specs$max_lags)
    fz_store_var2 <- rep(list(NaN), specs$max_lags)
    
    # Prepare data outside of loop
    
    # Create list with endogenous variables
    y_nl      <- endog_data
    
    # Get transition probabilities by logistic function for var1?
    if(isTRUE(specs$use_logistic_var1)){
      
      fz_var1      <- get_vals_switching_var1(specs$switching_var1, specs)
      
    } else {
      
      fz_var1      <- specs$switching_var1
    }
    
    # Use first lag of value from switching function for var1?
    if(isTRUE(specs$lag_switching_var1)){
      
      fz_var1            <-   dplyr::lag(fz_var1, 1)
      
    }
    
    
    # Get transition probabilities by logistic function for var2?
    if(isTRUE(specs$use_logistic_var2)){
      
      fz_var2      <- get_vals_switching_var2(specs$switching_var2, specs)
      
    } else {
      
      fz_var2      <- specs$switching_var2
    }
    
    # Use first lag of value from switching function for var2?
    if(isTRUE(specs$lag_switching_var2)){
      
      fz_var2            <-   dplyr::lag(fz_var2, 1)
      
    }
    
    
    # Prepare instrument variable
    # Check whether model type is 'iv'.
    # 0 = Normal model, 1 = IV model
    # Prepare instrument variable and add to exogenous data
    if(specs$model_type == 1){
      shock           <- specs$shock
      colnames(shock) <- 'shock'
      shock_name     <- colnames(shock)
    }
    
    
    for(ii in 1:specs$max_lags){                       ##### Aquí me quedé sabado 11####
      
      # Create lagged variables
      x_nl_temp      <- create_lags(endog_data, ii)
      
      linear_names   <- names(x_nl_temp)
      
      x_nl_s1      <- x_nl_temp %>%
        dplyr::mutate_all(list(s1 = ~.*((1 - fz_var1)*(1 - fz_var2)))) %>%
        dplyr::select(-one_of(linear_names))
      
      x_nl_s2        <- x_nl_temp %>%
        dplyr::mutate_all(list(s2 = ~.*((1 - fz_var1)*(fz_var2))))        %>%
        dplyr::select(-one_of(linear_names))
      
      x_nl_s3        <- x_nl_temp %>%
        dplyr::mutate_all(list(s3 = ~.*((fz_var1)*(1 - fz_var2)))) %>%
        dplyr::select(-one_of(linear_names))
      
      x_nl_s4        <- x_nl_temp %>%
        dplyr::mutate_all(list(s4 = ~.*(fz_var1*fz_var2)))        %>%
        dplyr::select(-one_of(linear_names))
      
      x_nl            <- cbind(x_nl_s1, x_nl_s2, x_nl_s3, x_nl_s4)
      
      
      # Prepare states of instrument
      if(specs$model_type == 1){
        shock_s1      <- shock %>%
          dplyr::mutate_all(list(shock_s1 = ~.*((1 - fz_var1)*(1 - fz_var2))))  %>%
          dplyr::select(-one_of(shock_name))
        
        shock_s2      <- shock %>%
          dplyr::mutate_all(list(shock_s2 = ~.*((1 - fz_var1)*(fz_var2))))        %>%
          dplyr::select(-one_of(shock_name))
        
        shock_s3      <- shock %>%
          dplyr::mutate_all(list(shock_s3 = ~.*((fz_var1)*(1 - fz_var2))))  %>%
          dplyr::select(-one_of(shock_name))
        
        shock_s4      <- shock %>%
          dplyr::mutate_all(list(shock_s4 = ~.*((fz_var1)*(fz_var2))))        %>%
          dplyr::select(-one_of(shock_name))
        
        # Add state instruments
        x_nl          <- cbind(shock_s1, shock_s2, shock_s3, shock_s4 , x_nl)
      }
      
      # Add trend if set
      if(specs$trend == 0){
        
        x_nl            <-   x_nl
        
        
      } else if (specs$trend == 1){
        
        x_nl            <-   x_nl                                    %>%
          dplyr::mutate(trend = row_number())
        
        
      }  else {
        
        x_nl            <-   x_nl                                       %>%
          dplyr::mutate(trend = row_number())      %>%
          dplyr::mutate(sq_trend = trend^2)
      }
      
      
      # Construct (lagged) exogenous data and merge it with lagged endogenous data
      if(!(is.null(specs$exog_data))){
        
        x_exog    <- create_lags(specs$exog_data, specs$lags_exog)     %>%
          `rownames<-`(NULL)
        x_nl      <- cbind(x_nl, x_exog)
        
      }
      
      # Add contemporaneous data if set
      if(!(is.null(specs$contemp_data))){
        
        x_nl      <- cbind(x_nl , specs$contemp_data)
        
      }
      
      # Merge endogenous and exogenous data
      yx_all               <-  cbind(y_nl, fz_var1, fz_var2, x_nl)         %>%
        stats::na.omit()
      
      y_nl_store[[ii]]      <-  yx_all[, 1:ncol(endog_data)]  %>%
        as.matrix()
      
      fz_store_var1[[ii]]        <-  yx_all[, (1 + ncol(endog_data))]
      
      fz_store_var2[[ii]]        <-  yx_all[, (2 + ncol(endog_data))]
      
      x_nl_store[[ii]]      <-  yx_all[, (3 + ncol(endog_data)): dim(yx_all)[2]]   %>%
        as.matrix()
      
      
      
      
    }
    # Save values te return in list
    y_nl <- y_nl_store
    x_nl <- x_nl_store
    fz_var1   <- fz_store_var1
    fz_var2 <- fz_store_var2
  }
  
  return(list(y_nl = y_nl, x_nl = x_nl, fz_var1 = fz_var1, fz_var2 = fz_var2))
}