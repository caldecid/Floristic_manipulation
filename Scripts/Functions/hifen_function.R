# Function for assessing "hifen" incompatibilities ------------------------

#' @title Hifen incompatibilities and absences Flora de Brasil

#' @description Finding incompatibilities and absences regarding the hifen utilization between Flora de Brasil and POWO/IPNI repositories

#' @param df the angiosperm dataset provided by Flora de Brasil

#' @returns A list with three dataframes:
#'  1) Incompatibility 1: hifen in Flora de Brasil but no hifen in Powo/IPNI; 
#' 2) Incompatibility 2: hifen in POWO/IPNI but no hifen in Flora de Brasil; 
#' 3) Species with hifen in POWO/IPNI but not present in Flora de Brasil


hifen_incompatibility <- function(df){
  
  ##completing dataframe information (auxiliary function)
  
  angiosperms_bra = complete_angio_df(df = df)
  

      ######## 1st incompatibility hifen in Flora de Brasil but no hifen in Powo/IPNI
        
      first_incon <- first_incon_function(df = angiosperms_bra) 
      
      
      # Second inconsistency ----------------------------------------------------
      
      second_incon <- second_incon_function(df = angiosperms_bra)
      
      
      
      # Third inconsistency -----------------------------------------------------
      
      third_incon <- third_incon_function(df = angiosperms_bra)
        
      ##listing
      list_inconsistencies <- list(first_incon, second_incon, third_incon)
      
      names(list_inconsistencies) <- c("First inconsistency",
                                       "Second inconsistency",
                                       "Third inconsistency")
      
      return(list_inconsistencies)
  }
  
  



  
  
  