RIitemfit <- function(data, simcut, output = "table", ...) {
  
  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    erm_out <- RM(data)
  } else if(max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
    erm_out <- PCM(data)
  } 
  
  
  # get conditional MSQ
  cfit <- iarm::out_infit(erm_out)
  
  # create dataframe
  item.fit.table <- data.frame(InfitMSQ = cfit$Infit,
                               OutfitMSQ = cfit$Outfit
  ) %>%
    round(3) %>%
    rownames_to_column("Item")
  
  if (!missing(simcut) & output == "table") {
    
    # get number of iterations used to get simulation based cutoff values
    iterations <- length(simcut) - 3
    
    # summarise simulations and set cutoff values
    lo_hi <- bind_rows(simcut[1:(length(simcut)-3)]) %>%
      summarise(max_infit_msq = quantile(InfitMSQ, .99),
                min_infit_msq = quantile(InfitMSQ, .01),
                max_outfit_msq = quantile(OutfitMSQ, .99),
                min_outfit_msq = quantile(OutfitMSQ, .01)
      )
    
    # get upper/lower values into a table
    fit_table <-
      bind_rows(simcut[1:(length(simcut)-3)]) %>%
      group_by(Item) %>%
      summarise(inf_thresh = paste0("[",round(quantile(InfitMSQ, .01),3),", ",round(quantile(InfitMSQ, .99),3),"]"),
                outf_thresh = paste0("[",round(quantile(OutfitMSQ, .01),3),", ",round(quantile(OutfitMSQ, .99),3),"]")
      )
    
    # set conditional highlighting based on cutoffs and output table
    item.fit.table %>%
      mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < lo_hi$min_infit_msq, "red",
                                                           ifelse(InfitMSQ > lo_hi$max_infit_msq, "red", "black")
      ))) %>%
      mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < lo_hi$min_outfit_msq, "red",
                                                             ifelse(OutfitMSQ > lo_hi$max_outfit_msq, "red", "black")
      ))) %>%
      add_column(`Infit thresholds` = fit_table$inf_thresh, .after = "InfitMSQ") %>% 
      add_column(`Outfit thresholds` = fit_table$outf_thresh, .after = "OutfitMSQ") %>% 
      kbl_rise(...) %>% 
      footnote(general = paste0("MSQ values based on ", method," estimation (n = ", nrow(data),"). 
                                Conditional highlighting uses simulation based thresholds from ", iterations," simulated datasets."))
    
  } else if (output == "table") {
    kbl_rise(item.fit.table) %>% 
      footnote(general = paste0("MSQ values based on ", method," estimation (n = ", nrow(data),")."))
    
  } else if (output == "dataframe") {
    return(item.fit.table)
  } else if (output == "quarto") {
    knitr::kable(item.fit.table)
  }
}

RIitemfit(pcmdat2)

RIitemfit(raschdat1)


RIgetfit3 <- function(data, iterations, cpu = 4) {
  
  sample_n <- nrow(data)
  
  require(doParallel)
  registerDoParallel(cores = cpu)

  if (missing(iterations)) {
    stop("Please set a number of iterations (at least 1000 is recommended).")
  }
  
  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
 
     } else if(max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    
    # estimate item threshold locations from data
    erm_out <- eRm::RM(data)
    item_locations <- erm_out$betapar * -1
    names(item_locations) <- names(data)
    
    # estimate theta values from data using WLE
    thetas <- RIestThetas(data, model = "RM")
    
    # get sample theta properties for simulation input
    sample_mean <- mean(thetas$WLE, na.rm = TRUE)
    sample_sd <- sd(thetas$WLE, na.rm = TRUE)
    
    fitstats <- list()
    fitstats <- foreach(icount(iterations)) %dopar%
      {
        inputThetas <- rnorm(n = sample_n,
                             mean = sample_mean,
                             sd = sample_sd)
        
        # simulate response data based on thetas and items above
        testData <-
          psychotools::rrm(inputThetas, item_locations, return_setting = FALSE) %>%
          as.data.frame()
        
        # get conditional MSQ
        erm_out <- RM(testData)
        cfit <- iarm::out_infit(erm_out)
        
        # create dataframe
        item.fit.table <- data.frame(InfitMSQ = cfit$Infit,
                                     OutfitMSQ = cfit$Outfit) %>%
          round(3) %>%
          rownames_to_column("Item")
      }
    
  } else if(max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
   
     # estimate item threshold locations from data
    item_locations <- RIitemparams(data, output = "dataframe") %>%
      dplyr::select(!Location) %>%
      janitor::clean_names() %>%
      as.matrix()
    
    # item threshold locations in list format for simulation function
    itemlist <- list()
    for (i in 1:nrow(item_locations)) {
      itemlist[[i]] <- list(na.omit(item_locations[i,]))
    }
    
    # estimate theta values from data using WLE
    thetas <- RIestThetas(data)
    
    # get sample theta properties for simulation input
    sample_mean <- mean(thetas$WLE, na.rm = TRUE)
    sample_sd <- sd(thetas$WLE, na.rm = TRUE)
    
    fitstats <- list()
    fitstats <- foreach(icount(iterations)) %dopar%
      {
        inputThetas <- rnorm(n = sample_n,
                             mean = sample_mean,
                             sd = sample_sd)
        
        # simulate response data based on thetas and items above
        testData <- SimPartialScore(
          deltaslist = itemlist,
          thetavec = inputThetas
        ) %>%
          as.data.frame()
        
        # get conditional MSQ
        erm_out <- PCM(testData)
        cfit <- iarm::out_infit(erm_out)
        
        # create dataframe
        item.fit.table <- data.frame(InfitMSQ = cfit$Infit,
                                     OutfitMSQ = cfit$Outfit) %>%
          round(3) %>%
          rownames_to_column("Item")
      }
  }
  
  fitstats$sample_n <- sample_n
  fitstats$sample_mean <- sample_mean
  fitstats$sample_sd <- sample_sd
  
  return(fitstats)
}

simfit <- RIgetfit3(pcmdat2, 1000, cpu = 8)

RIitemfit(pcmdat2, simcut = simfit)


RIgetfitTable <- function(gf, output = "table", limit = "99", tbl_width = 75) {
  
  iterations <- length(gf) - 3
  
  if (limit == "max") {
    
    fit_table <-
      bind_rows(gf[1:(length(gf)-3)]) %>%
      dplyr::rename(Item = item) %>%
      group_by(Item) %>%
      summarise(`Infit MSQ` = paste0("[",round(min(infit_msq),3),", ",round(max(infit_msq),3),"]"),
                `Outfit MSQ` = paste0("[",round(min(outfit_msq),3),", ",round(max(outfit_msq),3),"]"),
                `Infit ZSTD` = paste0("[",round(min(infit_zstd),3),", ",round(max(infit_zstd),3),"]"),
                `Outfit ZSTD` = paste0("[",round(min(outfit_zstd),3),", ",round(max(outfit_zstd),3),"]")
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    
  } else if (limit == "99") {
    
    fit_table <-
      bind_rows(gf[1:(length(gf)-3)]) %>%
      dplyr::rename(Item = item) %>%
      group_by(Item) %>%
      summarise(`Infit MSQ` = paste0("[",round(quantile(infit_msq, .01),3),", ",round(quantile(infit_msq, .99),3),"]"),
                `Outfit MSQ` = paste0("[",round(quantile(outfit_msq, .01),3),", ",round(quantile(outfit_msq, .99),3),"]"),
                `Infit ZSTD` = paste0("[",round(quantile(infit_zstd, .01),3),", ",round(quantile(infit_zstd, .99),3),"]"),
                `Outfit ZSTD` = paste0("[",round(quantile(outfit_zstd, .01),3),", ",round(quantile(outfit_zstd, .99),3),"]")
      )
  }
  
  if (output == "table"){
    kbl_rise(fit_table, tbl_width = tbl_width) %>%
      footnote(general = paste0("Results from ",iterations," simulated datasets with ",
                                gf$sample_n," respondents (theta mean = ", round(gf$sample_mean,2),", SD = ",round(gf$sample_sd,2),")."))
  } else if (output == "quarto") {
    knitr::kable(fit_table) %>%
      add_footnote(paste0("Results from ",iterations," simulated datasets with ",
                          gf$sample_n," respondents (theta mean = ", round(gf$sample_mean,2),", SD = ",round(gf$sample_sd,2),")."),
                   notation = "none")
  } else if (output == "dataframe") {
    
    bind_rows(gf[1:(length(gf)-3)]) %>%
      dplyr::rename(Item = item) %>%
      group_by(Item) %>%
      summarise(infit_msq_lo = round(quantile(infit_msq, .01),3),
                infit_msq_hi = round(quantile(infit_msq, .99),3),
                outfit_msq_lo = round(quantile(outfit_msq, .01),3),
                outfit_msq_hi = round(quantile(outfit_msq, .99),3),
                infit_zstd_lo = round(quantile(infit_zstd, .01),3),
                infit_zstd_hi = round(quantile(infit_zstd, .99),3),
                outfit_zstd_lo = round(quantile(outfit_zstd, .01),3),
                outfit_zstd_hi = round(quantile(outfit_zstd, .99),3)
      )
  }
}

