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


RIgetfit <- function(data, iterations, cpu = 4) {
  
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

RIgetfitPlot <- function(simcut) {
  require(ggdist)
  
  iterations <- length(simcut) - 3
  
    bind_rows(simcut[1:(length(simcut)-3)]) %>%
      pivot_longer(contains("MSQ"),
                   names_to = "statistic",
                   values_to = "Value") %>%
      
      ggplot(aes(x = Value, y = Item, slab_fill = after_stat(level))) +
      stat_dotsinterval(quantiles = iterations, point_interval = median_qi,
                        layout = "weave", slab_color = NA,
                        .width = c(0.66, 0.99)) +
      labs(x = "Conditional MSQ",
           y = "Item") +
      scale_color_manual(values = scales::brewer_pal()(3)[-1], aesthetics = "slab_fill", guide = "none") +
      labs(caption = str_wrap(paste0("Note: Results from ",iterations," simulated datasets with ",
                                     simcut$sample_n," respondents (mean theta = ", round(simcut$sample_mean,2),", SD = ",round(simcut$sample_sd,2),")."))
      ) +
      facet_wrap(~statistic, ncol = 2) +
      scale_x_continuous(breaks = seq(0.5,1.5,0.1), minor_breaks = NULL) +
      theme_minimal()
    
}

RIgetfitPlot(simfit)
