RIitemfitPCM3 <- function(dfin, samplesize, nsamples, zstd_min = -1.96, zstd_max = 1.96,
                         msq_min = 0.7, msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                         output = "table", tbl_width = 65, method = "conditional",
                         simcut = FALSE, gf, limit = "99") {
  
  if (missing(samplesize)) {
    df.erm <- PCM(dfin) # run PCM model
    
    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)
      
      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            item.fit$i.outfitZ,
                                            item.fit$i.infitZ)) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        rownames_to_column("Item")
      
      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
      
    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional ZSTD
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)
      
      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = item.fit$i.outfitZ,
                                   InfitZSTD = item.fit$i.infitZ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }
    
    if (simcut == TRUE) {
      if(missing(gf)) {
        stop("When `simcut = TRUE` you need to specify a `gf` object, output from `RIgetfit()`")
      }
      iterations <- length(gf) - 3
      
      if (limit == "99") {
        lo_hi <- bind_rows(gf[1:(length(gf)-3)]) %>%
          group_by(item) %>% 
          summarise(max_infit_msq = quantile(infit_msq, .99),
                    min_infit_msq = quantile(infit_msq, .01),
                    max_outfit_msq = quantile(outfit_msq, .99),
                    min_outfit_msq = quantile(outfit_msq, .01),
                    max_infit_zstd = quantile(infit_zstd, .99),
                    min_infit_zstd = quantile(infit_zstd, .01),
                    max_outfit_zstd = quantile(outfit_zstd, .99),
                    min_outfit_zstd = quantile(outfit_zstd, .01)
          ) %>% 
          as.data.frame()
        
        for (i in 1:nrow(lo_hi)) {
          item.fit.table[i,"OutfitZSTD"] <- cell_spec(item.fit.table[i,"OutfitZSTD"], 
                                                      color = ifelse(item.fit.table[i,"OutfitZSTD"] < lo_hi[i,"min_outfit_zstd"], "red",
                                                                     ifelse(item.fit.table[i,"OutfitZSTD"] > lo_hi[i,"max_outfit_zstd"], "red", "black")))
          item.fit.table[i,"InfitZSTD"] <- cell_spec(item.fit.table[i,"InfitZSTD"], 
                                                     color = ifelse(item.fit.table[i,"InfitZSTD"] < lo_hi[i,"min_infit_zstd"], "red",
                                                                    ifelse(item.fit.table[i,"InfitZSTD"] > lo_hi[i,"max_infit_zstd"], "red", "black")))
          item.fit.table[i,"OutfitMSQ"] <- cell_spec(item.fit.table[i,"OutfitMSQ"], 
                                                     color = ifelse(item.fit.table[i,"OutfitMSQ"] < lo_hi[i,"min_outfit_msq"], "red",
                                                                    ifelse(item.fit.table[i,"OutfitMSQ"] > lo_hi[i,"max_outfit_msq"], "red", "black")))
          item.fit.table[i,"InfitMSQ"] <- cell_spec(item.fit.table[i,"InfitMSQ"], 
                                                    color = ifelse(item.fit.table[i,"InfitMSQ"] < lo_hi[i,"min_infit_msq"], "red",
                                                                   ifelse(item.fit.table[i,"InfitMSQ"] > lo_hi[i,"max_infit_msq"], "red", "black")))
        }
        
        kbl_rise(item.fit.table) %>% 
          footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))
        
        
      } else if (limit == "max") {
        
        lo_hi <- bind_rows(gf[1:(length(gf)-3)]) %>%
          group_by(item) %>% 
          summarise(max_infit_msq = max(infit_msq),
                    min_infit_msq = min(infit_msq),
                    max_outfit_msq = max(outfit_msq),
                    min_outfit_msq = min(outfit_msq),
                    max_infit_zstd = max(infit_zstd),
                    min_infit_zstd = min(infit_zstd),
                    max_outfit_zstd = max(outfit_zstd),
                    min_outfit_zstd = min(outfit_zstd)
          ) %>% 
          as.data.frame()
        
      }
      
      for (i in 1:nrow(lo_hi)) {
        item.fit.table[i,"OutfitZSTD"] <- cell_spec(item.fit.table[i,"OutfitZSTD"], 
                                                    color = ifelse(item.fit.table[i,"OutfitZSTD"] < lo_hi[i,"min_outfit_zstd"], "red",
                                                                   ifelse(item.fit.table[i,"OutfitZSTD"] > lo_hi[i,"max_outfit_zstd"], "red", "black")))
        item.fit.table[i,"InfitZSTD"] <- cell_spec(item.fit.table[i,"InfitZSTD"], 
                                                   color = ifelse(item.fit.table[i,"InfitZSTD"] < lo_hi[i,"min_infit_zstd"], "red",
                                                                  ifelse(item.fit.table[i,"InfitZSTD"] > lo_hi[i,"max_infit_zstd"], "red", "black")))
        item.fit.table[i,"OutfitMSQ"] <- cell_spec(item.fit.table[i,"OutfitMSQ"], 
                                                   color = ifelse(item.fit.table[i,"OutfitMSQ"] < lo_hi[i,"min_outfit_msq"], "red",
                                                                  ifelse(item.fit.table[i,"OutfitMSQ"] > lo_hi[i,"max_outfit_msq"], "red", "black")))
        item.fit.table[i,"InfitMSQ"] <- cell_spec(item.fit.table[i,"InfitMSQ"], 
                                                  color = ifelse(item.fit.table[i,"InfitMSQ"] < lo_hi[i,"min_infit_msq"], "red",
                                                                 ifelse(item.fit.table[i,"InfitMSQ"] > lo_hi[i,"max_infit_msq"], "red", "black")))
      }
      
      kbl_rise(item.fit.table) %>% 
        footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))
      
    } else {
      
      if (output == "table") {
        # create table that highlights cutoff values in red
        item.fit.table %>%
          mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                   ifelse(OutfitZSTD > zstd_max, "red", "black")
          ))) %>%
          mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                                 ifelse(InfitZSTD > zstd_max, "red", "black")
          ))) %>%
          mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                                 ifelse(OutfitMSQ > msq_max, "red", "black")
          ))) %>%
          mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                               ifelse(InfitMSQ > msq_max, "red", "black")
          ))) %>%
          kbl(booktabs = T, escape = F,
              table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
          # bootstrap options are for HTML output
          kable_styling(
            bootstrap_options = c("striped", "hover"),
            position = "left",
            full_width = F,
            font_size = fontsize,
            fixed_thead = T
          ) %>%
          column_spec(1, bold = T) %>%
          row_spec(0, bold = T) %>%
          kable_classic(html_font = fontfamily) %>%
          # latex_options are for PDF output
          kable_styling(latex_options = c("striped", "scale_down")) %>%
          footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))
        
      } else if (output == "dataframe") {
        return(item.fit.table)
      } else if (output == "quarto") {
        knitr::kable(item.fit.table)
      }
    }
    
  } else {
    df.erm <- PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    ple <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(ple)
    
    # ZSTD multisample
    outfitZ <- c()
    infitZ <- c()
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
      #df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ <- cbind(outfitZ, item.fit.z$i.outfitZ)
      infitZ <- cbind(infitZ, item.fit.z$i.infitZ)
    }
    
    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)
      
      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            rowMeans(outfitZ),
                                            rowMeans(infitZ)
      )) %>%
        round(3) %>%
        rownames_to_column("Item")
      
      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
      
    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)
      
      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = rowMeans(outfitZ),
                                   InfitZSTD = rowMeans(infitZ)
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }
    
    if (output == "table") {
      # create table that highlights cutoff values in red
      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                 ifelse(OutfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                               ifelse(InfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                               ifelse(OutfitMSQ > msq_max, "red", "black")
        ))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                             ifelse(InfitMSQ > msq_max, "red", "black")
        ))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(
          bootstrap_options = c("striped", "hover"),
          position = "left",
          full_width = F,
          font_size = fontsize,
          fixed_thead = T
        ) %>%
        column_spec(1, bold = T) %>%
        row_spec(0, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped", "scale_down")) %>%
        footnote(general = paste0("MSQ values are based on a sample size of ", nrow(dfin)," respondents,\n using ",method," estimation.\n",
                                  "ZSTD values are the means from ", nsamples, " subsamples, each consisting\n of ", samplesize, " random respondents."))
      
    } else if (output == "dataframe") {
      return(item.fit.table)
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }
    
  }
}

RIitemfitPCM3(pcmdat2, simcut = T, gf = gf, limit = "99")
