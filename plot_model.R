normplot <- function (x, ...) {
  UseMethod("normplot")
}

residplot <- function (x, ...) {
  UseMethod("residplot")
}

residplotVI <- function (x, ...) {
  UseMethod("residplotVI")
}

dispersionP <- function (x, ...) {
  UseMethod("dispersionP")
}

normplot.lm <- function(model, ncol = NULL, nrow = NULL, title = NULL) {

  lstNormP <- list()
  
  #quantil-quantil plot
  Pqqplot <- ggplot(model) + stat_qq(aes(sample = .stdresid)) +
    geom_abline() + theme_minimal()
  lstNormP[["qqplot"]] <- Pqqplot
  
  #normal curve plot
  cl <- c("normal" = "#3E5496", "sample" = "black")
  
  Pdens <- ggplot(model, aes(.resid)) + 
    geom_density(aes(y=..density.., color="sample"), size =0.71) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(fortify(model)$.resid), sd = sd(fortify(model)$.resid)),
                  aes(color = "normal"), size =0.71) + 
    scale_color_manual(values = cl) +
    labs(color = "", x = "resid") +
    theme_minimal() %+replace% theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.key.size = unit(3, "mm"))
  
  lstNormP[["density"]] <- Pdens
  
  #resume
  Presume <- wrap_plots(lstNormP, ncol = ncol, nrow = nrow) + 
    plot_annotation(subtitle = title, 
                    caption = paste0("Formula: ",deparse(model$call[[2]])))
  lstNormP[["resume"]] <- Presume
  
  return(lstNormP)
}

residplot.lm <- function(model, delog = F, ncol = NULL, nrow = NULL, title = NULL) {
  
  lstResidP <- list()
  
  if (delog == F){
    #resid x fitted
    Presid_fit <- ggplot(model, aes(x = .fitted, y = ((.resid*100)/fortify(model)[,1]))) +
      geom_point() + 
      geom_pointdensity(adjust = 4, show.legend = FALSE) +
      geom_hline(yintercept = 0, size = 0.7) +
      labs(x = "fitted", y = "resid%") +
      ylim(-1*max(abs((fortify(model)[,".resid"]*100)/fortify(model)[,1])),
           max(abs((fortify(model)[,".resid"]*100)/fortify(model)[,1])))+
      theme_minimal()
    lstResidP[["Presid_fit"]] <- Presid_fit
    
    #resid x observed
    Presid_obs <- ggplot(model, aes(x = fortify(model)[,1], y = ((.resid*100)/fortify(model)[,1]))) +
      geom_point() + 
      geom_pointdensity(adjust = 4, show.legend = FALSE) +
      geom_hline(yintercept = 0, size = 0.7) +
      labs(x = "observed", y = "resid%") +
      ylim(-1*max(abs((fortify(model)[,".resid"]*100)/fortify(model)[,1])),
           max(abs((fortify(model)[,".resid"]*100)/fortify(model)[,1]))) +
      theme_minimal()
    
    lstResidP[["Presid_obs"]] <- Presid_obs
    
    #stard resid
    Sresid_fit <- ggplot(model, aes(x = .fitted, y = .stdresid)) +
      geom_point() + 
      geom_pointdensity(adjust = 4, show.legend = FALSE) +
      geom_hline(yintercept = 0, size = 0.7) +
      geom_hline(yintercept = c(-2,2), size = 0.7, linetype = "dotdash") +
      labs(x = "fitted", y = "std resid") +
      ylim(-1*max(abs(fortify(model)[,".stdresid"])),
           max(abs(fortify(model)[,".stdresid"]))) +
      theme_minimal()
    
    lstResidP[["Sresid_fit"]] <- Sresid_fit
    
    #obs x fitted
    obs_fit <- ggplot(model, aes(x = .fitted, y = fortify(model)[,1])) +
      geom_point() + 
      geom_pointdensity(adjust = 4, show.legend = FALSE) +
      geom_abline(intercept = 0, slope = 1, size = 0.7) +
      labs(x = "fitted", y = "observed") +
      lims(x = c(min(fortify(model)[,1],fortify(model)[,".fitted"]),
                 max(fortify(model)[,1],fortify(model)[,".fitted"])),
           y = c(min(fortify(model)[,1],fortify(model)[,".fitted"]),
                 max(fortify(model)[,1],fortify(model)[,".fitted"]))) +
      theme_minimal()
    
    lstResidP[["obs_fit"]] <- obs_fit
    
  } else {
    #resid x fitted
    Presid_fit <- ggplot(model, aes(x = exp(.fitted), y = ((.resid*100)/fortify(model)[,1]))) +
      geom_point() + 
      geom_pointdensity(adjust = 4, show.legend = FALSE) +
      geom_hline(yintercept = 0, size = 0.7) +
      labs(x = "fitted", y = "resid%") +
      ylim(-1*max(abs((fortify(model)[,".resid"]*100)/fortify(model)[,1])),
           max(abs((fortify(model)[,".resid"]*100)/fortify(model)[,1])))+
      theme_minimal()
    lstResidP[["Presid_fit"]] <- Presid_fit
    
    #resid x observed
    Presid_obs <- ggplot(model, aes(x = exp(fortify(model)[,1]), y = ((.resid*100)/fortify(model)[,1]))) +
      geom_point() + 
      geom_pointdensity(adjust = 4, show.legend = FALSE) +
      geom_hline(yintercept = 0, size = 0.7) +
      labs(x = "observed", y = "resid%") +
      ylim(-1*max(abs((fortify(model)[,".resid"]*100)/fortify(model)[,1])),
           max(abs((fortify(model)[,".resid"]*100)/fortify(model)[,1]))) +
      theme_minimal()
    
    lstResidP[["Presid_obs"]] <- Presid_obs
    
    #stard resid
    Sresid_fit <- ggplot(model, aes(x = exp(.fitted), y = .stdresid)) +
      geom_point() + 
      geom_pointdensity(adjust = 4, show.legend = FALSE) +
      geom_hline(yintercept = 0, size = 0.7) +
      geom_hline(yintercept = c(-2,2), size = 0.7, linetype = "dotdash") +
      labs(x = "fitted", y = "std resid") +
      ylim(-1*max(abs(fortify(model)[,".stdresid"])),
           max(abs(fortify(model)[,".stdresid"]))) +
      theme_minimal()
    lstResidP[["Sresid_fit"]] <- Sresid_fit 
    
    #obs x fitted
    obs_fit <- ggplot(model, aes(x = exp(.fitted), y = exp(fortify(model)[,1]))) +
      geom_point() + 
      geom_pointdensity(adjust = 4, show.legend = FALSE) +
      geom_abline(intercept = 0, slope = 1, size = 0.7) +
      labs(x = "fitted", y = "observed") +
      lims(x = c(min(exp(fortify(model)[,1]),exp(fortify(model)[,".fitted"])),
                 max(exp(fortify(model)[,1]),exp(fortify(model)[,".fitted"]))),
           y = c(min(exp(fortify(model)[,1]),exp(fortify(model)[,".fitted"])),
                 max(exp(fortify(model)[,1]),exp(fortify(model)[,".fitted"])))) +
      theme_minimal()
    lstResidP[["obs_fit"]] <- obs_fit
    
  }
  
  Presume <- wrap_plots(lstResidP,ncol = ncol, nrow = nrow) +
    plot_annotation(subtitle = title,
                    caption = paste0("Formula: ",deparse(model$call[[2]])))
  lstResidP[["resume"]] <- Presume
  
  return(lstResidP)
}

residplotVI.lm <- function(model, data = NULL, ncol = NULL, nrow = NULL, title = NULL) {

  #create a df
  if( !(is.null(data)) ){
    if(nrow(data) == nrow(fortify(model)) ){
      dfplot <- cbind(fortify(model), data[,(names(data) %in% names(fortify(model))) == F])
    } else {
      dfplot <- fortify(model)
    }
  } else {
    dfplot <- fortify(model)
  }
  dfplot$.residP <- (dfplot$.resid*100)/ dfplot[[toString(model$call[[2]][2])]]
  
  #list variables
  #lstVI <- as.list(attr(model$term, "term.labels"))
  lstVI <- as.list(names(dfplot[,(!grepl("^\\.", names(dfplot))) &  
                                  (names(dfplot) != toString(model$call[[2]][2])) ]))
  
  #create residue of each gaph
  
  #plot_fun
  #funcao para plotagem baseada no fortify
  plot_data_column = function (dfcol, data) {
    ggplot(data,
           aes_string(x = dfcol, y = ".residP")) +
      geom_point(alpha =0.3) +
      geom_hline(yintercept = 0, size = 0.7) +
      labs(x = dfcol, y = "resid%") +
      ylim(-1*max(abs(data$.residP)),
           max(abs(data$.residP)))+
      theme_minimal()
  }
  
  #apply fun
  lstResidI <- lapply(lstVI, plot_data_column, data = dfplot)
  names(lstResidI) <- lstVI
  
  #resume plot
  resume <- wrap_plots(lstResidI, ncol = ncol, nrow = nrow) + 
    plot_annotation(subtitle = title,
                    caption = paste0("Formula: ",deparse(model$call[[2]])))
  lstResidI[["resume"]] <- resume
  
  return(lstResidI)
}

dispersionP.lm <- function(model, ncol = NULL, nrow = NULL, data = NULL, delog = F, title = NULL) {

  #create a dtplot
  if( !(is.null(data)) ){
    if( nrow(data) == nrow(fortify(model)) ){
      dfplot <- cbind(fortify(model), data[,(names(data) %in% names(fortify(model))) == F])
    } else {
      dfplot <- fortify(model)
    }
  } else {
    dfplot <- fortify(model)
  }
  dfplot$.residP <- (dfplot$.resid*100)/ dfplot[[toString(model$call[[2]][2])]]
  
  #list variables
  #lstVI <- as.list(attr(model$term, "term.labels"))
  lstVI <- as.list(names(dfplot[,(!grepl("^\\.", names(dfplot))) &  
                                  (names(dfplot) != toString(model$call[[2]][2])) ]))
  #create residue of each gaph
  
  #plot_fun
  plot_data_column2 = function (data, dfcol, delog, model) {
    cl <- c("observed" = "grey", "fitted" = "black")
    if(delog == T){
      ggplot(data = data) + 
        geom_point(aes(x = !!sym(dfcol), y = exp(fortify(model)[,1]), color = "observed")) +
        geom_point(aes(x = !!sym(dfcol), y = exp(.fitted), color = "fitted")) +
        labs(y = all.vars(model$call)[1], x = dfcol)+
        scale_color_manual(values = cl) +
        theme_minimal() %+replace% theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(3, "mm"))
    } else{
      ggplot(data = data) + 
        geom_point(aes(x = !!sym(dfcol), y = (fortify(model)[,1]), color = "observed")) +
        geom_point(aes(x = !!sym(dfcol), y = (.fitted), color = "fitted")) +
        labs(y = toString(model$call[[2]][2]), x = dfcol)+
        scale_color_manual(values = cl) +
        theme_minimal() %+replace% theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(3, "mm"))
    }
  }
  
  #apply fun
  lstdisp <- lapply(lstVI, plot_data_column2, data = dfplot, delog = delog, model = model)
  names(lstdisp) <- lstVI
  
  #resume plot
  resume <- wrap_plots(lstdisp, ncol = ncol, nrow = ncol) + 
    plot_annotation(subtitle = title,
                    caption = paste0("Formula: ",deparse(model$call[[2]])))
  lstdisp[["resume"]] <- resume
  
  return(lstdisp)
}

plot_exp <- function(data, x_var, y_var, title) {
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  
  ggplot(data = data, aes(x = !!x_var, y = !!y_var)) + 
    geom_point(alpha = 0.3) +
    labs(subtitle = title) +
    lims(x = c(0,NA), y = c(0,NA)) +
    theme_minimal()
}



