# Overview
Function to plot model statistics for exploratory fit analysis

## Dependencias

```R
library(ggplot2)
library(patchwork)
library(ggpointdensity)
```

## Functions

As funçoes presente nessa rotina são descritas abaixo

```R
normplot (model, ncol = NULL, nrow = NULL, title = NULL)
residplot (model, delog = F, ncol = NULL, nrow = NULL, title = NULL)
residplotVI (model, data = NULL, ncol = NULL, nrow = NULL, title = NULL)
dispersionP.lm (model, , data = NULL, delog = F, ncol = NULL, nrow = NULL, title = NULL)
plot_exp (data, x_var, y_var, title)
```
### normplot
