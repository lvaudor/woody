# woody

This package imports **wood log data** and all related **discharge data** to run a **random forest** regression that to predicts **wood flux** according to some **discharge history** variables. 

You will have to install package `banqueHydro`, too, to be able to use it.

Install these packages through:

```{r}
install.packages("remotes") #if remotes is not already installed
remotes::install_github("lvaudor/banqueHydro")
remotes::install_github("lvaudor/woody", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

To learn more about this package please consult

(https://github.com/lvaudor/woody/blob/master/vignettes/woody.Rmd)

and this graphical summary:

1. Modelling log-flux of wood:

![](https://github.com/lvaudor/woody/blob/master/docs/diag_1.png?raw=true)


2. Calculating hourly flux of wood (from instantaneous log-flux)
![](https://github.com/lvaudor/woody/blob/master/docs/diag_2.png?raw=true)


