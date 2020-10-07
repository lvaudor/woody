# woody

This package imports **wood log data** and all related **discharge data** to run a **random forest** regression that to predicts **wood flux** according to some **discharge history** variables. 

You will have to install package `banqueHydro`, too, to be able to use it.

Install these packages through:

```{r}
install.packages("remotes") #if remotes is not already installed
remotes::install_github("lvaudor/banqueHydro")
remotes::install_github("lvaudor/woody", build_vignettes=TRUE)
```

To learn more about this package please consult vignette running:

```{r}
vignette(package="woody")
```


