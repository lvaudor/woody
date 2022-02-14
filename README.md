# woody

This package imports **wood log data** and all related **discharge data** to run a **random forest** regression that to predicts **wood flux** according to some **discharge history** variables. 

Install these packages through:

```{r}
install.packages("remotes") #if remotes is not already installed
remotes::install_github("lvaudor/banqueHydro")
remotes::install_github("lvaudor/woody", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

To learn more about this package please consult

(http://perso.ens-lyon.fr/lise.vaudor/Rpackages/woody)
