## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----tree_mypath--------------------------------------------------------------
mypath="../../woody_data/woody_data_train/"
mystation="V2942010"
fs::dir_tree(mypath)

## ----attach_woody-------------------------------------------------------------
library(woody)

## ----import_Wdata, message=FALSE, warning=FALSE-------------------------------
Wdat=import_Wdata(mypath) 

