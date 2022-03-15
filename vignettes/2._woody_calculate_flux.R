## ----attach_woody, warning=FALSE, message=FALSE-------------------------------
knitr::opts_chunk$set(warning=FALSE,message=FALSE)
library(woody)
library(tidyverse)
library(DiagrammeR)

## ----diagramme0, echo=FALSE, fig.height=10------------------------------------
grViz("
digraph boxes_and_circles {
  graph [overlap = true, fontsize = 10]
  node [shape = box, style= filled]
  A [fillcolor=pink]
  Qc  [fillcolor = cadetblue1]
  W [fillcolor = burlywood1]
  Qh [fillcolor=cadetblue1]
  myrf[fillcolor=gold]
  Qp [fillcolor=cadetblue2]
  Qn [fillcolor=cadetblue3]
  Wn [fillcolor=burlywood2]
  N  [fillcolor=darkkhaki]
  
  node [shape = ellipse,fixedsize = false,color = grey, style= filled, fillcolor=grey90]
  'interp_Qdata()'
  'predict_rf()'
  'summarise_Qdata()'  
  'summarise_Wdata()'
  
  # several 'edge' statements
  Qc -> 'interp_Qdata()' [arrowhead = none]
  'interp_Qdata()'-> Qh [label= 'interpolate Qdata hourly']
  Qh, myrf-> 'predict_rf()'
  'predict_rf()' -> 'Qp' [label = ' add Y_pred values']
  Qp -> 'summarise_Qdata()' [arrowhead = none]
  'summarise_Qdata()' -> 'Qn' [label = ' add N_pred values']
  A,W -> 'summarise_Wdata()' [arrowhead = none]
  'summarise_Wdata()' -> 'Wn' [label = ' add N_obse values']
  Wn,Qn  -> 'N' 

}
")

## ----diagramme1, echo=FALSE, fig.height=7-------------------------------------
grViz("
digraph boxes_and_circles {
  graph [overlap = true,  labeljust=l]
  
  graph [fontsize=10 compound=true overlap=true];
    node [shape=egg fontsize=10, style=filled]
    // all nodes related to Q
    node [fillcolor=cadetblue1]
    'Qc8'  [label='Qc[1,2]'] Qh
    'Qh' [label='Qh[1,2]']
    node [fillcolor=cadetblue2]
    'Qp'  [label='Qp[1,2]'] 
    node [fillcolor=cadetblue3]
    'Qn'  [label='Qn[1,2]']
    'Qn12'  [label='Qn[1,2]']
    'Qn13'  [label='Qn[1,2]']
    // all nodes related to W
    node [fillcolor=burlywood1, label='W[1,2]']
    'W8' 'W9' 'W10' 'W11'
    // all nodes related to W+Qnode 
     node [label= 'W[1,2]', fillcolor = burlywood]
    'W8' 'W9' 'W10' 'W11'
    node [label= 'Wn[1,2]', fillcolor = burlywood2]
    'Wn' 'Wn13'
    node [label= 'N[1,2]', fillcolor = darkkhaki]
    'N'
    
    node[shape=box]
    myrf [fillcolor = gold, label='myrf']
    
    // all nodes related to descriptors
    node [shape=egg, fillcolor=ivory, label='descriptors[1,2]']
    'descriptors8'    'descriptors9'    'descriptors10' 'descriptors11' 'descriptors12' 'descriptors13'
    
    subgraph cluster_8 {'descriptors8' 'W8' 'Qc8' label='tib_WpQc'}
    subgraph cluster_9 {'descriptors9' 'W9' 'Qh'; label='tib_WpQh'}
    subgraph cluster_10 {'descriptors10' 'W10' 'Qp' ; label='tib_WpQp'}
    subgraph cluster_11 {'descriptors11' 'W11' 'Qn'; label='tib_WpQn'}
    subgraph cluster_12 {'descriptors12' 'Wn' 'Qn12'; label='tib_WnQn'}
    subgraph cluster_13 {'descriptors13' 'Wn13', 'Qn13' 'N'; label='tib_WnQnN'}
    
    edge[color=grey]
    'descriptors8' -> 'descriptors9' -> 'descriptors10' -> 'descriptors11' -> 'descriptors12' -> 'descriptors13'
    'W8' -> 'W9' -> 'W10' -> 'W11' -> 'Wn' -> 'Wn13'
    'descriptors11' -> 'Wn' [label='An[1,2]', fontsize=6]
    'Qc8' -> 'Qh' 
    'Qp' -> 'Qn' -> 'Qn12' -> 'Qn13'  
    'Qh','myrf' -> 'Qp'
    'Wn','Qn12' -> 'N'
  }
")

## ----plot_distrib, fig.width=6------------------------------------------------
tib_WpQc=readRDS("../data-raw/results/tib_WpQc.RDS")
Wdata_pred=tib_WpQc %>% 
  select(Wdata) %>% 
  tidyr::unnest(Wdata, .name_repair="minimal")

p=ggplot(Wdata_pred,aes(x=Y_pred,y=Y-Y_pred))+
  geom_point(alpha=0.05)+
  facet_grid(cols=vars(site),
             labeller = labeller(.rows = label_both,
                                 .cols = label_both))+
  geom_hline(aes(yintercept=0), col='red')
plot(p)

## ----est_sigma_as_f_mu--------------------------------------------------------
data_sigma=Wdata_pred %>% 
  ungroup() %>% 
  mutate(Ycat=cut(Y_pred,quantile(Y_pred,seq(0,1,by=0.1)))) %>% 
  group_by(Ycat) %>% 
  summarise(mu=mean(Y_pred),
            sigma=sd(Y_pred-Y)) 

ggplot(data_sigma, aes(x=mu,y=sigma))+
  geom_point()+
  geom_smooth(method="lm")

lm_sigma=lm(sigma~mu, data=data_sigma)
gamma=lm_sigma$coefficients[[2]]
delta=lm_sigma$coefficients[[1]]
sigma_coeffs=c(gamma,delta)

## ----tib_WQh------------------------------------------------------------------
myrf=readRDS("../data-raw/results/rf.RDS")
tib_WQc=readRDS("../data-raw/results/tib_WQc.RDS")
tib_WQh=tib_WQc %>%
  # interpolate at an hourly timescale
  mutate(Qdata=purrr::map(.x=Qdata,
                          ~interp_Qdata(.x, timescale="hours")))

## ----tib_WQp------------------------------------------------------------------
correction_covariate_shift=readRDS("../data-raw/correction_covariate_shift.RDS")


tib_WQp=tib_WQh %>%
  mutate(Qdata=purrr::map(.x=Qdata %>% na.omit(),
                          ~predict_rf(newdata=.x,obj_rf=myrf,
                                      correction=correction_covariate_shift)))


## ----tib_WQn------------------------------------------------------------------
tib_WQn=tib_WQp %>%
  mutate(apath=paste0("../data-raw/annot_times_",site,".csv")) %>%
  mutate(Qdata=purrr::map(.x=Qdata,
                          ~summarise_Qdata(Qdata=.x,sigma_coeffs=sigma_coeffs)))

## ----tib_WnQn-----------------------------------------------------------------
tib_WnQn=tib_WQn %>%
  mutate(Adata=purrr::map2(.x=apath,.y=site,
                           ~import_Adata(path=.x,site=.y))) %>%
  mutate(Wdata=purrr::map2(.x=Wdata,.y=Adata,
                           ~summarise_Wdata(Wdata=.x,Adata=.y)))

## ----show_Adata, echo=FALSE---------------------------------------------------
knitr::kable(head(tib_WnQn$Adata[[1]]))

## ----show_tib_WnQn_Wdata, echo=FALSE------------------------------------------
knitr::kable(head(tib_WnQn$Wdata[[1]]))

## ----tib_WnQnN----------------------------------------------------------------
tib_WnQnN=tib_WnQn %>% 
  mutate(Ndata=purrr::map2(.x=Qdata,.y=Wdata,
                           ~left_join(.x,.y,by=c("site","Time"))))

## ----unnest_Ndata-------------------------------------------------------------
Ndata=tib_WnQnN %>% 
  ungroup() %>% 
  select(Ndata) %>% 
  tidyr::unnest(Ndata)

## ----R2-----------------------------------------------------------------------
R2=tib_WnQnN %>% select(site,Ndata) %>% 
  mutate(R2=purrr::map_df(Ndata,calc_rf_R2,type="N")) %>% 
  tidyr::unnest(R2)
R2

## ----plot_N_log, fig.height=16, fig.width=8-----------------------------------
Ndataplot=Ndata %>% 
  tidyr::pivot_longer(cols=c(N_pred,N_obse), names_to="type") %>%
  filter(!is.na(event))
ggplot(Ndataplot, aes(x=Time,y=value))+
  geom_path(aes(col=type))+
  geom_point(aes(col=type))+
  facet_wrap(site~event, scales="free_x", ncol=1)+
  scale_y_log10()

## ----plot_N_nat, fig.height=16, fig.width=8-----------------------------------
Ndataplot=Ndata %>% 
  tidyr::pivot_longer(cols=c(N_pred,N_obse), names_to="type") %>%
  filter(!is.na(event))
ggplot(Ndataplot, aes(x=Time,y=value))+
  geom_path(aes(col=type))+
  geom_point(aes(col=type))+
  facet_wrap(site~event, scales="free_x", ncol=1)

## ----plot_predicted_observed--------------------------------------------------
ggplot(Ndata %>% filter(!is.na(event)), aes(x=N_obse,y=N_pred, col=event))+
  geom_point()+
  geom_abline(vintercept=0,slope=1,col="red")+
  facet_wrap(~site)+
  scale_x_log10()+scale_y_log10()

