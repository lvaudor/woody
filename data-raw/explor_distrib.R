mu=5
sigma=0.2
# Y=log(N)=log(3600/W)
# Y suit loi Norm(mu,sigma)
# donc N suit loi d'espérance exp(mu+sigma²/2)
tib=tibble(Y=rnorm(100000,mu,sigma)) %>%
  mutate(N=exp(Y),
         W=3600/N)
exp(mu+(sigma^2)/2)
mean(tib$N)

espN=3600/exp(log(3600)-mu+(sigma^2)/2)
espNb=exp(mu+(sigma^2)/2)
espN
espNb
