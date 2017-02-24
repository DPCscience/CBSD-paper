library(foreach)
library(doParallel)
deregress<-function(model, trait){
  BLUP <- ranef(model, condVar=TRUE)$clone
  PEV <- c(attr(BLUP, "postVar"))
  clone.var <- c(VarCorr(model)$clone)#random effects
  block = c(VarCorr(model)$block)#random effects
  env.block = c(VarCorr(model)$env.block)#random effects
  out <- BLUP/(1-(PEV/clone.var))#deregressed BLUPs
  r2 <- 1-(PEV/clone.var)
  ResidVar = (attr(VarCorr(model),"sc"))^2
  h2 = clone.var/(clone.var + block  +env.block+ ResidVar)#change according to the random effects
  wt = (1-h2)/((0.1 + (1-r2)/r2)*h2)
  MeanPlots<-mean(table(model@frame$clone))
  MedianPlots<-median(table(model@frame$clone))
  VarComps<-as.data.frame(VarCorr(model))
  H2.plot = clone.var/(clone.var + block + (ResidVar/MedianPlots))
  return(list(Trait=trait, drgBLUP=out, BLUP=BLUP, weights=wt, varcomps=VarComps,MeanPlotNum=MeanPlots,MedianPlotNum=MedianPlots,H2=h2,H2.plot=H2.plot))
}