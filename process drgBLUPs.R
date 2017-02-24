# Deregress all: ------------------------------
traits<-c("a","b","c")#as many traits as you may have

DRG1 <- foreach(a=traits, i=icount(), .inorder=TRUE) %dopar% { #it can be parallelized
  require(lme4)
  if(i <= 4)
  { model<-lmer(data=pheno,formula = get(traits[i]) ~ checks + (1|clone) ) }#changes depending on the fixed and random effects
  drg<-deregress(model,traits[i])#model is the lmer object
}

# Create dataframe of deregressed EBVs, EBVs and weights ------------------------
drgphenos<-data.frame(CLONE = unique(pheno$clone),stringsAsFactors=F)# pheno=name of your original pheno file, this is the initial empty dataframe

for(i in 1:length(DRG1)){
  drg<-data.frame(CLONE = rownames(DRG1[[i]]$drgBLUP),stringsAsFactors=F)
  drg[,DRG1[[i]]$Trait]<-DRG1[[i]]$drgBLUP
  drg[,paste(DRG1[[i]]$Trait,"ebv",sep=".")]<-DRG1[[i]]$BLUP
  drg[,paste(DRG1[[i]]$Trait,"wt",sep=".")]<-DRG1[[i]]$weights
  drgphenos<-merge(drgphenos,drg,by="CLONE",all=T)
}
rownames(drgphenos)<-drgphenos$CLONE
