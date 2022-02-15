### Best cline model selection ###

### this script is edited based on an example script “men12209-sup-0004-hzarExampleScript.R” of Hzar paper “hzar: hybrid zone analysis using an R software package” 
### (https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F1755-0998.12209&file=men12209-sup-0004-hzarExampleScript.R).

## Load the package
library(hzar);
library(doMC);
library(png);

## Save all plots in a series of png files
# png(width=900, height=900, res=200, family="Arial", filename="mExPlot%03d.png",pointsize=8)

## A typical chain length.  This value is the default setting in the package.
chainLength=1e5;
# chainLength=5e4;                       

## Make each model run off a separate seed
mainSeed=
  list(A=c(596,528,124,978,544,99),
       B=c(528,124,978,544,99,596),
       C=c(124,978,544,99,596,528),
       D=c(596,528,124,978,544,99),
       E=c(528,124,978,544,99,596),
       F=c(124,978,544,99,596,528))


if(require(doMC)){
  ## If you have doMC, use foreach in parallel mode
  ## to speed up computation.
  registerDoMC()
} else {
  ## Use foreach in sequential mode
  registerDoSEQ();
}

## Blank out space in memory to hold molecular analysis
if(length(apropos("^mkn$",ignore.case=FALSE)) == 0 ||
   !is.list(mkn) ) mkn <- list()
## We are doing just the one allele at one locus, but it is
## good to stay organized.
mkn$AdaA <- list();
## Space to hold the observed data
mkn$AdaA$obs <- list();
## Space to hold the models to fit
mkn$AdaA$models <- list();
## Space to hold the compiled fit requests
mkn$AdaA$fitRs <- list();
## Space to hold the output data chains
mkn$AdaA$runs <- list();
## Space to hold the analysed data
mkn$AdaA$analysis <- list();


## Locus Ada, Allele A from Brumfield et al 2001
# mkn$AdaA$obs <-
  # hzar.doMolecularData1DPops(manakinMolecular$distance,
                             # manakinMolecular$ada.A,
                             # manakinMolecular$ada.nSamples);


transect_data_F <- read.csv("03_Research_citizen_combine_17094_rmEnvValue.frequency_processed.processed.transect2_GB.csv", as.is=T);

mkn$AdaA$obs <- hzar.doMolecularData1DPops(transect_data_F$transect_pos,
                             transect_data_F$groundColour_B,
                             transect_data_F$groundColour_n);

## Look at a graph of the observed data
hzar.plot.obsData(mkn$AdaA$obs);

#### Show some plot modification commands for use by new users


## Make a helper function
mkn.loadAdaAmodel <- function(scaling,tails,
                              id=paste(scaling,tails,sep="."))
  mkn$AdaA$models[[id]] <<- hzar.makeCline1DFreq(mkn$AdaA$obs, scaling, tails)
  
mkn.loadAdaAmodel("none","none","model01");
mkn.loadAdaAmodel("none" ,"both","model02");
mkn.loadAdaAmodel("free","none","model03");
mkn.loadAdaAmodel("free" ,"both","model04");
mkn.loadAdaAmodel("fixed","none","model05");
mkn.loadAdaAmodel("fixed" ,"both","model06");

## Check the default settings
print(mkn$AdaA$models)

## Modify all models to focus on the region where the observed
## data were collected.
## Observations were between 0 and 570 km.
mkn$AdaA$models <- sapply(mkn$AdaA$models,
                          hzar.model.addBoxReq,
                          0, 4761,
                          simplify=FALSE)


## Check the updated settings
print(mkn$AdaA$models)

## Compile each of the models to prepare for fitting
mkn$AdaA$fitRs$init <- sapply(mkn$AdaA$models,
                         hzar.first.fitRequest.old.ML,
                         obsData=mkn$AdaA$obs,
                         verbose=FALSE,
                         simplify=FALSE)

## Update the settings for the fitter if desired.
mkn$AdaA$fitRs$init$model01$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model01$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model01$mcmcParam$seed[[1]] <-
  mainSeed$A

mkn$AdaA$fitRs$init$model02$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model02$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model02$mcmcParam$seed[[1]] <-
  mainSeed$B 

mkn$AdaA$fitRs$init$model03$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model03$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model03$mcmcParam$seed[[1]] <-
  mainSeed$C 
  
mkn$AdaA$fitRs$init$model04$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model04$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model04$mcmcParam$seed[[1]] <-
  mainSeed$D

mkn$AdaA$fitRs$init$model05$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model05$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model05$mcmcParam$seed[[1]] <-
  mainSeed$E 

mkn$AdaA$fitRs$init$model06$mcmcParam$chainLength <-
  chainLength;                          #1e5
mkn$AdaA$fitRs$init$model06$mcmcParam$burnin <-
  chainLength %/% 10;                   #1e4
mkn$AdaA$fitRs$init$model06$mcmcParam$seed[[1]] <-
  mainSeed$F   
  


## Check fit request settings
print(mkn$AdaA$fitRs$init)







## Run just one of the models for an initial chain
mkn$AdaA$runs$init <- list()

mkn$AdaA$runs$init$model01 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model01)
## Plot the trace
plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model01))

## Run another model for an initial chain
mkn$AdaA$runs$init$model02 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model02)
## Plot the trace
plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model02))

## Run another model for an initial chain
mkn$AdaA$runs$init$model03 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model03)
## Plot the trace
plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model03))

## Run just one of the models for an initial chain
mkn$AdaA$runs$init$model04 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model04)
## Plot the trace
plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model04))

## Run another model for an initial chain
mkn$AdaA$runs$init$model05 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model05)
## Plot the trace
plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model05))

## Run another model for an initial chain
mkn$AdaA$runs$init$model06 <-
  hzar.doFit(mkn$AdaA$fitRs$init$model06)
## Plot the trace
plot(hzar.mcmc.bindLL(mkn$AdaA$runs$init$model06))

# The Metropolis acceptance rate
# Recent optimal scaling theory has produced a condition for the asymptotically optimal acceptance rate of Metropolis algorithms to be the well-known 0.234 when applied to certain multi-dimensional target distributions.

# The Metropolis acceptance rate of ModelXX and ModelXX > 0.234











## Compile a new set of fit requests using the initial chains 
mkn$AdaA$fitRs$chains <-
  lapply(mkn$AdaA$runs$init,
         hzar.next.fitRequest)

## Replicate each fit request 3 times, keeping the original
## seeds while switching to a new seed channel.
mkn$AdaA$fitRs$chains <-
  hzar.multiFitRequest(mkn$AdaA$fitRs$chains,
                       each=3,
                       baseSeed=NULL)


## Just to be thorough, randomize the initial value for each fit

## runif(45,0,4761) center for 15 models
###runif(n, a, b) generates n uniform random numbers between a and b.

# > runif(18,0,4761)
 # [1] 4321.0792  660.5222  855.9821 4740.9078 3509.5428  337.7526 1608.5543
 # [8] 4128.5353 3867.6668 1692.2470  218.9130 2804.1826 4425.7489 2120.3820
# [15] 2605.2191 3942.9046  163.3335 1477.0913


mkn$AdaA$fitRs$chains[[1]]$modelParam$init["center"]= 4321.0792
mkn$AdaA$fitRs$chains[[2]]$modelParam$init["center"]= 660.5222
mkn$AdaA$fitRs$chains[[3]]$modelParam$init["center"]= 855.9821
mkn$AdaA$fitRs$chains[[4]]$modelParam$init["center"]= 4740.9078
mkn$AdaA$fitRs$chains[[5]]$modelParam$init["center"]= 3509.5428
mkn$AdaA$fitRs$chains[[6]]$modelParam$init["center"]= 337.7526
mkn$AdaA$fitRs$chains[[7]]$modelParam$init["center"]= 1608.5543
mkn$AdaA$fitRs$chains[[8]]$modelParam$init["center"]= 4128.5353
mkn$AdaA$fitRs$chains[[9]]$modelParam$init["center"]= 3867.6668
mkn$AdaA$fitRs$chains[[10]]$modelParam$init["center"]= 1692.2470
mkn$AdaA$fitRs$chains[[11]]$modelParam$init["center"]= 218.9130
mkn$AdaA$fitRs$chains[[12]]$modelParam$init["center"]= 2804.1826
mkn$AdaA$fitRs$chains[[13]]$modelParam$init["center"]= 4425.7489
mkn$AdaA$fitRs$chains[[14]]$modelParam$init["center"]= 2120.3820
mkn$AdaA$fitRs$chains[[15]]$modelParam$init["center"]= 2605.2191
mkn$AdaA$fitRs$chains[[16]]$modelParam$init["center"]= 3942.9046
mkn$AdaA$fitRs$chains[[17]]$modelParam$init["center"]= 163.3335
mkn$AdaA$fitRs$chains[[18]]$modelParam$init["center"]= 1477.0913



# > runif(18,0,4761)
 # [1] 2065.11330 2053.21684 2685.68547 2669.97817 1293.35245 2005.43569
 # [7] 3663.01978 2313.32831 1559.05990  497.22087 3901.50765 2757.94308
# [13] 4620.24683   74.55348 2386.26991  161.75335 3532.25941 2458.46647

mkn$AdaA$fitRs$chains[[1]]$modelParam$init["width"]= 2065.11330
mkn$AdaA$fitRs$chains[[2]]$modelParam$init["width"]= 2053.21684
mkn$AdaA$fitRs$chains[[3]]$modelParam$init["width"]= 2685.68547
mkn$AdaA$fitRs$chains[[4]]$modelParam$init["width"]= 2669.97817
mkn$AdaA$fitRs$chains[[5]]$modelParam$init["width"]= 1293.35245
mkn$AdaA$fitRs$chains[[6]]$modelParam$init["width"]= 2005.43569
mkn$AdaA$fitRs$chains[[7]]$modelParam$init["width"]= 3663.01978
mkn$AdaA$fitRs$chains[[8]]$modelParam$init["width"]= 2313.32831
mkn$AdaA$fitRs$chains[[9]]$modelParam$init["width"]= 1559.05990
mkn$AdaA$fitRs$chains[[10]]$modelParam$init["width"]= 497.22087
mkn$AdaA$fitRs$chains[[11]]$modelParam$init["width"]= 3901.50765
mkn$AdaA$fitRs$chains[[12]]$modelParam$init["width"]= 2757.94308
mkn$AdaA$fitRs$chains[[13]]$modelParam$init["width"]= 4620.24683
mkn$AdaA$fitRs$chains[[14]]$modelParam$init["width"]= 74.55348
mkn$AdaA$fitRs$chains[[15]]$modelParam$init["width"]= 2386.26991
mkn$AdaA$fitRs$chains[[16]]$modelParam$init["width"]= 161.75335
mkn$AdaA$fitRs$chains[[17]]$modelParam$init["width"]= 3532.25941
mkn$AdaA$fitRs$chains[[18]]$modelParam$init["width"]= 2458.46647



## Go ahead and run a chain of 3 runs for every fit request
mkn$AdaA$runs$chains <-  hzar.doChain.multi(mkn$AdaA$fitRs$chains,
                                            doPar=TRUE,
                                            inOrder=FALSE,
                                            count=3)

## Did modelX converge?
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[1:3],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[4:6],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[7:9],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[10:12],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[13:15],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )
summary(do.call(mcmc.list,
                lapply(mkn$AdaA$runs$chains[16:18],
                       function(x) hzar.mcmc.bindLL(x[[3]]) )) )



## Start aggregation of data for analysis

## Create a model data group for the null model (expected allele
## frequency independent of distance along cline) to include in
## analysis.
mkn$AdaA$analysis$initDGs <- list(
  nullModel =  hzar.dataGroup.null(mkn$AdaA$obs))

## Create a model data group (hzar.dataGroup object) for each
## model from the initial runs.
mkn$AdaA$analysis$initDGs$model01 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model01)
mkn$AdaA$analysis$initDGs$model02 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model02)
mkn$AdaA$analysis$initDGs$model03 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model03)
mkn$AdaA$analysis$initDGs$model04 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model04)
mkn$AdaA$analysis$initDGs$model05 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model05)
mkn$AdaA$analysis$initDGs$model06 <-
  hzar.dataGroup.add(mkn$AdaA$runs$init$model06)


## Create a hzar.obsDataGroup object from the four hzar.dataGroup
## just created, copying the naming scheme (nullModel, modelI,
## modelII, modelIII).
mkn$AdaA$analysis$oDG <-
  hzar.make.obsDataGroup(mkn$AdaA$analysis$initDGs)
mkn$AdaA$analysis$oDG <-
    hzar.copyModelLabels(mkn$AdaA$analysis$initDGs,
                         mkn$AdaA$analysis$oDG)

## Convert all 27 runs to hzar.dataGroup objects, adding them to
## the hzar.obsDataGroup object.
mkn$AdaA$analysis$oDG <-
  hzar.make.obsDataGroup(lapply(mkn$AdaA$runs$chains,
                                hzar.dataGroup.add),
                         mkn$AdaA$analysis$oDG);


## Check to make sure that there are only four hzar.dataGroup
## objects named nullModel, modelI, modelII, and modelIII in the
## hzar.obsDataGroup object.
print(summary(mkn$AdaA$analysis$oDG$data.groups))

## Compare the 3 cline models to the null model graphically
hzar.plot.cline(mkn$AdaA$analysis$oDG);

## Do model selection based on the AICc scores
print(mkn$AdaA$analysis$AICcTable <-
      hzar.AICc.hzar.obsDataGroup(mkn$AdaA$analysis$oDG));






## Print out the model with the minimum AICc score
print(mkn$AdaA$analysis$model.name <-
  rownames(mkn$AdaA$analysis$AICcTable
           )[[ which.min(mkn$AdaA$analysis$AICcTable$AICc )]])





## Extract the hzar.dataGroup object for the selected model
mkn$AdaA$analysis$model.selected <-
  mkn$AdaA$analysis$oDG$data.groups[[mkn$AdaA$analysis$model.name]]


## Look at the variation in parameters for the selected model
print(hzar.getLLCutParam(mkn$AdaA$analysis$model.selected,
                         names(mkn$AdaA$analysis$model.selected$data.param)));

## Print the maximum likelihood cline for the selected model
print(hzar.get.ML.cline(mkn$AdaA$analysis$model.selected))

## Plot the maximum likelihood cline for the selected model
hzar.plot.cline(mkn$AdaA$analysis$model.selected);

## Plot the 95% credible cline region for the selected model
# hzar.plot.fzCline(mkn$AdaA$analysis$model.selected);
hzar.plot.fzCline(mkn$AdaA$analysis$model.selected, pch=NA);
hzar.plot.obsData(mkn$AdaA$analysis$model.selected, pch=19, cex=0.1 + log10(mkn$AdaA$analysis$model.selected$obsData$frame$n), add=T);
abline(v=hzar.getLLCutParam(mkn$AdaA$analysis$model.selected,"center"));

## Plot the 95% credible cline region for the selected model
hzar.plot.fzCline(mkn$AdaA$analysis$model.selected);
abline(v=hzar.getLLCutParam(mkn$AdaA$analysis$model.selected,"center"));

## End Molecular Analysis

# dev.off()








