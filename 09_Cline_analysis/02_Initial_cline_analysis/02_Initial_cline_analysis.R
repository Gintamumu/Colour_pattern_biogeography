# cd /data/martin/genomics/analyses/Danaus_SDM/07_cline/
# sconda maxent_env
# R


library(hzar);

# transect_data <- read.csv("GBIF_Dchrysippus_20201206_20210721_DAS_records_Auguest2021_curated.processed.4DegreeBlocks.transect1.csv", as.is=T);

######Generate a hzar.obsData object using summary data about each locality#######
# fwdata <- hzar.doMolecularData1DPops(transect_data$position,
                             # transect_data$ratio_fw,
                             # transect_data$total_fw)


########Generate a plot of the observed data points.#########
# hzar.plot.obsData(fwdata)

######Make a cline model with the requested attributes######
# fwmodel <- hzar.makeCline1DFreq(fwdata, scaling="fixed",tails="none")

########Add parameter restriction clauses to cline model
# fwmodel <- hzar.model.addBoxReq(fwmodel,-100,7000)

##############Generate a ML based hzar.fitRequest using a meta model structure.##########
# fwmodelFitR <- hzar.first.fitRequest.old.ML(model=fwmodel, fwdata, verbose=FALSE)

# fwmodelFitR$mcmcParam$chainLength <- 5e4

# fwmodelFitR$mcmcParam$burnin <- 1e3

####Run the optimizer.####
# fwmodelFit <- hzar.doFit(fwmodelFitR)

##############Generate a mcmc object with sampled parameters and log likelihoods.##############
# plot(hzar.mcmc.bindLL(fwmodelFit))

######Prepare optimizer output for analysis.#######
# fwmodelData <- hzar.dataGroup.add(fwmodelFit)

## Not run: 
## mknAdaAmodelData <-
 ## hzar.dataGroup.add(
   ## mknAdaAmodelData,
   ## hzar.chain.doSeq(hzar.next.fitRequest(mknAdaAmodelFit)));

###########Generate a plot of the cline#############
# hzar.plot.cline(fwmodelData)

############Plot the 95% credible cline region for the given locus model.############
# hzar.plot.fzCline(fwmodelData);

#############Get the region of parameter space close to the maximum likelihood##############
# print(hzar.getLLCutParam(fwmodelData,c("center","width")))

# hzar.plot.fzCline(gwmodelData, pch=NA);
# hzar.plot.obsData(gwmodelData, pch=19, cex=0.1 + log10(gwmodelData$obsData$frame$n), add=T);
# abline(v=hzar.getLLCutParam(gwmodelData,"center"));

# png("hindwingWhite_transect1Central Africa.png", width = 2000, height=1000, res=300, bg=NA);
# par(mar=c(4,4,1,1));

# hzar.plot.fzCline(gwmodelData, pch=NA);
# hzar.plot.obsData(gwmodelData, pch=19, cex=0.1 + log10(gwmodelData$obsData$frame$n), add=T);
# abline(v=hzar.getLLCutParam(gwmodelData,"center"));

# dev.off();

# Abline
# The R function abline() can be used to add vertical, horizontal or regression lines to a graph. A simplified format of the abline() function is : abline(a=NULL, b=NULL, h=NULL, v=NULL, ...) a, b : single values specifying the intercept and the slope of the line. h : the y-value(s) for horizontal line(s)

#######################################################################################












### hindwingWhite transect1_2_1

transect_data <- read.csv("GBIF_Dchrysippus_20201206_20210721_DAS_records_Auguest2021_curated.processed.4DegreeBlocks.transect1_2_1c.csv", as.is=T);

hwdata <- hzar.doMolecularData1DPops(transect_data$transect_pos,
                             transect_data$hindwingWhite_mean,
                             transect_data$hindwingWhite_n);

# hwdata <- hzar.doNormalData1DRaw(transect_data$transect_pos,
                             # transect_data$hindwingWhite_mean,
                             # transect_data$hindwingWhite_n)
# Error in data.frame(site = siteID, dist = as.numeric(distance), mu = as.numeric(muObs),  :
  # arguments imply differing number of rows: 0, 17



# hzar.doNormalData1DPops(distance, muObs, varObs, nEff,
# siteID=paste("P",1:length(distance),sep=""),
# ylim=NULL)

hzar.plot.obsData(hwdata);

gwmodel <- hzar.makeCline1DFreq(hwdata, scaling="fixed",tails="none");

gwmodel <- hzar.model.addBoxReq(gwmodel,-100,7000);

# gwmodel <- hzar.model.addBoxReq(gwmodel,-2000,7000);

gwmodelFitR <- hzar.first.fitRequest.old.ML(model=gwmodel, hwdata, verbose=FALSE);

gwmodelFitR$mcmcParam$chainLength <- 5e4;

gwmodelFitR$mcmcParam$burnin <- 1e3;

gwmodelFit <- hzar.doFit(gwmodelFitR);

plot(hzar.mcmc.bindLL(gwmodelFit));

gwmodelData <- hzar.dataGroup.add(gwmodelFit);

print(hzar.getLLCutParam(gwmodelData,c("center","width")));

# print(hzar.getLLCutParam(gwmodelData,c("center","width", "slope")));

#
hzar.plot.fzCline(gwmodelData, pch=NA);
hzar.plot.obsData(gwmodelData, pch=19, cex=0.1 + log10(gwmodelData$obsData$frame$n), add=T);
abline(v=hzar.getLLCutParam(gwmodelData,"center"));
# abline(v=hzar.getLLCutParam(gwmodelData,"center") + hzar.getLLCutParam(gwmodelData,"width"), col = "red");

png("hindwingWhite_transect1CentralAfrica_2_1c.png", width = 2000, height=1000, res=300, bg=NA);
par(mar=c(4,4,1,1));

hzar.plot.fzCline(gwmodelData, pch=NA);
hzar.plot.obsData(gwmodelData, pch=19, cex=0.1 + log10(gwmodelData$obsData$frame$n), add=T);
abline(v=hzar.getLLCutParam(gwmodelData,"center"));
# abline(v=hzar.getLLCutParam(gwmodelData,"center") + hzar.getLLCutParam(gwmodelData,"width"), col = "red");

dev.off();









