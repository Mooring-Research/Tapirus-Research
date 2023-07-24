# Recent Models folder
## Unicovariate modeling 

For each species, an unmarkedFitOccu (unmarked package) frame was created with detection, covariate, and effort tables for each species of Tapir for every covariate available for that tapir.
Some have more covariates than others, but the common ones include distance to road, NDVI, avgMaxTemp, avgMinTemp, Precipitation, and elevation. 

The outputs for these models are found in the unicovariateModelSummaries folder. 

# Multicovariate Modeling
All combinations of covariates were run against each other, up to 3 covariates together. These combinations were found using 2 and 3 dimentional matrices which were then 
printed to a file (findingModelCombos folder).

The output of these models is found in the multicovariateModelSummaries folder. These text files include pearson's correlation matrices as well to see what models have the 
least corelation between each covariate in the multivariate model. 

# OldModelScripts
Models from previous work are found in this folder. These were built off of and cleaned for use in the uni and multi-covariate model scripts. 