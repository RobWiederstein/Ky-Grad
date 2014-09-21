library (amelia)
data(freetrade)
summary (freetrade)

#most packages delete observations with missing data
summary(lm(tariff ~ polity + pop + gdp.pc + year + country, data = freetrade))
#Note that 60 of the 171 original observations are deleted due to missingness.

#The first step is to identify the variables to include in the imputation model.
#Any variable that will be in the analysis model should also be in the imputation model.

a.out <- amelia(freetrade, m = 5, ts = "year", cs = "country")
plot (a.out)

#Each of the imputed datasets is now in the list a.out$imputations.
#the ith imputed datasets can be retrieved from this list as a.out$imputations[[i]].
hist(a.out$imputations[[3]]$tariff, col="grey", border="white")

#helpful graph of missing values
missmap(a.out)

#Example of passing data imputation sets to other packages
require(Zelig)
z.out <- zelig(tariff ~ polity + pop + gdp.pc + year +country, data = freetrade,
               model = "ls")
summary(z.out)  #60 observations deleted due to missingness
missmap (freetrade)

#Now run process on data imputations from free trade
z.out.imp <- zelig(tariff ~ polity + pop + gdp.pc + year +country, 
                   data = a.out$imputations, model = "ls")
                 

