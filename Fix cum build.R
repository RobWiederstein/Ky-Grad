wd <- getwd()
saar <- read.csv (paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/"), 
                  sep = ",", header = T, as.is = TRUE)
gr   <- read.csv (paste (wd, "objects", "gr.2002.2006.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)
afgr <- read.csv (paste (wd, "objects", "afgr.07.11.csv", sep = "/"),
                  sep = ",", header = T, as.is = TRUE)
agrc <- read.csv (paste (wd, "objects", "agrc.12.csv", sep = "/"),
                  sep = ",", header = T, as.is = TRUE)
elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)
frd  <- read.csv (paste (wd, "objects", "FRD.Lunch.Pct.2012.KDE.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)
sdcp <- read.csv (paste (wd, "objects", "Child.Poverty.by.School.District.Census.2003.2013.csv", sep = "/"),
                  sep = ",", header = T, as.is = TRUE)

#really small schools with 8 grade enrollment, but no 12th grade
#probably why they are missing on the afgr data
attach (saar)
exclude.schools <- saar [DISTRICT == "Anchorage Independent" | 
                           DISTRICT == "East Bernstadt Independent" |
                           DISTRICT == "Science Hill Independent"  |
                           DISTRICT == "Southgate Independent" |
                           DISTRICT == "West Point Independent",]
detach (saar)
exclude.school.eighth.grade.2013 <- sum (exclude.schools$GR8E.2013.KDE)/
  sum (saar$GR8E.2013.KDE, na.rm = T)
rm (exclude.schools)
#fix new index so lengths of data frames are equal
index <- as.data.frame (afgr$DISTRICT, stringsAsFactors = FALSE)
names (index) <- "DISTRICT"
index <- as.data.frame (index[-grep ("Monticello", index$DISTRICT), ]) #Monticello to merge with Wayne
names (index) <- "DISTRICT"
elsi <- merge (index, elsi)
saar <- merge (index, saar)
gr   <- merge (index, gr)
afgr <- merge (index, afgr)
agrc <- merge (index, agrc)
frd  <- merge (index, frd)
sdcp <- merge (index, sdcp)

#2013--Need the agrc data for school year 2013-2014.
#to be released in Sept, 2014.
#District <- index
#Year <- rep (2013, 168)
#Tot.Enrollment.saar <- saar$TOTAL.2013.KDE
#Tot.Enrollment.elsi <- rep (NA, 168)
#FRD.Lunch.Pct <- frd$FRD_LUNCH_PCT                   #taken from 2012-2013 KDE school report card
#FRD.Decile <- cut_number(FRD.Lunch.Pct, n = 10)
#levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
#Gr.8.Cohort.KDE <- agrc$Grad.w.Diploma.in.4.years.2012.KDE /
#  saar$GR8E.2007.KDE
#Gr.9.Cohort.KDE <- agrc$Grad.w.Diploma.in.4.years.2012.KDE /
#  saar$GR9E.2008.KDE
#Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2012.KDE / 
#  saar$GR8E.2007.KDE
#Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2012.KDE /
#  saar$GR9E.2008.KDE
#tot.2012 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
#                   FRD.Lunch.Pct, FRD.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, 
#                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#add census data
#School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2012
#School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
#levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
#tot.2012 <- cbind (tot.2012, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
##end census data

#rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
#     FRD.Lunch.Pct, FRD.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
#     Gr.12.Gr.9.ratio.KDE)

#2012
District               <- index
Year                   <- rep (2012, 168)
Total.KDE              <- saar$TOTAL.2012.KDE
Gr08E.KDE              <- saar$GR8E.2012.KDE
Gr09E.KDE              <- saar$GR9E.2012.KDE 
Gr12E.KDE              <- saar$GR12E.2012.KDE
Diplo.KDE              <- agrc$Grad.w.Diploma.in.4.years.2012.KDE
FRDL.Pct               <- frd$FRD_LUNCH_PCT                   #taken from 2012-2013 KDE school report card
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2012
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- agrc$Grad.w.Diploma.in.4.years.2012.KDE / saar$GR8E.2007.KDE
Gr.9.Cohort.KDE        <- agrc$Grad.w.Diploma.in.4.years.2012.KDE / saar$GR9E.2008.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2012.KDE / saar$GR8E.2007.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2012.KDE / saar$GR9E.2008.KDE

tot.2012 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2011
District               <- index
Year                   <- rep (2011, 168)
Total.KDE              <- saar$TOTAL.2011.KDE
Gr08E.KDE              <- saar$GR8E.2011.KDE
Gr09E.KDE              <- saar$GR9E.2011.KDE 
Gr12E.KDE              <- saar$GR12E.2011.KDE
Diplo.KDE              <- afgr$Grad.w.Diploma.in.4.years.2011.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2011.12) / 
                          as.integer (elsi$Total.Students..UG.PK.12...District..2011.12)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2011
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / saar$GR8E.2006.KDE
Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / saar$GR9E.2007.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2011.KDE / saar$GR8E.2006.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2011.KDE / saar$GR9E.2007.KDE

tot.2011 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2010
District               <- index
Year                   <- rep (2010, 168)
Total.KDE              <- saar$TOTAL.2010.KDE
Gr08E.KDE              <- saar$GR8E.2010.KDE
Gr09E.KDE              <- saar$GR9E.2010.KDE 
Gr12E.KDE              <- saar$GR12E.2010.KDE
Diplo.KDE              <- afgr$Grad.w.Diploma.in.4.years.2010.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2010.11) / 
                          as.integer (elsi$Total.Students..UG.PK.12...District..2010.11)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2010
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / saar$GR8E.2005.KDE
Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / saar$GR9E.2006.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2010.KDE / saar$GR8E.2005.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2010.KDE / saar$GR9E.2006.KDE

tot.2010 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2009
District               <- index
Year                   <- rep (2009, 168)
Total.KDE              <- saar$TOTAL.2009.KDE
Gr08E.KDE              <- saar$GR8E.2009.KDE
Gr09E.KDE              <- saar$GR9E.2009.KDE 
Gr12E.KDE              <- saar$GR12E.2009.KDE
Diplo.KDE              <- afgr$Grad.w.Diploma.in.4.years.2009.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2009.10) / 
                          as.integer (elsi$Total.Students..UG.PK.12...District..2009.10)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2009
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2009.KDE / saar$GR8E.2004.KDE
Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2009.KDE / saar$GR9E.2005.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2009.KDE / saar$GR8E.2004.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2009.KDE / saar$GR9E.2005.KDE

tot.2009 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2008
District               <- index
Year                   <- rep (2008, 168)
Total.KDE              <- saar$TOTAL.2008.KDE
Gr08E.KDE              <- saar$GR8E.2008.KDE
Gr09E.KDE              <- saar$GR9E.2008.KDE 
Gr12E.KDE              <- saar$GR12E.2008.KDE
Diplo.KDE              <- afgr$Grad.w.Diploma.in.4.years.2008.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2008.09) / 
                          as.integer (elsi$Total.Students..UG.PK.12...District..2008.09)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2008
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2008.KDE / saar$GR8E.2003.KDE
Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2008.KDE / saar$GR9E.2004.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2008.KDE / saar$GR8E.2003.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2008.KDE / saar$GR9E.2004.KDE

tot.2008 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2007
District               <- index
Year                   <- rep (2007, 168)
Total.KDE              <- saar$TOTAL.2007.KDE
Gr08E.KDE              <- saar$GR8E.2007.KDE
Gr09E.KDE              <- saar$GR9E.2007.KDE 
Gr12E.KDE              <- saar$GR12E.2007.KDE
Diplo.KDE              <- afgr$Grad.w.Diploma.in.4.years.2007.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2007.08) / 
                          as.integer (elsi$Total.Students..UG.PK.12...District..2007.08)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2007
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2007.KDE / saar$GR8E.2002.KDE
Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2007.KDE / saar$GR9E.2003.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2007.KDE / saar$GR8E.2002.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2007.KDE / saar$GR9E.2003.KDE

tot.2007 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2006
District               <- index
Year                   <- rep (2006, 168)
Total.KDE              <- saar$TOTAL.2006.KDE
Gr08E.KDE              <- saar$GR8E.2006.KDE
Gr09E.KDE              <- saar$GR9E.2006.KDE 
Gr12E.KDE              <- saar$GR12E.2006.KDE
Diplo.KDE              <- gr$Grad.w.Diploma.in.4.years.2006.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2006.07) / 
                          as.integer (elsi$Total.Students..UG.PK.12...District..2006.07)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2006
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2006.KDE / saar$GR8E.2001.KDE
Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2006.KDE / saar$GR9E.2002.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2006.KDE / saar$GR8E.2001.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2006.KDE / saar$GR9E.2002.KDE

tot.2006 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2005
District               <- index
Year                   <- rep (2005, 168)
Total.KDE              <- saar$TOTAL.2005.KDE
Gr08E.KDE              <- saar$GR8E.2005.KDE
Gr09E.KDE              <- saar$GR9E.2005.KDE 
Gr12E.KDE              <- saar$GR12E.2005.KDE
Diplo.KDE              <- gr$Grad.w.Diploma.in.4.years.2005.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2005.06) / 
  as.integer (elsi$Total.Students..UG.PK.12...District..2005.06)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2005
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2005.KDE / saar$GR8E.2000.KDE
Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2005.KDE / saar$GR9E.2001.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2005.KDE / saar$GR8E.2000.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2005.KDE / saar$GR9E.2001.KDE

tot.2005 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2004--FRD lunch data is missing for 2004.05 & 2003.04
District               <- index
Year                   <- rep (2004, 168)
Total.KDE              <- saar$TOTAL.2004.KDE
Gr08E.KDE              <- saar$GR8E.2004.KDE
Gr09E.KDE              <- saar$GR9E.2004.KDE 
Gr12E.KDE              <- saar$GR12E.2004.KDE
Diplo.KDE              <- gr$Grad.w.Diploma.in.4.years.2004.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2005.06) / 
                          as.integer (elsi$Total.Students..UG.PK.12...District..2005.06)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2004
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2004.KDE / saar$GR8E.1999.KDE
Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2004.KDE / saar$GR9E.2000.KDE
Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2004.KDE / saar$GR8E.1999.KDE
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2004.KDE / saar$GR9E.2000.KDE

tot.2004 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)


#2003
District               <- index
Year                   <- rep (2003, 168)
Total.KDE              <- saar$TOTAL.2003.KDE
Gr08E.KDE              <- saar$GR8E.2003.KDE
Gr09E.KDE              <- saar$GR9E.2003.KDE 
Gr12E.KDE              <- saar$GR12E.2003.KDE
Diplo.KDE              <- gr$Grad.w.Diploma.in.4.years.2003.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2005.06) / 
                          as.integer (elsi$Total.Students..UG.PK.12...District..2005.06)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- sdcp$PCT.CH.POV.5.17.2003
SDCP.Decile            <- cut_number (SDCP.Pct, n = 10)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- rep (NA, 168)
Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2003.KDE / saar$GR9E.1999.KDE
Gr.12.Gr.8.ratio.KDE   <- rep (NA, 168)
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2003.KDE / saar$GR9E.1999.KDE

tot.2003 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

cum.2003.2012 <- rbind (tot.2012, tot.2011, tot.2010, tot.2009, tot.2008, tot.2007,
                        tot.2006, tot.2005, tot.2004, tot.2003)

#Save as R object to load in later script
cumulative <- paste (wd, "objects", "cum.2003.2012.csv", sep = "/")
write.table (cum.2003.2012, file = cumulative, sep = ",")