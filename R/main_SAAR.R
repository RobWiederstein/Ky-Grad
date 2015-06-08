source("./R/functions_SAAR.R")

#import annual data sets, spruce them up, change format from wide to long

SAAR1999 <- wide_2_long_1999(spruce_SAAR1999(import_SAAR1999()))
SAAR2000 <- wide_2_long_2000(spruce_SAAR2000(import_SAAR2000()))
SAAR2001 <- wide_2_long_2001(spruce_SAAR2001(import_SAAR2001()))
SAAR2002 <- wide_2_long_2002(spruce_SAAR2002(import_SAAR2002()))
SAAR2003 <- wide_2_long_2003(spruce_SAAR2003(import_SAAR2003()))
SAAR2004 <- wide_2_long_2004(spruce_SAAR2004(import_SAAR2004()))
SAAR2005 <- wide_2_long_2005(spruce_SAAR2005(import_SAAR2005()))
SAAR2006 <- wide_2_long_2006(spruce_SAAR2006(import_SAAR2006()))
SAAR2007 <- wide_2_long_2007(spruce_SAAR2007(import_SAAR2007()))
SAAR2008 <- wide_2_long_2008(spruce_SAAR2008(import_SAAR2008()))
SAAR2009 <- wide_2_long_2009(spruce_SAAR2009(import_SAAR2009()))
SAAR2010 <- wide_2_long_2010(spruce_SAAR2010(import_SAAR2010()))
SAAR2011 <- wide_2_long_2011(spruce_SAAR2011(import_SAAR2011()))
SAAR2012 <- wide_2_long_2012(spruce_SAAR2012(import_SAAR2012()))
SAAR2013 <- wide_2_long_2013(spruce_SAAR2013(import_SAAR2013()))


#combine dataframes
df <- data.frame(
                rbind(
                SAAR1999, SAAR2000, SAAR2001, SAAR2002, SAAR2003, SAAR2004, 
                SAAR2005, SAAR2006, SAAR2007, SAAR2008, SAAR2009, SAAR2010, 
                SAAR2011, SAAR2012, SAAR2013
                ), 
                stringsAsFactors = F
)

#write out r object to objects directory
home <- getwd()
dir <- "objects"
file <- "saar_1999_2013"
myfile <- paste(home, dir, file, sep = "/")
save(df, file = myfile)

rm(list = ls())
