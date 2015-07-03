# import saar datasets by year----
import_SAAR1999 <- function(){
    a <- read.csv(paste(getwd(), "/data_raw/SAAR1999.csv", sep = ""), 
             skip = 6, 
             nrow = 176,
             header = TRUE, 
             fill = TRUE, 
             blank.lines.skip=TRUE,
             strip.white = TRUE, 
             colClasses = c("character", rep("integer", 13
                                             )
                            )
             )
    a
}

import_SAAR2000 <- function(){
    a <- read.csv(paste(getwd() ,"/data_raw/SAAR2000.csv", sep = ""), 
             skip = 6, 
             nrow = 176,
             header = TRUE, 
             fill = TRUE, 
             blank.lines.skip = TRUE,
             strip.white = TRUE, 
             colClasses = c("character", rep("integer", 13
             )
             )
    )
    a
}

import_SAAR2001 <- function(){
    a <- read.csv(paste (getwd(), "/data_raw/SAAR2001.csv", sep = ""),
             skip = 6, 
             nrow = 176, 
             header = TRUE, 
             fill = TRUE, 
             blank.lines.skip = TRUE,
             strip.white = TRUE, 
             colClasses = c("character", rep("integer", 13
             )
             )
    )
    a
}

import_SAAR2002 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2002.csv", sep = ""), 
             skip = 6, 
             nrow = 176, 
             header = TRUE, 
             fill = TRUE, 
             blank.lines.skip = TRUE,
             strip.white = TRUE, 
             colClasses = c("character", rep("integer", 13
             )
             )
    )
}

import_SAAR2003 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2003.csv", sep = ""),
             skip = 6,
             nrow = 176, 
             header = TRUE, 
             fill = TRUE, 
             blank.lines.skip = TRUE,
             strip.white = TRUE, 
             colClasses = c("character", rep("integer", 13
             )
             )
    )
}
import_SAAR2004 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2004.csv", sep = ""), 
             header = TRUE, 
             sep = ",", 
             blank.lines.skip = TRUE, 
             fill = TRUE, 
             skip = 6,
             strip.white = TRUE, 
             nrow = 351,
             colClasses = c("numeric", "character", rep("integer", 13
             )
             )
    )
}
import_SAAR2005 <- function(){
    a <- read.csv(paste (getwd(), "/data_raw/SAAR2005.csv", sep = ""), 
                  header = TRUE, 
                  sep=",", 
                  blank.lines.skip = TRUE, 
                  fill = TRUE, 
                  skip = 6,
                  strip.white = TRUE, 
                  nrow = 176,
                  colClasses = c("character", rep("integer", 13
                  )
                  )
    )
}
import_SAAR2006 <- function(){
    a <- read.csv(paste (getwd(), "/data_raw/SAAR2006.csv", sep = ""), 
                  header = TRUE, 
                  sep = ",", 
                  blank.lines.skip = TRUE, 
                  fill = TRUE, 
                  skip = 6,
                  strip.white = TRUE,
                  colClasses = c("character", "character", rep("integer", 13
                  )
                  )
    )
}
import_SAAR2007 <- function(){
    a <-read.csv(paste (getwd(), "/data_raw/SAAR2007.csv", sep = ""), 
                 header = TRUE, 
                 sep = ",", 
                 blank.lines.skip = TRUE, 
                 fill = TRUE, 
                 skip = 6,
                 strip.white=TRUE,
                 colClasses = c("character", "character", rep("integer", 13
                 )
                 ) 
    )
}
import_SAAR2008 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2008.csv", sep = ""), 
             header = TRUE, 
             sep = ",", 
             blank.lines.skip = TRUE, 
             fill = TRUE, 
             skip = 6,
             strip.white = TRUE,
             colClasses = c("character", "character", rep("integer", 15
             )
             )
    )
}
import_SAAR2009 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2009.csv", sep = ""), 
             header = TRUE, 
             sep = ",", 
             blank.lines.skip = TRUE, 
             fill = TRUE, 
             skip = 6,
             strip.white = TRUE,
             colClasses = c("character", "character", rep("integer", 15
             )
             )
    )
}
import_SAAR2010 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2010.csv", sep = ""), 
             header = TRUE, 
             sep=",", 
             blank.lines.skip = TRUE, 
             fill = TRUE, 
             skip = 6,
             strip.white=TRUE,
             colClasses = c("character", "character", rep("integer", 15
             )
             )
    )
}
import_SAAR2011 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2011.csv", sep = ""), 
             header = TRUE, 
             sep = ",", 
             blank.lines.skip = TRUE, 
             fill = TRUE, 
             skip = 6,
             strip.white = TRUE,
             colClasses = c("character", "character", rep("integer", 15
             )
             )
    )
}
import_SAAR2012 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2012.csv", sep = ""), 
             header = TRUE, 
             sep = ",", 
             blank.lines.skip = TRUE, 
             fill = TRUE, 
             skip = 6,
             strip.white = TRUE,
             colClasses = c("character", "character", rep("integer", 15
             )
             )
    ) 
}
import_SAAR2013 <- function(){
    read.csv(paste (getwd(), "/data_raw/SAAR2013.csv", sep = ""), 
             header = TRUE, 
             sep = ",", 
             blank.lines.skip = TRUE, 
             fill = TRUE, 
             skip = 6,
             strip.white = TRUE,
             colClasses = c("character", "character", rep("integer", 15
             )
             )
    )
}

#clean each year's data for merge ----
#index column for merge using 1999 as base year
create_index <- function(){
    district.index <- import_SAAR1999()[,1]        #school districts
    district.index<-sub("^[0-9][0-9][0-9].", "" , district.index)     #remove number in front
    district.index<-sub("Independent", "Independen", district.index) #add "t" to independent
    district.index<-sub("Independen", "Independ", district.index)
    district.index<-sub("Independ", "Independent", district.index)
    district.index<-as.data.frame(district.index)
    names(district.index)<-c("DISTRICT")
    district.index
}

#spruce the data up
spruce_SAAR1999 <- function(temp){
    temp$DISTRICT <- sub("^[0-9][0-9][0-9].", "" , temp$DISTRICT)
    temp$DISTRICT <- sub("Independent", "Independen", temp$DISTRICT)
    temp$DISTRICT <- sub("Independen", "Independ", temp$DISTRICT)
    temp$DISTRICT <- sub("Independ", "Independent",temp$DISTRICT)
    setdiff(create_index()$DISTRICT, temp$DISTRICT)
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}

spruce_SAAR2000 <- function(temp){
    temp$DISTRICT <- sub("^[0-9][0-9][0-9].", "" , temp$DISTRICT)
    temp$DISTRICT <- sub("Independent", "Independen", temp$DISTRICT)
    temp$DISTRICT <- sub("Independen", "Independ", temp$DISTRICT)
    temp$DISTRICT <- sub("Independ", "Independent", temp$DISTRICT)
    temp$DISTRICT <- sub("Erlanger-Elsmere Independent.", 
                         "Erlanger-Elsmere Independent", 
                         temp$DISTRICT
    )
    temp$DISTRICT <- sub("Walton-Verona Independent", 
                         "Walton Verona Independent",
                         temp$DISTRICT
    )
    temp
}

spruce_SAAR2001 <- function(temp){
    temp$DISTRICT <- sub("^[0-9][0-9][0-9].", "" , temp$DISTRICT)
    temp$DISTRICT <- sub("Independent", "Independen", temp$DISTRICT)
    temp$DISTRICT <- sub("Independen", "Independ", temp$DISTRICT)
    temp$DISTRICT <- sub("Independ", "Independent", temp$DISTRICT)
    temp$DISTRICT <- sub("Walton-Verona Independent", 
                         "Walton Verona Independent", 
                         temp$DISTRICT
    )
    setdiff(create_index()$DISTRICT, temp$DISTRICT)
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}


spruce_SAAR2002 <- function(temp){
    temp$DISTRICT <- sub("^[0-9][0-9][0-9].", "" , temp$DISTRICT)
    temp$DISTRICT <- sub("Independent", "Independen", temp$DISTRICT)
    temp$DISTRICT <- sub("Independen", "Independ", temp$DISTRICT)
    temp$DISTRICT <- sub("Independ", "Independent", temp$DISTRICT)
    temp$DISTRICT <- sub("Walton-Verona Independent",
                         "Walton Verona Independent",
                         temp$DISTRICT
    )
    
    setdiff(create_index()$DISTRICT, temp$DISTRICT)
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2003 <- function(temp){
    temp$DISTRICT <- sub("^[0-9][0-9][0-9].", "" , temp$DISTRICT)
    temp$DISTRICT <- sub("Independent", "Independen", temp$DISTRICT)
    temp$DISTRICT <- sub("Independen", "Independ", temp$DISTRICT)
    temp$DISTRICT <- sub("Independ", "Independent", temp$DISTRICT)
    temp$DISTRICT <- sub("Walton-Verona Independent",
                         "Walton Verona Independent",
                         temp$DISTRICT)
    setdiff(create_index()$DISTRICT, temp$DISTRICT)
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2004 <- function(temp){
    temp <- temp[!is.na(temp$X11),]
    temp <- temp[,2:15]
    names (temp)[1] <- "DISTRICT"
    temp$DISTRICT <- sub("Walton-Verona Independent", 
                         "Walton Verona Independent", 
                         temp$DISTRICT
    )
    setdiff(create_index()$DISTRICT, temp$DISTRICT)
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2005 <- function(temp){
    temp$DISTRICT<-sub("^[0-9][0-9][0-9].", "" , temp$DISTRICT)
    temp$DISTRICT<-sub("Walton-Verona Independent", 
                       "Walton Verona Independent", 
                       temp$DISTRICT
    )
    setdiff(create_index()$DISTRICT, temp$DISTRICT)
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2006 <- function(temp){
    temp <- temp[!is.na(temp$X11),]
    temp <- temp[nchar(temp[,2])==0,]
    temp[165,1] <- "Walton Verona Independent"
    temp <- temp[, -2]
    temp <- temp[-176, ]  #Grand Total
    names (temp)[1] <- toupper(names(temp)[1])
    setdiff(create_index()$DISTRICT, temp$DISTRICT) #Harrodsburg Independent missing
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2007 <- function(temp){
    temp <- temp[,1:15]
    temp <- subset(temp, (grepl("TOTALS", temp[, 1])))  #extract districts by totals
    temp$DISTRICT <- sub("TOTALS for ", "", temp$DISTRICT)
    temp<-temp[, -2]
    temp[164,1] <- "Walton Verona Independent"
    temp[144,1] <- "Raceland Independent"
    setdiff(create_index()$DISTRICT, temp$DISTRICT) #Providence/Harrodsburg 
    #Independent missing
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2008 <- function(temp){
    temp <- subset(temp, (grepl("TOTALS", temp[,1])))  #extract districts by totals
    temp$DISTRICT <- sub("TOTALS for ", "",temp$DISTRICT)
    temp <- temp[,-2]
    temp[144,1] <- "Raceland Independent"
    temp[164,1] <- "Walton Verona Independent"
    setdiff(create_index()$DISTRICT, temp$DISTRICT) #Providence/Harrodsburg 
    #Independent missing
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2009 <- function(temp){
    temp <- subset(temp, (grepl("TOTALS", temp$DISTRICT)))
    temp$DISTRICT <- sub("TOTALS for ", "",temp$DISTRICT)
    temp[86,1]<-"Jefferson County"
    temp[144,1]<-"Raceland Independent"
    temp[164,1]<-"Walton Verona Independent"
    temp <- temp[, -2]
    setdiff(create_index()$DISTRICT, temp$DISTRICT) #Providence/Harrodsburg 
    #Independent missing
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2010 <- function(temp){
    temp <- subset(temp, (grepl("TOTALS", temp$DISTRICT)))
    temp$DISTRICT <- gsub("TOTALS for ", "",temp$DISTRICT)
    temp <- temp[, -c(2,3)]
    temp[86,1] <- "Jefferson County"
    temp[144,1] <- "Raceland Independent"
    temp[164,1] <- "Walton Verona Independent"
    setdiff(create_index()$DISTRICT, temp$DISTRICT) #Providence/Harrodsburg 
    #Independent missing
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2011 <- function(temp){
    temp <- subset(temp, (grepl("TOTALS", temp$DISTRICT)))
    temp$DISTRICT <- gsub("TOTALS for ", "",temp$DISTRICT)
    temp <- temp[, -2]
    temp[86,1] <- "Jefferson County"
    temp[144,1] <- "Raceland Independent"
    temp[164,1]<-"Walton Verona Independent"
    setdiff(create_index()$DISTRICT, temp$DISTRICT) #Providence/Harrodsburg 
    #Independent missing
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2012 <- function(temp){
    temp <- subset(temp, (grepl("TOTALS", temp$DISTRICT)))
    temp$DISTRICT <- sub("TOTALS for ", "",temp$DISTRICT)
    temp <- temp[, -2]
    temp[144,1] <- "Raceland Independent"
    temp[164,1] <- "Walton Verona Independent"
    setdiff(create_index()$DISTRICT, temp$DISTRICT) #Providence/Harrodsburg 
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}
spruce_SAAR2013 <- function(temp){
    temp <- subset(temp, (grepl("TOTALS", temp$DISTRICT)))
    temp$DISTRICT <- sub("TOTALS for ", "",temp$DISTRICT)
    temp <- temp[, -2]
    temp[143,1] <- "Raceland Independent"
    temp[163,1] <- "Walton Verona Independent"
    setdiff(create_index()$DISTRICT, temp$DISTRICT) 
    #Providence/Harrodsburg/Monticello
    setdiff(temp$DISTRICT, create_index()$DISTRICT)
    temp
}

#go from wide to long format ----
wide_2_long_1999 <- function(temp){
    library(reshape)
    temp$year <- rep(1999, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2000 <- function(temp){
    library(reshape)
    temp$year <- rep(2000, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2001 <- function(temp){
    library(reshape)
    temp$year <- rep(2001, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2002 <- function(temp){
    library(reshape)
    temp$year <- rep(2002, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2003 <- function(temp){
    library(reshape)
    temp$year <- rep(2003, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2004 <- function(temp){
    library(reshape)
    temp$year <- rep(2004, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2005 <- function(temp){
    library(reshape)
    temp$year <- rep(2005, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2006 <- function(temp){
    library(reshape)
    temp$year <- rep(2006, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2007 <- function(temp){
    library(reshape)
    temp$year <- rep(2007, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2008 <- function(temp){
    library(reshape)
    temp$year <- rep(2008, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2009 <- function(temp){
    library(reshape)
    temp$year <- rep(2009, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2010 <- function(temp){
    library(reshape)
    temp$year <- rep(2010, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2011 <- function(temp){
    library(reshape)
    temp$year <- rep(2011, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2012 <- function(temp){
    library(reshape)
    temp$year <- rep(2012, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}
wide_2_long_2013 <- function(temp){
    library(reshape)
    temp$year <- rep(2013, nrow(temp))
    names(temp) <- tolower(names(temp))
    temp <- melt(temp, id.vars = c("year", "district"), variable_name = "grade")
}

#merge the datasets ----
merge_SAAR_ALL <- function(){
    #get functions
    #source("./R/functions_SAAR.R")
    
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
    df
}

#write it out into R object ----
write_SAAR_ALL <- function(){
    df <- merge_SAAR_ALL()
    #write out r object to "objects" directory
    wd <- getwd()
    dir <- "objects"
    file <- "saar_1999_2013"
    myfile <- paste(wd, dir, file, sep = "/")
    save(df, file = myfile)
}


