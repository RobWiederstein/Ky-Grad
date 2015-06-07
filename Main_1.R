Main          <- function (){
Clean.SAAR    <- function (){
library (gdata)
#import from data sets
import_SAAR1999 <- function(){
  read.csv(paste(getwd(), "/data sets/SAAR1999.csv", sep = ""), 
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
}
import_SAAR2000 <- function(){
  read.csv(paste(getwd() ,"/data sets/SAAR2000.csv", sep = ""), 
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
import_SAAR2001 <- function(){
  read.csv(paste (getwd(), "/data sets/SAAR2001.csv", sep = ""),
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
import_SAAR2002 <- function(){
  read.csv(paste (getwd(), "/data sets/SAAR2002.csv", sep = ""), 
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
  read.csv(paste (getwd(), "/data sets/SAAR2003.csv", sep = ""),
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
  read.csv(paste (getwd(), "/data sets/SAAR2004.csv", sep = ""), 
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
  a <- read.csv(paste (getwd(), "/data sets/SAAR2005.csv", sep = ""), 
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
  a <- read.csv(paste (getwd(), "/data sets/SAAR2006.csv", sep = ""), 
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
  a <-read.csv(paste (getwd(), "/data sets/SAAR2007.csv", sep = ""), 
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
  read.csv(paste (getwd(), "/data sets/SAAR2008.csv", sep = ""), 
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
  read.csv(paste (getwd(), "/data sets/SAAR2009.csv", sep = ""), 
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
  read.csv(paste (getwd(), "/data sets/SAAR2010.csv", sep = ""), 
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
  read.csv(paste (getwd(), "/data sets/SAAR2011.csv", sep = ""), 
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
  read.csv(paste (getwd(), "/data sets/SAAR2012.csv", sep = ""), 
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
  read.csv(paste (getwd(), "/data sets/SAAR2013.csv", sep = ""), 
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
spruce_SAAR1999 <- function(){
  temp <- import_SAAR1999()
  temp$DISTRICT <- sub("^[0-9][0-9][0-9].", "" , temp$DISTRICT)
  temp$DISTRICT <- sub("Independent", "Independen", temp$DISTRICT)
  temp$DISTRICT <- sub("Independen", "Independ", temp$DISTRICT)
  temp$DISTRICT <- sub("Independ", "Independent",temp$DISTRICT)
  setdiff(create_index()$DISTRICT, temp$DISTRICT)
  setdiff(temp$DISTRICT, create_index()$DISTRICT)
  temp
}
spruce_SAAR2000 <- function(){
  temp <- import_SAAR2000()
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
spruce_SAAR2001 <- function(){
  temp <- import_SAAR2001()
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
spruce_SAAR2002 <- function(){
  temp <- import_SAAR2002()
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
spruce_SAAR2003 <- function(){
  temp <- import_SAAR2003()
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
spruce_SAAR2004 <- function(){
  temp <- import_SAAR2004()
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
spruce_SAAR2005 <- function(){
  temp <- import_SAAR2005()
  temp$DISTRICT<-sub("^[0-9][0-9][0-9].", "" , temp$DISTRICT)
  temp$DISTRICT<-sub("Walton-Verona Independent", 
                     "Walton Verona Independent", 
                     temp$DISTRICT
                     )
  setdiff(create_index()$DISTRICT, temp$DISTRICT)
  setdiff(temp$DISTRICT, create_index()$DISTRICT)
  temp
}
spruce_SAAR2006 <- function(){
  temp <- import_SAAR2006()
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
spruce_SAAR2007 <- function(){
  temp <- import_SAAR2007()
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
spruce_SAAR2008 <- function(){
  temp <- import_SAAR2008()
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
spruce_SAAR2009 <- function(){
  temp <- import_SAAR2009()
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
spruce_SAAR2010 <- function(){
  temp <- import_SAAR2010()
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
spruce_SAAR2011 <- function(){
  temp <- import_SAAR2011()
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
spruce_SAAR2012 <- function(){
  temp <- import_SAAR2012()
  temp <- subset(temp, (grepl("TOTALS", temp$DISTRICT)))
  temp$DISTRICT <- sub("TOTALS for ", "",temp$DISTRICT)
  temp <- temp[, -2]
  temp[144,1] <- "Raceland Independent"
  temp[164,1] <- "Walton Verona Independent"
  setdiff(create_index()$DISTRICT, temp$DISTRICT) #Providence/Harrodsburg 
  setdiff(temp$DISTRICT, create_index()$DISTRICT)
  temp
}
spruce_SAAR2013 <- function(){
  temp <- import_SAAR2013()
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
merged_SAAR_ALL <- function(){
  library (reshape, reshape2)
  a <- spruce_SAAR1999()
  b <- spruce_SAAR2000()
  c <- spruce_SAAR2001()
  d <- spruce_SAAR2002()
  e <- spruce_SAAR2003()
  f <- spruce_SAAR2004()
  g <- spruce_SAAR2005()
  h <- spruce_SAAR2006()
  i <- spruce_SAAR2007()
  j <- spruce_SAAR2008()
  k <- spruce_SAAR2009()
  l <- spruce_SAAR2010()
  m <- spruce_SAAR2011()
  n <- spruce_SAAR2012()
  o <- spruce_SAAR2013()
  
  
  df <- merge  (a, b, all = T, by = "DISTRICT")
  df <- merge (df, c, all = T, by = "DISTRICT")
  df <- merge (df, d, all = T, by = "DISTRICT")
  df <- merge (df, e, all = T, by = "DISTRICT")
  df <- merge (df, f, all = T, by = "DISTRICT")
  df <- merge (df, g, all = T, by = "DISTRICT")
  df <- merge (df, h, all = T, by = "DISTRICT")
  df <- merge (df, i, all = T, by = "DISTRICT")
  df <- merge (df, j, all = T, by = "DISTRICT")
  df <- merge (df, k, all = T, by = "DISTRICT")
  df <- merge (df, l, all = T, by = "DISTRICT")
  df <- merge (df, m, all = T, by = "DISTRICT")
  df <- merge (df, n, all = T, by = "DISTRICT")
  df <- merge (df, o, all = T, by = "DISTRICT")
  
  SAAR.1999.2013 <- df[, c(grep("DISTRICT", names (df)),
                           grep("8", names (df)),
                           grep("9", names (df)), 
                           grep("12", names (df)), 
                           grep("^[Tt][Oo]", names (df)
                                )
                           )
                       ]
  
  #name columns
  names (SAAR.1999.2013) <- c("DISTRICT", 
                              paste ("GR8E", 1999:2013, "KDE", sep = "."),
                              paste ("GR9E", 1999:2013, "KDE", sep = "."),
                              paste ("GR12E", 1999:2013, "KDE", sep = "."),
                              paste ("TOTAL", 1999:2013, "KDE", sep = "."
                                     )
                              )
  SAAR.1999.2013
}
save_SAAR_ALL <- function(){
  saar.new <- paste (getwd(), "objects", "SAAR.1999.2013.csv", sep = "/")
  write.table (merged_SAAR_ALL(), file = saar.new, sep = ",")
}
}     #Superintendant Annual Attendance Report from KDE
Clean.GRAD    <- function (){
  #Grad Rate 2003-2007 was sent via email from Kentucky Department of Education
  #No longer available on website. Disaggregated by school district
  
  wd <- getwd()
  file <- paste (wd, "data sets", "Grad Rate 2003-2007.csv", sep = "/")
  gr <- read.csv (file = file, sep = ",", header = TRUE,
                  strip.white = TRUE,
                  colClasses = "character")
  file <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
  saar <- read.csv (file = file, sep = ",", header = T,
                    strip.white = T,
                    colClasses = "character")
  
  #pull out columns for 4 year graduates
  attach (gr)
  gr <- gr [, c(4, 5, grep ("GRADS_4YR", names (gr)))]
  gr <- gr [SCHNAME == "---DISTRICT TOTAL---", ]
  gr <- gr [, -2]
  detach (gr)
  
  names(gr)[2:6] <- paste ("Grad.w.Diploma.in.4.years", 2002:2006, "KDE", sep = ".")
  names(gr)[1]   <- "DISTRICT"
  gr[grep ("Walton", gr$DISTRICT),1] <- "Walton Verona Independent"

  
  #Save as R object to load in later script
  gr.new <- paste (wd, "objects", "gr.2002.2006.csv", sep = "/")
  write.csv(gr, file = gr.new)
  
  gr
}     #Graduation Rate Data 2003-2007 from KDE
Clean.AFGR    <- function (){
#AFGR data is for 2008-2012.
#David Curd with KDE advised that AFGR data is lagged one year.
#2012 data would be for 2010-11 School Year
#KDE's naming convention is to refer to the second year of the 2010-11 school year.

wd <- getwd()
file <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
saar <- read.table (file, header = TRUE, sep = ",")

AFGR <- read.csv(file = paste (wd, "data sets", "AFGR_2012.csv", sep = "/"), 
                 sep = ",", 
                 header = TRUE,
                 strip.white = TRUE,
                 colClasses = "character")
attach (AFGR)

#Diploma Recipients from KDE (DR.**) by year.  Because 1 yr lag, 2012 is for 2010-2011 school year.
#Code uses first year of school year, contrary to KDE naming conventions
DR.07 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2008'), c (1, 5, 10)]
DR.08 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2009'), c (1, 5, 10)]
DR.09 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2010'), c (1, 5, 10)]
DR.10 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2011'), c (1, 5, 10)]
DR.11 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2012'), c (1, 5, 10)]   
rm (AFGR)
detach (AFGR)


#create a common column for merge with SAAR data
index <- as.data.frame (saar$DISTRICT, stringsAsFactors = F)
names (index) <- "DISTRICT"

DR.07[84, 2]  <- "Jefferson County"
DR.07[91, 2]  <- "LaRue County"
DR.07[110, 2] <- "McCracken County"
DR.07[111, 2] <- "McCreary County"
DR.07[112, 2] <- "McLean County"
DR.07[142, 2] <- "Raceland Independent"
DR.07[160, 2] <- "Walton Verona Independent"

#assign common column to DR.** for merge
DR.08[,2] <- DR.07[,2]
DR.09[,2] <- DR.07[,2]
DR.10[,2] <- DR.07[,2]
DR.11[,2] <- DR.07[,2]

#assign common column name "DISTRICT"
names(DR.07)[2] <- "DISTRICT"
names(DR.08)[2] <- "DISTRICT"
names(DR.09)[2] <- "DISTRICT"
names(DR.10)[2] <- "DISTRICT"
names(DR.11)[2] <- "DISTRICT"

#merge
afgr.07.11 <- merge(DR.07, DR.08, by = "DISTRICT", all = T)
afgr.07.11 <- merge(afgr.07.11, DR.09, by = "DISTRICT", all = T)
afgr.07.11 <- merge(afgr.07.11, DR.10, by = "DISTRICT", all = T)
afgr.07.11 <- merge(afgr.07.11, DR.11, by = "DISTRICT", all = T)
afgr.07.11 <- afgr.07.11 [, c(1,3,5,7,9,11)]
names(afgr.07.11)[2:6] <- paste ("Grad.w.Diploma.in.4.years", 2007:2011, "KDE", sep = ".")

#Save as R object to load in later script
afgr.new <- paste (wd, "objects", "afgr.07.11.csv", sep = "/")
write.table (afgr.07.11, file = afgr.new, sep = ",")

afgr.07.11
}     #Average Freshmen Graduation Rate 2008-2012 from KDE
Clean.AGRC    <- function (){
#http://applications.education.ky.gov/SRC/DataSets.aspx
#2012-2013 Diploma Recipients w/i 4 years

wd <- getwd()
file <- paste (wd, "data sets", "ACCOUNTABILITY_GRADUATION_RATE_COHORT.csv", sep = "/")
agrc <- read.csv (file, header = T, sep = ",", as.is = T,
                strip.white = T)
file <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
saar <- read.csv (file, header = T, sep = ",", as.is = T, strip.white = T)
index <- data.frame (saar$DISTRICT)

agrc <- agrc [agrc$DISAGG_ORDER == "0" & agrc$SCH_NAME == "--District Total--",]
agrc <- agrc [, c(1, 4, 8)]
names(agrc)[3] <- paste ("Grad.w.Diploma.in.4.years", 2012, "KDE", sep = ".")
names (agrc)[2] <- "DISTRICT"
agrc$DISTRICT[142] <- "Raceland Independent"
agrc$DISTRICT[160] <- "Walton Verona Independent"

setdiff (saar$DISTRICT, agrc$DISTRICT)  #7 schools missing
#Save as R object to load in later script
file <- paste (wd, "objects", "agrc.12.csv", sep = "/")
write.table (agrc, file = file, sep = ",")
agrc
}     #Accountability Graduation Rate Cohort 2013 from KDE
################################################################################## Start Here!
Clean.AGRC14 <- function (){
  wd <- getwd()
  file <- paste (wd, "data sets", "ACCOUNTABILITY_GRADUATION_RATE_COHORT_2013_14.csv", sep = "/")
  agrc14 <- read.csv (file, header = T, sep = ",", as.is = T,
                    strip.white = T)
  file <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
  saar <- read.csv (file, header = T, sep = ",", as.is = T, strip.white = T)
  index <- data.frame (saar$DISTRICT)
  
  
  agrc14 <- na.omit(agrc14)

  
  agrc14 <- agrc14 [agrc14$SCH_NAME == "---District Total---", ]
  agrc14 <- agrc14 [agrc14$DISAGG_ORDER == "0", ]
  agrc14 <- agrc14 [agrc14$SCH_YEAR != NA, ]
  agrc14 <- agrc14 [, c(1, 4, 8)]
  names(agrc14)[3] <- paste ("Grad.w.Diploma.in.4.years", 2012, "KDE", sep = ".")
  names (agrc14)[2] <- "DISTRICT"
  agrc14$DISTRICT[142] <- "Raceland Independent"
  agrc14$DISTRICT[160] <- "Walton Verona Independent"
  
  setdiff (saar$DISTRICT, agrc$DISTRICT)  #7 schools missing
  #Save as R object to load in later script
  file <- paste (wd, "objects", "agrc.12.csv", sep = "/")
  write.table (agrc, file = file, sep = ",")
  agrc14
  
}

Clean.KYGR    <- function (){
  #build table for Kentucky state-wide reported graduation rates
  #no computation--pulled Ky's number from KDE data
  library (ggplot2)
  wd <- getwd()
  
  #get grad rate data from KDE
  file <- paste (wd, "data sets", "Grad Rate 2003-2007.csv", sep = "/")
  gr <- read.csv (file, header = T, sep = ",", as.is = T,
                  strip.white = T)
  
  #get afgr data from KDE
  file <- paste (wd, "data sets", "AFGR_2012.csv", sep = "/")
  afgr <- read.csv (file, header = T, sep = ",", as.is = T,
                    strip.white = T)
  
  #get AGRC data from KDE
  file <- paste (wd, "data sets", "ACCOUNTABILITY_GRADUATION_RATE_COHORT.csv",
                 sep = "/")
  agrc <- read.csv (file, header = T, sep = ",", as.is = T,
                    strip.white = T)
  
  
  # get state totals for 2003-2007
  gr <- gr[gr$DISTNAME == "STATE", ]
  gr <- gr [, grep ("GRAD_", names (gr))]
  
  #get state totals for 2007 to 2012
  afgr <- afgr [afgr$School.Name == "STATE TOTAL" & afgr$Gender == "Total" & 
                  afgr$Ethnicity == "Total", c(1, 5, 19)]
  
  #get state totals for 2012-2013
  agrc <- agrc [agrc$DISAGG_ORDER == "0" & agrc$DIST_NAME == "State", 
                c(1, 4, 10)]
  
  #combine in new table from 2002-2003 to 2012-2013
  Year <- 2002:2012
  State <- rep ("KY", length (2002:2012))
  Reported <- (c(as.numeric (gr[1, 1:5]), as.numeric (afgr[5:1,3]),
                 as.numeric (agrc[,3]))) / 100
  Method <- c(rep ("Leaver", 5), rep ("AFGR", 5), rep ("Cohort", 1))
  Ky.grad.rate <- (cbind (Year, State, Reported, Method))
  Ky.grad.rate <- as.data.frame (Ky.grad.rate, row.names = 1:10, stringsAsFactors = F)
  Ky.grad.rate$Year <- as.integer (Ky.grad.rate$Year)
  Ky.grad.rate$Reported <- as.numeric (Ky.grad.rate$Reported)
  
  #Save as R object to load in later script
  file <- paste (wd, "objects", "State.grad.rate.2002.2012.KDE.csv", sep = "/")
  write.table (Ky.grad.rate, file = file, sep = ",")
  Ky.grad.rate
}     #Kentucky Reported Graduation Rate
Clean.ELSI    <- function (){
  #ELSI Cleanup Script
  wd <- getwd()
  file <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
  saar <- read.table (file, header = TRUE, sep = ",",
                      colClasses = "character")
  
  #import NCES download
  wd <- getwd()
  ELSI <- paste (wd, "data sets", "ELSI_csv_export_6353280376697842613140.csv",
                 sep = "/")
  
  elsi <- read.csv(file = ELSI, 
                   sep = ",", 
                   skip = 6,
                   nrow = 178,
                   header = TRUE,
                   strip.white = TRUE,
                   colClasses = "character")
  
  #build common column for merge
  index <- saar$DISTRICT
  rm (saar)
  reconcile <- tolower (elsi$Agency.Name)
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }  
  reconcile <- as.array (sapply(reconcile, simpleCap, simplify = T, USE.NAMES = F))
  reconcile <- gsub ("Co$", "County", reconcile)
  reconcile <- gsub ("Ind$", "Independent", reconcile)
  reconcile <- gsub ("elsmere", "Elsmere", reconcile)
  reconcile <- gsub ("Larue", "LaRue", reconcile)
  reconcile <- gsub ("Mccracken", "McCracken", reconcile)
  reconcile <- gsub ("Mccreary", "McCreary", reconcile)
  reconcile <- gsub ("Mclean", "McLean", reconcile)
  reconcile <- gsub ("In$", "Independent", reconcile)
  
  elsi$Agency.Name <- reconcile
  names(elsi)[1] <- "DISTRICT"
  
  #Merge to index
  index <- as.data.frame (index)
  names(index)[1] <- "DISTRICT"
  elsi <- merge (index, elsi, by = "DISTRICT", all = T) 
  #only difference between index is Richmond Indpendent which
  #764 students in 1987-88 school year with 32 diploma recipients in 1986-87
  #also Maysville Independent which had 38 diploma recipients in its last
  #year if existence in 1988-99. It had 666 students PK-12 in 1989-90.
  
  
  
  #Save as R object to load in later script
  file <- paste(wd, "objects", "elsi_new.csv", sep = "/")
  write.table (elsi, file = file, sep = ",")
elsi
}     #Elementary Secondary from CCD
Clean.FRDL    <- function (){
#need the free and reduced lunch for 2012.  KDE website for school report card

library (ggplot2)
wd <- getwd()

#get grad rate data from KDE
file <- paste (wd, "data sets", "LEARNING_ENVIRONMENT_STUDENTS-TEACHERS.csv", sep = "/")
le <- read.csv (file, header = T, sep = ",", as.is = T,
                strip.white = T)

#cut down data frame
le <- le[, c(1, 3, 4, 7, 26:29)]
le <- le [le$SCH_NAME =="---District---" ,]
le <- le[1:174, -c(3, 6, 8)]

#set columns to integer
le$ENROLLMENT_TOTAL <- sub (",", "", le$ENROLLMENT_TOTAL)
le$ENROLLMENT_FREE_LUNCH_CNT <- sub (",", "", le$ENROLLMENT_FREE_LUNCH_CNT)
le$ENROLLMENT_REDUCED_LUNCH_CNT <- sub (",", "", le$ENROLLMENT_REDUCED_LUNCH_CNT)
le [, 3] <- as.integer (le [, 3])
le [, 4] <- as.integer (le [, 4])
le [, 5] <- as.integer (le [, 5])


#clean it up
names (le)[2] <- "DISTRICT"
ENROLLMENT_FRD_CNT <- le$ENROLLMENT_FREE_LUNCH_CNT + le$ENROLLMENT_REDUCED_LUNCH_CNT
FRD_LUNCH_PCT <- ENROLLMENT_FRD_CNT / le$ENROLLMENT_TOTAL
le <- cbind (le, FRD_LUNCH_PCT)
le$DISTRICT <- sub ("Raceland-Worthington Independent", "Raceland Independent", le$DISTRICT)
le$DISTRICT <- sub ("Walton-Verona Independent", "Walton Verona Independent", le$DISTRICT)

#Save as R object to load in later script
file <- paste (wd, "objects", "FRD.Lunch.Pct.2012.KDE.csv", sep = "/")
write.table (le, file = file, sep = ",")
}     #Free and Reduced Lunch from ELSI and KDE
Clean.SDCP    <- function (){
#http://www.census.gov/did/www/saipe/data/schools/index.html
#SDCP = School District Child Poverty
#Poverty rates by school district b/c FRL crude
#get saar and subset to index
#triple checked
wd <-getwd()
file  <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
saar <- read.csv (file, as.is = T)
index <- as.data.frame (saar[,1])
names (index) <- "DISTRICT"
index$DISTRICT <- as.character (index$DISTRICT)

#import and clean 2003
wd <- getwd()
file <- paste (wd, "data sets", "sd03_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd03 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd03 <- sd03 [, 3:6]
names (sd03) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd03 [,4] / sd03 [,3], 4) # % of poor children / all children
sd03 <- cbind (sd03, PCT.CH.POV.5.17)
names (sd03)[-1] <- paste (names(sd03)[-1], "2003", sep = ".")
sd03$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd03$DISTRICT)

#match index columns up for merge
#change first letter to cap
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

a <-  tolower(sd03$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd03$DISTRICT <- a

sd03$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd03$DISTRICT[96] <- "LaRue County"
sd03$DISTRICT[115] <- "McCracken County"
sd03$DISTRICT[116] <- "McCreary County"
sd03$DISTRICT[117] <- "McLean County"

#index does not include Fort Campbell or Fort Knox Dependent Schools
#school district child poverty rate
sdcp <- merge (index, sd03, all = T)

#import and clean 2004
wd <- getwd()
file <- paste (wd, "data sets", "sd04_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd04 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd04 <- sd04 [, 3:6]
names (sd04) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd04 [,4] / sd04 [,3], 4)
sd04 <- cbind (sd04, PCT.CH.POV.5.17)
names (sd04)[-1] <- paste (names(sd04)[-1], "2004", sep = ".")

sd04$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd04$DISTRICT)
a <-  tolower(sd04$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd04$DISTRICT <- a

sd04$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd04$DISTRICT[96] <- "LaRue County"
sd04$DISTRICT[115] <- "McCracken County"
sd04$DISTRICT[116] <- "McCreary County"
sd04$DISTRICT[117] <- "McLean County"

sdcp <- merge (sdcp, sd04, all = T)

#inport and clean 2005
wd <- getwd()
file <- paste (wd, "data sets", "sd05_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd05 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd05 <- sd05 [, 3:6]
names (sd05) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd05 [,4] / sd05 [,3], 4)
sd05 <- cbind (sd05, PCT.CH.POV.5.17)
names (sd05)[-1] <- paste (names(sd05)[-1], "2005", sep = ".")

sd05$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd05$DISTRICT)
a <-  tolower(sd05$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd05$DISTRICT <- a

sd05$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd05$DISTRICT[96] <- "LaRue County"
sd05$DISTRICT[115] <- "McCracken County"
sd05$DISTRICT[116] <- "McCreary County"
sd05$DISTRICT[117] <- "McLean County"

sdcp <- merge (sdcp, sd05, all = T)

#import and clean 2006
wd <- getwd()
file <- paste (wd, "data sets", "sd06_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd06 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd06 <- sd06 [, 3:6]
names (sd06) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd06 [,4] / sd06 [,3], 4)
sd06 <- cbind (sd06, PCT.CH.POV.5.17)
names (sd06)[-1] <- paste (names(sd06)[-1], "2006", sep = ".")

sd06$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd06$DISTRICT)
a <-  tolower(sd06$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd06$DISTRICT <- a

sd06$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd06$DISTRICT[96] <- "LaRue County"
sd06$DISTRICT[115] <- "McCracken County"
sd06$DISTRICT[116] <- "McCreary County"
sd06$DISTRICT[117] <- "McLean County"

sdcp <- merge (sdcp, sd06, all = T)

#import and clean 2007
wd <- getwd()
file <- paste (wd, "data sets", "sd07_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd07 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd07 <- sd07 [, 3:6]
names (sd07) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd07 [,4] / sd07 [,3], 4)
sd07 <- cbind (sd07, PCT.CH.POV.5.17)
names (sd07)[-1] <- paste (names(sd07)[-1], "2007", sep = ".")

sd07$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd07$DISTRICT)
a <-  tolower(sd07$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd07$DISTRICT <- a

sd07$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd07$DISTRICT[95] <- "LaRue County"
sd07$DISTRICT[114] <- "McCracken County"
sd07$DISTRICT[115] <- "McCreary County"
sd07$DISTRICT[116] <- "McLean County"


sdcp <- merge (sdcp, sd07, all = T)

#import and clean 2008
wd <- getwd()
file <- paste (wd, "data sets", "sd08_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd08 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd08 <- sd08 [, 3:6]
names (sd08) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd08 [,4] / sd08 [,3], 4)
sd08 <- cbind (sd08, PCT.CH.POV.5.17)
names (sd08)[-1] <- paste (names(sd08)[-1], "2008", sep = ".")

sd08$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd08$DISTRICT)
a <-  tolower(sd08$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd08$DISTRICT <- a

sd08$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd08$DISTRICT[95] <- "LaRue County"
sd08$DISTRICT[114] <- "McCracken County"
sd08$DISTRICT[115] <- "McCreary County"
sd08$DISTRICT[116] <- "McLean County"

sdcp <- merge (sdcp, sd08, all = T)

#import and clean 2009
wd <- getwd()
file <- paste (wd, "data sets", "sd09_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd09 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd09 <- sd09 [, 3:6]
names (sd09) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd09 [,4] / sd09 [,3], 4)
sd09 <- cbind (sd09, PCT.CH.POV.5.17)
names (sd09)[-1] <- paste (names(sd09)[-1], "2009", sep = ".")

sd09$DISTRICT <- gsub (" School District", "", sd09$DISTRICT)

#name changes and hyphens
sd09$DISTRICT[95] <- "LaRue County"
sd09$DISTRICT[146] <- "Raceland Independent"
sd09$DISTRICT[166] <- "Walton Verona Independent"

#merge
sdcp <- merge (sdcp, sd09, all = T)

#import and clean 2010
wd <- getwd()
file <- paste (wd, "data sets", "sd10_KY.txt",  sep = "/")
widths <- c(2, 6, 72, 10, 10, 8, 21)
sd10 <- read.fwf(file = file, widths = widths, colClasses = "character", strip.white = T)
sd10 <- sd10 [, 3:6]
names (sd10) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (as.integer (sd10 [,4]) / as.integer (sd10 [,3]), 4)
sd10 <- cbind (sd10, PCT.CH.POV.5.17)
names (sd10)[-1] <- paste (names(sd10)[-1], "2010", sep = ".")
sd10$DISTRICT <- gsub (" School District", "", sd10$DISTRICT)

#name changes and hyphens
sd10$DISTRICT[95] <- "LaRue County"
sd10$DISTRICT[146] <- "Raceland Independent"
sd10$DISTRICT[166] <- "Walton Verona Independent"

sdcp <- merge (sdcp, sd10, all = T)

#import and clean 2011
wd <- getwd()
file <- paste (wd, "data sets", "sd11_KY.txt",  sep = "/")
widths <- c(2, 6, 72, 10, 10, 8, 21)
sd11 <- read.fwf(file = file, widths = widths, colClasses = "character", strip.white = T)
sd11 <- sd11 [, 3:6]
names (sd11) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (as.integer (sd11 [,4]) / as.integer (sd11 [,3]), 4)
sd11 <- cbind (sd11, PCT.CH.POV.5.17)
names (sd11)[-1] <- paste (names(sd11)[-1], "2011", sep = ".")
sd11$DISTRICT <- gsub (" School District", "", sd11$DISTRICT)

#name changes and hyphens
sd11$DISTRICT[95] <- "LaRue County"
sd11$DISTRICT[146] <- "Raceland Independent"
sd11$DISTRICT[166] <- "Walton Verona Independent"

sdcp <- merge (sdcp, sd11, all = T)

#import and clean 2012
wd <- getwd()
file <- paste (wd, "data sets", "sd12_KY.txt",  sep = "/")
widths <- c(2, 6, 72, 10, 10, 8, 21)
sd12 <- read.fwf(file = file, widths = widths, colClasses = "character", strip.white = T)
sd12 <- sd12 [, 3:6]
names (sd12) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (as.integer (sd12 [,4]) / as.integer (sd12 [,3]), 4)
sd12 <- cbind (sd12, PCT.CH.POV.5.17)
names (sd12)[-1] <- paste (names(sd12)[-1], "2012", sep = ".")
sd12$DISTRICT <- gsub (" School District", "", sd12$DISTRICT)

#name changes and hyphens
sd12$DISTRICT[95] <- "LaRue County"
sd12$DISTRICT[146] <- "Raceland Independent"
sd12$DISTRICT[166] <- "Walton Verona Independent"

sdcp <- merge (sdcp, sd12, all = T)

#Need 2013.  Not available as of August 21, 2014.
#http://www.census.gov/did/www/saipe/data/schools/index.html
#used 2012 for 2013
sd13 <- sd12
names (sd13) <- gsub ("2012", "2013", names (sd13))
sdcp <- merge (sdcp, sd13, all = T)

#pull out percentages
sdcp <- sdcp[, c(1, grep ("PCT", names (sdcp)))]

#eliminate Fort Campbell and Fort Knox Schools
sdcp <- merge (index, sdcp)

#save in objects
wd <- getwd()
file <- paste (wd, "objects", "Child.Poverty.by.School.District.Census.2003.2013.csv", sep = "/")
write.csv (sdcp, file)
}     #School District Child Poverty from U.S. Census
Build.CUM     <- function (){
#load all cleaned up objects for a build
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
  Gr.8.Cohort.KDE        <- agrc$Grad.w.Diploma.in.4.years.2012.KDE / saar$GR8E.2008.KDE
  Gr.9.Cohort.KDE        <- agrc$Grad.w.Diploma.in.4.years.2012.KDE / saar$GR9E.2009.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2012.KDE / saar$GR8E.2008.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2012.KDE / saar$GR9E.2009.KDE
  
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
  Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / saar$GR8E.2007.KDE
  Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / saar$GR9E.2008.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2011.KDE / saar$GR8E.2007.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2011.KDE / saar$GR9E.2008.KDE
  
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
  Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / saar$GR8E.2006.KDE
  Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / saar$GR9E.2007.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2010.KDE / saar$GR8E.2006.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2010.KDE / saar$GR9E.2007.KDE
  
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
  Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2009.KDE / saar$GR8E.2005.KDE
  Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2009.KDE / saar$GR9E.2004.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2009.KDE / saar$GR8E.2005.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2009.KDE / saar$GR9E.2006.KDE
  
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
  Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2008.KDE / saar$GR8E.2004.KDE
  Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2008.KDE / saar$GR9E.2005.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2008.KDE / saar$GR8E.2004.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2008.KDE / saar$GR9E.2005.KDE
  
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
  Gr.8.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2007.KDE / saar$GR8E.2003.KDE
  Gr.9.Cohort.KDE        <- afgr$Grad.w.Diploma.in.4.years.2007.KDE / saar$GR9E.2004.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2007.KDE / saar$GR8E.2003.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2007.KDE / saar$GR9E.2004.KDE
  
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
  Gr.8.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2006.KDE / saar$GR8E.2002.KDE
  Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2006.KDE / saar$GR9E.2003.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2006.KDE / saar$GR8E.2002.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2006.KDE / saar$GR9E.2003.KDE
  
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
  Gr.8.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2005.KDE / saar$GR8E.2001.KDE
  Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2005.KDE / saar$GR9E.2002.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2005.KDE / saar$GR8E.2001.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2005.KDE / saar$GR9E.2002.KDE
  
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
  Gr.8.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2004.KDE / saar$GR8E.2000.KDE
  Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2004.KDE / saar$GR9E.2001.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2004.KDE / saar$GR8E.2000.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2004.KDE / saar$GR9E.2001.KDE
  
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
  Gr.8.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2003.KDE / saar$GR8E.1999.KDE
  Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2003.KDE / saar$GR9E.2000.KDE
  Gr.12.Gr.8.ratio.KDE   <- saar$GR12E.2003.KDE / saar$GR8E.1999.KDE
  Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2003.KDE / saar$GR9E.2000.KDE
  
  tot.2003 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                     FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                     Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)
  
  rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
      FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
      Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)
  
#2002
District               <- index
Year                   <- rep (2002, 168)
Total.KDE              <- saar$TOTAL.2002.KDE
Gr08E.KDE              <- saar$GR8E.2002.KDE
Gr09E.KDE              <- saar$GR9E.2002.KDE 
Gr12E.KDE              <- saar$GR12E.2002.KDE
Diplo.KDE              <- gr$Grad.w.Diploma.in.4.years.2002.KDE
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2002.03) / 
  as.integer (elsi$Total.Students..UG.PK.12...District..2002.03)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- rep (NA, 168)
SDCP.Decile            <- rep (NA, 168)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- rep (NA, 168)
Gr.9.Cohort.KDE        <- gr$Grad.w.Diploma.in.4.years.2002.KDE / saar$GR9E.1999.KDE
Gr.12.Gr.8.ratio.KDE   <- rep (NA, 168)
Gr.12.Gr.9.ratio.KDE   <- saar$GR12E.2002.KDE / saar$GR9E.1999.KDE

tot.2002 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2001
District               <- index
Year                   <- rep (2001, 168)
Total.KDE              <- saar$TOTAL.2001.KDE
Gr08E.KDE              <- saar$GR8E.2001.KDE
Gr09E.KDE              <- saar$GR9E.2001.KDE 
Gr12E.KDE              <- saar$GR12E.2001.KDE
Diplo.KDE              <- rep (NA, 168)
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2001.02) / 
  as.integer (elsi$Total.Students..UG.PK.12...District..2001.02)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- rep (NA, 168)
SDCP.Decile            <- rep (NA, 168)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- rep (NA, 168)
Gr.9.Cohort.KDE        <- rep (NA, 168)
Gr.12.Gr.8.ratio.KDE   <- rep (NA, 168)
Gr.12.Gr.9.ratio.KDE   <- rep (NA, 168)

tot.2001 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#2000
District               <- index
Year                   <- rep (2000, 168)
Total.KDE              <- saar$TOTAL.2000.KDE
Gr08E.KDE              <- saar$GR8E.2000.KDE
Gr09E.KDE              <- saar$GR9E.2000.KDE 
Gr12E.KDE              <- saar$GR12E.2000.KDE
Diplo.KDE              <- rep (NA, 168)
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2000.01) / 
  as.integer (elsi$Total.Students..UG.PK.12...District..2000.01)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- rep (NA, 168)
SDCP.Decile            <- rep (NA, 168)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- rep (NA, 168)
Gr.9.Cohort.KDE        <- rep (NA, 168)
Gr.12.Gr.8.ratio.KDE   <- rep (NA, 168)
Gr.12.Gr.9.ratio.KDE   <- rep (NA, 168)

tot.2000 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

#1999
District               <- index
Year                   <- rep (1999, 168)
Total.KDE              <- saar$TOTAL.1999.KDE
Gr08E.KDE              <- saar$GR8E.1999.KDE
Gr09E.KDE              <- saar$GR9E.1999.KDE 
Gr12E.KDE              <- saar$GR12E.1999.KDE
Diplo.KDE              <- rep (NA, 168)
FRDL.Pct               <- as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..1999.00) / 
  as.integer (elsi$Total.Students..UG.PK.12...District..1999.00)
FRD.Decile             <- cut_number(FRDL.Pct, n = 10)
levels (FRD.Decile)    <- c(paste ("D0", 1:9, sep = ""), "D10")
SDCP.Pct               <- rep (NA, 168)
SDCP.Decile            <- rep (NA, 168)
levels (SDCP.Decile)   <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE        <- rep (NA, 168)
Gr.9.Cohort.KDE        <- rep (NA, 168)
Gr.12.Gr.8.ratio.KDE   <- rep (NA, 168)
Gr.12.Gr.9.ratio.KDE   <- rep (NA, 168)

tot.1999 <- cbind (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
                   FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

rm (Year, District, Total.KDE, Gr08E.KDE, Gr09E.KDE, Gr12E.KDE, Diplo.KDE,
    FRDL.Pct, FRD.Decile, SDCP.Pct, SDCP.Decile, Gr.8.Cohort.KDE, Gr.9.Cohort.KDE,
    Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.9.ratio.KDE)

cum.1999.2012 <- rbind (tot.2012, tot.2011, tot.2010, tot.2009, tot.2008, tot.2007,
                        tot.2006, tot.2005, tot.2004, tot.2003, tot.2002, tot.2001,
                        tot.2000, tot.1999)

  #Save as R object to load in later script
  file <- paste (wd, "objects", "cum.1999.2012.csv", sep = "/")
  write.csv (cum.1999.2012, file = file) 
}     #Cumulative build from data sources (Wide to Long)
Build.NAT     <- function (){
#build national eigth grade cohort per Heckman supplementary material
wd <- getwd()
file <- paste (wd, "data sets", "ELSI_csv_export_6353406681986067459539.csv", sep = "/")
gr <- read.csv (file, header = T, sep = ",", strip.white = T, skip = 6, nrow = 51,
                colClasses = "character", as.is = T)

#set to integer
gr[2:31] <- sapply (gr[2:31], FUN = as.integer)


#eliminate NAs
begin.states <- gr[,1]
apply(gr, 2, function(x) length(which(is.na(x)))) #NAs per column
gr <- gr[,-31]  #25 NAs in column 31
gr <- gr[complete.cases(gr[, 1:30]), ]
remain.states <- gr [,1]
exclude <- setdiff (begin.states, remain.states) #DC, NY, PN, SC, WI excluded

#shorten variable names
names (gr) <- sub("Grade", "Gr", names (gr))
names (gr) <- sub ("Diploma", "Dipl", names (gr))
names (gr) <- sub ("State", "", names (gr))
names (gr) <- sub ("Students", "Stud", names (gr))
names (gr) <- sub ("Recipients", "Reci", names (gr))
names (gr) <- gsub ("..", ".", names (gr), fixed = T)
names (gr) <- gsub ("..", ".", names (gr), fixed = T)
names (gr) <- gsub ("..", ".", names (gr), fixed = T)
names (gr) <- gsub ("..", ".", names (gr), fixed = T)

#total columns
total <- colSums (gr[,2:ncol (gr)], na.rm = T)
total <- as.data.frame (total)
total <- t (total)
total <-as.data.frame (total)



Gr.8.C.National.2009.10 <- total$Dipl.Reci.2009.10 / total$Gr.8.Stud.2005.06
Gr.8.C.National.2008.09 <- total$Dipl.Reci.2008.09 / total$Gr.8.Stud.2004.05
Gr.8.C.National.2007.08 <- total$Dipl.Reci.2007.08 / total$Gr.8.Stud.2003.04
Gr.8.C.National.2006.07 <- total$Dipl.Reci.2006.07 / total$Gr.8.Stud.2002.03
Gr.8.C.National.2005.06 <- total$Dipl.Reci.2005.06 / total$Gr.8.Stud.2001.02
Gr.8.C.National.2004.05 <- total$Dipl.Reci.2004.05 / total$Gr.8.Stud.2000.01
Gr.8.C.National.2003.04 <- total$Dipl.Reci.2003.04 / total$Gr.8.Stud.1999.00
Gr.8.C.National.2002.03 <- total$Dipl.Reci.2002.03 / total$Gr.8.Stud.1998.99

Nat.Grad.Rate <- cbind (Gr.8.C.National.2002.03,   
                        Gr.8.C.National.2003.04,
                        Gr.8.C.National.2004.05,   
                        Gr.8.C.National.2005.06,   
                        Gr.8.C.National.2006.07, 
                        Gr.8.C.National.2007.08,
                        Gr.8.C.National.2008.09,   
                        Gr.8.C.National.2009.10) 

rm (Gr.8.C.National.2002.03,   
    Gr.8.C.National.2003.04,
    Gr.8.C.National.2004.05,   
    Gr.8.C.National.2005.06,   
    Gr.8.C.National.2006.07, 
    Gr.8.C.National.2007.08,
    Gr.8.C.National.2008.09,   
    Gr.8.C.National.2009.10)

Nat.Grad.Rate <- as.data.frame (t(Nat.Grad.Rate))
Year <- (2002:2009)
Nat.Grad.Rate <- cbind (Year, Nat.Grad.Rate)
names (Nat.Grad.Rate)[2] <- "Gr.8.Cohort.National"

ky <- gr[gr$.Name == "KENTUCKY", ]
Gr.8.C.KY.2009.10 <- ky$Dipl.Reci.2009.10 / ky$Gr.8.Stud.2005.06
Gr.8.C.KY.2008.09 <- ky$Dipl.Reci.2008.09 / ky$Gr.8.Stud.2004.05
Gr.8.C.KY.2007.08 <- ky$Dipl.Reci.2007.08 / ky$Gr.8.Stud.2003.04
Gr.8.C.KY.2006.07 <- ky$Dipl.Reci.2006.07 / ky$Gr.8.Stud.2002.03
Gr.8.C.KY.2005.06 <- ky$Dipl.Reci.2005.06 / ky$Gr.8.Stud.2001.02
Gr.8.C.KY.2004.05 <- ky$Dipl.Reci.2004.05 / ky$Gr.8.Stud.2000.01
Gr.8.C.KY.2003.04 <- ky$Dipl.Reci.2003.04 / ky$Gr.8.Stud.1999.00
Gr.8.C.KY.2002.03 <- ky$Dipl.Reci.2002.03 / ky$Gr.8.Stud.1998.99

KY.Grad.Rate  <- cbind (Gr.8.C.KY.2002.03,   
                        Gr.8.C.KY.2003.04,
                        Gr.8.C.KY.2004.05,   
                        Gr.8.C.KY.2005.06,   
                        Gr.8.C.KY.2006.07, 
                        Gr.8.C.KY.2007.08,
                        Gr.8.C.KY.2008.09,   
                        Gr.8.C.KY.2009.10) 
rm  (Gr.8.C.KY.2002.03,   
     Gr.8.C.KY.2003.04,
     Gr.8.C.KY.2004.05,   
     Gr.8.C.KY.2005.06,   
     Gr.8.C.KY.2006.07, 
     Gr.8.C.KY.2007.08,
     Gr.8.C.KY.2008.09,   
     Gr.8.C.KY.2009.10)
KY.Grad.Rate <- as.data.frame(t(KY.Grad.Rate))
names (KY.Grad.Rate) <- "Gr.8.Cohort.KY"
KY.Grad.Rate$Year <- 2002:2009
Nat.vs.KY.Grad.Rate <- merge (Nat.Grad.Rate, KY.Grad.Rate)

#write object out
file <- paste (wd, "objects", "Nat.vs.KY.Gr.8.Cohort.2002.2009.csv", sep = "/")
write.csv (Nat.vs.KY.Grad.Rate, file = file)

#Eighth Grade Cohort-Plot
p1 <- ggplot(Nat.vs.KY.Grad.Rate, aes (Year, Gr.8.Cohort.KY))
p1 <- p1 + geom_line(colour = "red")
p1 <- p1 + ylim (.6, .95)
p1 <- p1 + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.National), colour = "black")
p1 <- p1 + xlab("Year") + ylab ("Rate")
p1 <- p1 + ggtitle ("Eighth Grade Cohort from CCD \n National vs. Kentucky")
p1 <- p1 + annotate ("text", x = 2009.5, y = .79, label = "National",size = 3)
p1 <- p1 + annotate ("text" , x = 2009, y = .86, label = "Kentucky", colour = "red", size = 3)
p1 <- p1 + annotate("rect", xmin = 2002, xmax = 2014, ymin = .625, ymax = .925,
                    alpha = .1)
p1 <- p1 + annotate ("text", x = 2013, y = .64, label = "NCLB", size = 5)
p1
path <- paste (getwd(), "figure", sep = "/")
ggsave (p1, filename = "Eighth Grade Cohort Computed from CCD National vs Kentucky.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)

}     #From Common Core build National & KY 8th 
                                      #Grade Cohort
Fig.CCD.KDE   <- function (){
library (ggplot2)
#4 charts--compare saar ratio to common core ratio
wd <- getwd()

elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)
gr <- read.csv (paste (wd, "objects", "gr.2002.2006.csv", sep = "/" ),
                sep = ",", header = T, as.is = T)
afgr <- read.csv (paste (wd, "objects", "afgr.07.11.csv", sep = "/" ),
                  sep = ",", header = T, as.is = T)
agrc <- read.csv (paste (wd, "objects", "agrc.12.csv", sep = "/" ),
                  sep = ",", header = T, as.is = T)
saar <- read.csv (paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/"), 
                  sep = ",", header = T, as.is = TRUE)

#kde data
#denominator
saar <- saar[, c(1, grep ("GR8E", names (saar)))] 
saar<- colSums(saar[2:16], na.rm = T)

#numerator
gr <- colSums (gr[3:7], na.rm = T)
afgr <- colSums (afgr[2:6], na.rm = T)
agrc <- colSums (agrc[3], na.rm = T)
diploma.kde <- c(gr, afgr, agrc)

Year <-  1999:2013
new <- cbind (Year, saar)
Diploma.KDE <- c(rep(NA, 3), diploma.kde, NA)
new <- cbind (new, Diploma.KDE)
ratio <- new [5:14,3]/ new [1:10,2]
E.Gr.Ratio.KDE <- c(rep (NA, 4), ratio, NA)
new <- cbind (new, E.Gr.Ratio.KDE)
row.names (new) <- Year
rm (afgr, agrc, Diploma.KDE, gr, E.Gr.Ratio.KDE, saar, wd, Year, diploma.kde, ratio)

#NCES data
#numerator
dip.nces <- elsi[, c(1, grep ("Diploma.R", names (elsi)))]
names (dip.nces) <- gsub ("Diploma.Recipients.......District..", "D.R.", names (dip.nces))
dip.nces <- dip.nces[, c(1, 24:2)]
dip.nces[, 2:ncol (dip.nces)] <- sapply(dip.nces[, 2: ncol (dip.nces)], as.integer)
#dip.nces <- dip.nces [, 19:24]

dip.nces <- colSums (dip.nces[, 2: ncol (dip.nces)], na.rm = T)
dip.nces [20] <- round ((38202 + 39086) / 2, 0) #data imputation for missing data in 2005
dip.nces <- dip.nces [which (names(dip.nces)== "D.R.1999.00"): length (names (dip.nces))]
dip.nces <- c(dip.nces, rep (NA, 5))
new <- cbind (new, dip.nces)

#denominator
enr.nces <- elsi [, c(1, grep("Grade.8", names (elsi)))]
names (enr.nces) <- gsub("Grade.8.Students..Public.School..", "G8.", names (enr.nces))
enr.nces <- enr.nces [, c(1, 16:2)]

enr.nces [, 2:16] <- sapply (enr.nces[, 2:16], as.integer)
enr.nces <- colSums (enr.nces[, 2:16], na.rm = T)
enr.nces <- enr.nces [which (names (enr.nces) == "G8.1999.00"): length (enr.nces)]
enr.nces <- c(enr.nces, rep (NA, 2))
new <- cbind (new, enr.nces)
new <- as.data.frame (new)
E.Gr.Ratio.CCD <- new$dip.nces[5:10] / new$enr.nces [1:6]
names (E.Gr.Ratio.CCD) <- 2003:2008
E.Gr.Ratio.CCD <- c(rep (NA, 4), E.Gr.Ratio.CCD, rep (NA, 5))
new <- cbind (new, E.Gr.Ratio.CCD)

#Rename variables, shuffle columns
names (new) <- c("Year", "E.Gr.Enrollment.KDE", "Diploma.KDE", "E.Gr.Ratio.KDE", 
                 "Diploma.NCES", "E.Gr.Enrollment.NCES", "E.Gr.Ratio.NCES")
new <- new [, c(1,2,3,4,6,5,7)]

file     <- paste (getwd(), "objects", "CCD.vs.Kentucky.8.Cohort.csv", sep = "/")
write.csv (new, file = file) 

#Eighth Grade Cohort-Plot
p1 <- ggplot(new, aes (Year, E.Gr.Ratio.KDE))
p1 <- p1 + geom_line(colour = "red")
p1 <- p1 + ylim (.6, .95)
p1 <- p1 + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
p1 <- p1 + geom_line (aes (Year, E.Gr.Ratio.NCES), colour = "black")
p1 <- p1 + xlab("Year") + ylab ("Rate")
p1 <- p1 + ggtitle ("Eighth Grade Cohort Computed \n from CCD and KDE")
p1 <- p1 + annotate ("text", x = 2008, y = .85, label = "CCD",size = 3)
p1 <- p1 + annotate ("text" , x = 2012, y = .80, label = "KDE", colour = "red", size = 3)
p1 <- p1 + annotate("rect", xmin = 2002, xmax = 2014, ymin = .625, ymax = .925,
                    alpha = .1)
p1 <- p1 + annotate ("text", x = 2013, y = .64, label = "NCLB", size = 5)
p1
path <- paste (getwd(), "figure", sep = "/")
ggsave (p1, filename = "Eighth Grade Cohort Computed from CCD and KDE.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)

#Enrollments
p1 <- ggplot(new, aes (Year, E.Gr.Enrollment.KDE))
p1 <- p1 + geom_line(colour = "red")
p1 <- p1 + ylim (25000, 70000)
p1 <- p1 + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
p1 <- p1 + geom_line ( aes(Year, E.Gr.Enrollment.NCES), colour = "black")
p1 <- p1 + xlab("Year") + ylab ("Students") + ggtitle ("Kentucky Eighth Grade Enrollments")
p1 <- p1 + annotate ("text", x = 2011.5, y = 48000, label = "CCD", size = 3)
p1 <- p1 + annotate ("text" , x = 2012, y = 55000, label = "KDE", colour = "red", size = 3)
p1 <- p1 + annotate("rect", xmin = 2002, xmax = 2014, ymin = 27500, ymax = 67500,
                    alpha = .1)
p1 <- p1 + annotate ("text", x = 2013, y = 29000, label = "NCLB", size = 5)
p1
path <- paste (getwd(), "figure", sep = "/")
ggsave (p1, filename = "Kentucky Eighth Grade Enrollments.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)

#diplomas
p1 <- ggplot(new, aes (Year, Diploma.KDE))
p1 <- p1 + geom_line(colour = "red")
p1 <- p1 + ylim (25000, 70000)
p1 <- p1 + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
p1 <- p1 + geom_line ( aes(Year, Diploma.NCES), colour = "black")
p1 <- p1 + xlab("Year") + ylab ("Students") + ggtitle ("Kentucky Diploma Recipients")
p1 <- p1 + annotate ("text", x = 2008, y = 44000, label = "CCD", size = 3)
p1 <- p1 + annotate ("text" , x = 2012, y = 39000, label = "KDE", colour = "red", size = 3)
p1 <- p1 + annotate("rect", xmin = 2002, xmax = 2014, ymin = 27500, ymax = 67500,
                    alpha = .1)
p1 <- p1 + annotate ("text", x = 2013, y = 29000, label = "NCLB", size = 5)
p1
path <- paste (getwd(), "figure", sep = "/")
ggsave (p1, filename = "Kentucky Diploma Recipients.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)

}     #Compare CCD to KDE
Fig.Poverty   <- function (){
library ("ggplot2")

#Figures
wd    <- getwd()
file  <- paste (wd, "objects", "cum.1999.2012.csv", sep = "/")
cum   <- read.csv (file, sep = ",", header = T, as.is = T)
file  <- paste (wd, "objects", "State.grad.rate.2002.2012.KDE.csv", sep = "/")
ky    <- read.csv (file, sep = ",", header = T, as.is = T)
file  <- paste (wd, "objects", "Nat.vs.KY.Gr.8.Cohort.2002.2009.csv", sep = "/")
nat   <- read.csv (file, sep = ",", header = T, as.is = T)
file  <- paste (wd, "objects", "CCD.vs.Kentucky.8.Cohort.csv", sep = "/")
ccd   <- read.csv (file, sep = ",", header = T, as.is = T)

#convert decile  columns to quantiles
quintile.1 <- c("D01", "D02")
quintile.2 <- c("D03", "D04")
quintile.3 <- c("D05", "D06")
quintile.4 <- c("D07", "D08")
quintile.5 <- c("D09", "D10")

#set column FRD from decile to quintile
cum [which (cum$FRD.Decile %in% quintile.1), 10] <- "Q01"
cum [which (cum$FRD.Decile %in% quintile.2), 10] <- "Q02"
cum [which (cum$FRD.Decile %in% quintile.3), 10] <- "Q03"
cum [which (cum$FRD.Decile %in% quintile.4), 10] <- "Q04"
cum [which (cum$FRD.Decile %in% quintile.5), 10] <- "Q05"

#set column School Povert from decile to quintile
cum[which (cum$SDCP.Decile  %in% quintile.1), 12] <- "Q01"
cum[which (cum$SDCP.Decile  %in% quintile.2), 12] <- "Q02"
cum[which (cum$SDCP.Decile  %in% quintile.3), 12] <- "Q03"
cum[which (cum$SDCP.Decile  %in% quintile.4), 12] <- "Q04"
cum[which (cum$SDCP.Decile  %in% quintile.5), 12] <- "Q05"
rm (list = ls(patter = "^qu"))

#set up 1st and 5 deciles (Quintile) for comparisons
FRD.Q01 <- subset (cum, cum$FRD.Decile == "Q01")
FRD.Q05 <- subset (cum, cum$FRD.Decile == "Q05")
SP.Q01 <- subset (cum, cum$SDCP.Decile == "Q01")
SP.Q05 <- subset (cum, cum$SDCP.Decile == "Q05")

#average for all schools
Gr.8.Cohort.All    <-  ccd$E.Gr.Ratio.KDE
names (Gr.8.Cohort.All) <- 1999:2013

#grads and eighth grade enrollments from FRD first quintile
grads.FRD.Q01   <- tapply (FRD.Q01$Diplo.KDE, INDEX = FRD.Q01$Year, sum)
e.gr.FRD.Q01    <- tapply (FRD.Q01$Gr08E.KDE, INDEX = FRD.Q01$Year, sum)

#grads and eighth grade enrollments from FRD fifth quintile
grads.FRD.Q05   <- tapply (FRD.Q05$Diplo.KDE, INDEX = FRD.Q05$Year, sum)
e.gr.FRD.Q05    <- tapply (FRD.Q05$Gr08E.KDE, INDEX = FRD.Q05$Year, sum)

#by school FRD
Gr.8.Cohort.by.School.FRD.Q01 <- tapply (FRD.Q01$Gr.8.Cohort.KDE, INDEX = FRD.Q01$Year, mean)
Gr.8.Cohort.by.School.FRD.Q05 <- tapply (FRD.Q05$Gr.8.Cohort.KDE, INDEX = FRD.Q05$Year, mean)
#by school SDCP
Gr.8.Cohort.by.School.SDCP.Q01 <- c(rep (NA, 4), tapply (SP.Q01$Gr.8.Cohort.KDE, INDEX = SP.Q01$Year, mean), NA)
Gr.8.Cohort.by.School.SDCP.Q05 <- c(rep (NA, 4), tapply (SP.Q05$Gr.8.Cohort.KDE, INDEX = SP.Q05$Year, mean), NA)

#combine
All <- cbind (Gr.8.Cohort.All, 
              grads.FRD.Q01, 
              e.gr.FRD.Q01, 
              grads.FRD.Q05, 
              e.gr.FRD.Q05,
              Gr.8.Cohort.by.School.FRD.Q01, 
              Gr.8.Cohort.by.School.FRD.Q05,
              Gr.8.Cohort.by.School.SDCP.Q01,
              Gr.8.Cohort.by.School.SDCP.Q05)
All <- as.data.frame (All)
All$Gr.8.Cohort.FRD.Q05 <- c (rep (NA,4), All$grads.FRD.Q05[5:14] / All$e.gr.FRD.Q05[1:10], NA)
All$Gr.8.Cohort.FRD.Q01 <- c (rep (NA,4), All$grads.FRD.Q01[5:14] / All$e.gr.FRD.Q01[1:10], NA)
All$Year <- 1999:2013

#plot 8th Grade Cohort at School level comparing first and fifth quintiles
p1 <- ggplot (All, aes (Year, Gr.8.Cohort.All))
p1 <- p1 + geom_line (colour = "red")
p1 <- p1 + xlab ("Year") + ylab ("Rate")
p1 <- p1 + ggtitle ("Eighth Grade Cohort Rate Disaggregated \n by Free and Reduced Lunch")
p1 <- p1 + ylim (c(.60, .95))
p1 <- p1 + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
p1 <- p1 + geom_line (aes(Year, Gr.8.Cohort.by.School.FRD.Q05))
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.by.School.FRD.Q01))
p1 <- p1 + annotate ("text", x = 2012.5, y = .76, label = "Q05", size = 3)
p1 <- p1 + annotate ("text", x = 2012.5, y = .89, label = "Q01", size = 3)
p1 <- p1 + annotate ("text", x = 2012.5, y = .82, label = "All", size = 3, colour = "red")
p1 <- p1 + annotate("rect", xmin = 2002, xmax = 2014, ymin = .625, ymax = .925,
                    alpha = .1)
p1 <- p1 + annotate ("text", x = 2013, y = .64, label = "NCLB", size = 5)
p1
path <- paste (getwd(), "figure", sep = "/")
ggsave (p1, filename = "Eighth Grade Cohort Rate Disaggregated by Free & Reduced Lunch.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)

#plot schools by school district poverty 
p1 <- ggplot (All, aes (Year, Gr.8.Cohort.All))
p1 <- p1 + geom_line (colour = "red")
p1 <- p1 + xlab("Year") + ylab ("Rate") + 
      ggtitle("Eighth Grade Cohort Rate Disaggregated \n by School District Child Poverty")
p1 <- p1 + ylim (c(.60, .95))
p1 <- p1 + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
p1 <- p1 + geom_line (aes(Year, Gr.8.Cohort.by.School.SDCP.Q01))
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.by.School.SDCP.Q05))
p1 <- p1 + annotate ("text", x = 2012.5, y = .76, label = "Q05", size = 3)
p1 <- p1 + annotate ("text", x = 2012.5, y = .89, label = "Q01", size = 3)
p1 <- p1 + annotate ("text", x = 2012.5, y = .82, label = "All", size = 3, colour = "red")
p1 <- p1 + annotate("rect", xmin = 2002, xmax = 2014, ymin = .625, ymax = .925,
                    alpha = .1)
p1 <- p1 + annotate ("text", x = 2013, y = .64, label = "NCLB", size = 5)
p1
path <- paste (getwd(), "figure", sep = "/")
ggsave (p1, filename = "Eighth Grade Cohort Rate Disaggregated by School District Poverty
        Level.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)

#Save as R object to load in later script
file <- paste (wd, "objects", "E.Gr.Cohort.by.Poverty.Quintile.csv", sep = "/")
write.csv (All, file = file)


}     #Chart School District Poverty
Fig.MISSING   <- function (){
#check absolute differences between 8th Graders and number of 
#diploma recipients
library (ggplot2)
wd <- getwd()
file <- paste (wd, "objects", "CCD.vs.Kentucky.8.Cohort.csv", sep = "/")
ccd  <- read.csv (file = file, sep = ",", as.is = T, header = T)


Gr8E <- ccd$E.Gr.Enrollment.KDE
names (Gr8E) <- 1999:2013
Dipl <- ccd$Diploma.KDE
names (Dipl) <- 1999:2013
missing <- cbind(Gr8E[1:10], Dipl[5:14])
missing <- as.data.frame (missing)
row.names (missing) <- 2003:2012
names (missing) <- c("8GrE.5.Yrs.Prior","Diplomas.w.i.4.years")
missing$gone <-missing[,1] - missing[,2] 
missing$year <- 2003:2012

#plot
p1 <- ggplot (missing, aes(year, gone), colour = "gray")
p1 <- p1 + geom_bar (stat = "identity", width = .75)
#p1 <- p1 + geom_bar (gone, colour = "red")
p1
p1 <- p1 + xlab("Year") + ylab("Missing Students") + ggtitle ("Difference in 8th Grade Enrollments 
                                                      Diploma Recipients")
p1 <- p1 + scale_x_continuous (limits = c(2003, 2012), breaks = seq(from = 2003, to = 2012, by = 2))
p1 <- p1 + ylim(0, 15000)
p1
path <- paste (getwd(), "figure", sep = "/")
ggsave (p1, filename = "Number of Missing Students by Year.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)


}     #Missing Students declined from 12000 to 9000
Fig.Reported  <- function (){
  wd <- getwd()
  file <- paste (wd, "objects", "State.grad.rate.2002.2012.KDE.csv", sep = "/")
  gr <- read.csv (file = file, sep = ",", header = T)
  file <- paste (wd, "objects", "CCD.vs.Kentucky.8.Cohort.csv", sep = "/")
  ccd <- read.csv (file = file, sep = ",", header = T) 
  
  Gr.8.Cohort.All    <-  ccd$E.Gr.Ratio.KDE
  names (Gr.8.Cohort.All)   <- 1999:2013
  Gr.8.Cohort.All    <- Gr.8.Cohort.All[4: (length (Gr.8.Cohort.All)-1)]
  Reported <- as.data.frame(cbind(gr, Gr.8.Cohort.All))
 
  
names (Reported)[(grep ("Reported", names (Reported)))] <- "Leaver" 
AFGR <- c(rep(NA, 5), Reported$Leaver[6:10], NA)
Cohort <- c(rep(NA, 10), Reported$Leaver[11])
Reported <- cbind(Reported[,1:3], AFGR, Cohort, Reported$Gr.8.Cohort.All)
Reported$Leaver <- c(Reported$Leaver[1:5], rep (NA, 6))

library (ggplot2)

  p <- ggplot (Reported, aes (Year, Gr.8.Cohort.All))
  p <- p + geom_line(colour = "red")
  p <- p + annotate ("text", x = 2012.5, y = .82, label = "All", size = 3, colour = "red")
  p <- p + ylim (.60, .95) + xlim (2001, 2015)
  p <- p + xlab("Year") + ylab ("Rate") + 
  ggtitle ("Eighth Grade Cohort Rate Compared to \n KDE Reported Graduation Rates")
  p <- p + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
  p <- p + geom_line (aes(Year, Leaver))
  p <- p + annotate ("text", x = 2003, y = .85, label = "Leaver", size = 3)
  p <- p + geom_line (aes(Year, AFGR))
  p <- p + annotate ("text", x = 2009.5, y = .75, label = "AFGR", size = 3)
  p <- p + geom_point (aes(Year, Cohort))
  p <- p + annotate ("text", x = 2011.5, y = .88, label = "Cohort", size = 3)
  p <- p + annotate("rect", xmin = 2002, xmax = 2014, ymin = .625, ymax = .925, alpha = .1)
  p <- p + annotate ("text", x = 2013, y = .64, label = "NCLB", size = 5)
  p
path <- paste (getwd(), "figure", sep = "/")
ggsave (p, filename = "KDE Reported Graduation Rates Versus Eighth Grade Cohort.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)
  
  
}     #Plot KY reported vs. KY 8th Grade cohort
Fig.Cohorts   <- function (){
  file = paste (getwd(), "objects", "cum.1999.2012.csv", sep = "/")
  cum  <- read.csv(file = file, as.is = T, header = T, sep = ",")
  E.Gr.C.KDE <- tapply (cum$Gr.8.Cohort.KDE, INDEX = cum$Year, mean)
  N.Gr.C.KDE <- tapply (cum$Gr.9.Cohort.KDE, INDEX = cum$Year, mean)
  b <- as.data.frame (cbind(E.Gr.C.KDE, N.Gr.C.KDE))
  b$Year <- as.integer (row.names (b))
  
  p <- ggplot (b, aes (Year, E.Gr.C.KDE))
  p <- p + geom_line(colour = "red")
  p <- p + annotate ("text", x = 2012.5, y = .81, label = "Eighth.All", size = 3, colour = "red")
  p <- p + ylim (.60, .95) + xlim (2001, 2015)
  p <- p + xlab("Year") + ylab ("Rate") + 
    ggtitle ("Eighth vs. Ninth Grade Cohort Rate")
  p <- p + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
  p <- p + geom_line (aes(Year, N.Gr.C.KDE))
  p <- p + annotate ("text", x = 2012.5, y = .75, label = "Ninth.All", size = 3)
  p <- p + annotate("rect", xmin = 2002, xmax = 2014, ymin = .625, ymax = .925, alpha = .1)
  p <- p + annotate ("text", x = 2013, y = .64, label = "NCLB", size = 5)
  p
  path <- paste (getwd(), "figure", sep = "/")
  ggsave (p, filename = "Eighth vs Ninth Grade Cohort Rate.pdf", 
          path = path,
          units = c("in"),
          height = 4, width = 6,
          dpi = 300)  
}     #compare 8th to 9th Grade KDE Cohort
Fig.Boxplot   <- function (){
file = paste (getwd(), "objects", "cum.1999.2012.csv", sep = "/")
cum  <- read.csv(file = file, as.is = T, header = T, sep = ",")
cum <- cum[-which (is.na (cum$FRD.Decile)), ]

p <- ggplot(cum, aes(FRD.Decile, Gr.8.Cohort.KDE), na.rm = T)
p <- p + geom_boxplot()
p <- p + ylim(.25,1.5)
p <- p + xlab ("Decile") + ylab("Rate")
p <- p + ggtitle ("Eighth Grade Cohort Disaggregated \n by Free and Reduced Lunch Decile")
p
path <- paste (getwd(), "figure", sep = "/")
ggsave (p, filename = "Eighth Gr Cohort by FRD Decile boxplot.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)  
cum <- cum[-which (is.na (cum$SDCP.Decile)), ]
p <- ggplot(cum, aes(SDCP.Decile, Gr.8.Cohort.KDE), na.rm = T)
p <- p + geom_boxplot()
p <- p + ylim(.25,1.5)
p <- p + xlab ("Decile") + ylab("Rate")
p <- p + ggtitle ("Eighth Grade Cohort Disaggregated \n by School District Child Poverty")
p
path <- paste (getwd(), "figure", sep = "/")
ggsave (p, filename = "Eighth Gr Cohort by SDCP Decile boxplot.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)  
}     #Grade 8 Cohort boxplots on poverty deciles
LM.8Gr.FRD    <- function (){
  #import
  wd <- getwd()
  file <- paste (wd, "objects", "cum.1999.2012.csv", sep = "/")
  cum <- read.csv (file = file, sep = ",", header = T, as.is = T)
  #model
  z <- lm(cum$Gr.8.Cohort.KDE ~ cum$FRDL.Pct)
  summary (z)
  #write out file
  wd <- getwd()
  file <- paste (wd, "objects", "lm.Grade.8.Cohort.to.FRD", sep = "/")
  save (z, file = file)
  
}     #Linear model 8 Gr. Cohort to Free Reduced Lunch
LM.8Gr.SDCP   <- function (){
  #import
  wd <- getwd()
  file <- paste (wd, "objects", "cum.1999.2012.csv", sep = "/")
  cum <- read.csv (file = file, sep = ",", header = T, as.is = T)
  #model
  z1 <- lm (cum$Gr.8.Cohort.KDE ~ cum$SDCP.Pct)
  summary (z1)
  #write out file
  wd <- getwd()
  file <- paste (wd, "objects", "lm.Grade.8.Cohort.to.SDCP", sep = "/")
  save (z1, file = file)
  
}     #Linear model 8 Gr. Cohort to SDCP
}     #This program combines data sets from the NCES, KDE,
                                      #U.S. Census to compute a graduation rate for Kentucky
                                      #students from 2004 to 2013.  While a number of metrics 
                                      #are computed, focus is on the eighth grade cohort.  It
                                      #is computed by dividing the number of regular diplomas
                                      #awarded within 4 years divided by the number of 
                                      #eighth grade students five years prior.  It is 
                                      #disaggregated at the school district level and by
                                      #child poverty.