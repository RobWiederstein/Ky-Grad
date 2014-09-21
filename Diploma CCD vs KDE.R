wd <- getwd()
file <- paste (wd, "data sets", "Grad Rate 2003-2007.csv", sep = "/")
gr <- read.csv (file = file, sep = ",", header = TRUE,
                strip.white = TRUE,
                colClasses = "character")
AFGR <- read.csv(file = paste (wd, "data sets", "AFGR_2012.csv", sep = "/"), 
                 sep = ",", 
                 header = TRUE,
                 strip.white = TRUE,
                 colClasses = "character")

wd <- getwd()
file <- paste (wd, "data sets", "ACCOUNTABILITY_GRADUATION_RATE_COHORT.csv", sep = "/")
agrc <- read.csv (file, header = T, sep = ",", as.is = T,
                  strip.white = T)

agrc <- agrc [agrc$DISAGG_ORDER == "0" & agrc$SCH_NAME == "--District Total--",]


attach (AFGR)


#Diploma Recipients from KDE (DR.**) by year.  Because 1 yr lag, 2012 is for 2010-2011 school year.
#Code uses first year of school year, contrary to KDE naming conventions
DR.07 <- AFGR[which(Gender == 'Total' & 
                     Ethnicity == 'Total' & 
                     School.Name == 'DISTRICT TOTAL' & 
                     School.Year == '2008'), 
               ]
DR.08 <- AFGR[which(Gender== 'Total' & 
                      Ethnicity == 'Total' &
                      School.Name == 'DISTRICT TOTAL' & 
                      School.Year == '2009'),
              ]

DR.09 <- AFGR[ which(Gender == 'Total' & 
                     Ethnicity == 'Total' & 
                     School.Name == 'DISTRICT TOTAL' &
                     School.Year == '2010'),
               ]

DR.10 <- AFGR[which(Gender == 'Total' &
                      Ethnicity == 'Total' & 
                      School.Name  == 'DISTRICT TOTAL' & 
                      School.Year == '2011'),
              ]
DR.11 <- AFGR[which(Gender == 'Total' & 
                      Ethnicity == 'Total' & 
                      School.Name == 'DISTRICT TOTAL' &
                      School.Year == '2012'),
              ]   
rm (AFGR)
detach (AFGR)

DR.11 <- DR.11[, c(5, 10, 11, 12, 13, 14, 15:19)]
DR.11 <- DR.11[, c(1:9)]
DR.11[2: ncol (DR.11)] <- sapply (DR.11[2: ncol(DR.11)], as.integer)
colSums (DR.11[2: ncol(DR.11)])
