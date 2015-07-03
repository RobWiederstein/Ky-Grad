#get graduate data for years 2003 - 2007 ----
get_grads_2003_2007 <- function(){
    #Grad Rate 2003-2007 was sent via email from Kentucky Department of Education
    #No longer available on website. Disaggregated by school district
    #import
    wd <- getwd()
    file <- paste (wd, "data_raw", "Grad Rate 2003-2007.csv", sep = "/")
    gr <- read.csv (file = file, sep = ",", header = TRUE,
                    strip.white = TRUE,
                    colClasses = "character",
                    stringsAsFactors = F)
    
    #subset cols to grads only
    gr <- gr [, c(4, 5, grep ("GRADS_4YR", names (gr)))]
    gr <- gr [gr$SCHNAME == "---DISTRICT TOTAL---", ]
    gr <- gr [, -2]
    
    #wide to long
    names(gr)[1]   <- "DISTRICT"
    library(reshape)
    gr <- melt.data.frame(gr, id.vars = "DISTRICT", variable_name = "variable")
    num <- grep ("Walton-Verona Independent", gr$DISTRICT)
    gr$DISTRICT[num] <- "Walton Verona Independent"
    
    #create Year column
    library(stringr)
    a <- str_split(gr$variable, pattern = "_")
    gr$Year <- paste("20", sapply(a, "[", 3), sep = "")
    gr <- gr[, c("Year", "DISTRICT", "variable", "value")]
    gr
}

#get graduate data for years 2008 - 2012----
get_grads_2008_2012 <- function(){
#AFGR data is for 2008-2012.
#David Curd with KDE advised that AFGR data is lagged one year.
#2012 data would be for 2010-11 School Year
#KDE's naming convention is to refer to the second year of the 2010-11 school year.
    wd <- getwd()
    AFGR <- read.csv(file = paste (wd, "data_raw", "AFGR_2012.csv", sep = "/"), 
                     sep = ",", 
                     header = TRUE,
                     strip.white = TRUE,
                     colClasses = "character")

    df <- subset(AFGR, 
                 subset = (Gender == 'Total' & 
                           Ethnicity == 'Total' &
                           School.Name == 'DISTRICT TOTAL'), 
                 select = c(School.Year, 
                            District.Name,
                            Graduates.with.Diploma.in.4.years)
                 )
    #rename vars
    names(df)[2] <- "DISTRICT"
    names(df)[1] <- "Year"
    #sync district names to saar index
    df$DISTRICT[grep("Jefferson County Public School", df$DISTRICT)] <- "Jefferson County"
    df$DISTRICT[grep("Larue County", df$DISTRICT)] <- "LaRue County"
    df$DISTRICT[grep("Mccracken County", df$DISTRICT)] <- "McCracken County"
    df$DISTRICT[grep("Mccreary County", df$DISTRICT)] <- "McCreary County"
    df$DISTRICT[grep("Raceland-Worthington Independe", df$DISTRICT)] <- "Raceland Independent"
    df$DISTRICT[grep("Walton-Verona Independent", df$DISTRICT)] <- "Walton Verona Independent"

    #reshape
    df <- melt.data.frame(df, id.vars = c("Year", "DISTRICT"))
    df
}

#get graduate data for year 2013----
get_grads_2013 <- function(){
    #http://applications.education.ky.gov/SRC/DataSets.aspx
    #2012-2013 Diploma Recipients w/i 4 years
    #import
    wd <- getwd()
    file <- paste (wd, "data_raw", "ACCOUNTABILITY_GRADUATION_RATE_COHORT.csv", sep = "/")
    agrc <- read.csv (file, header = T, sep = ",", as.is = T,
                      strip.white = T)
    
    #subset
    agrc <- subset(agrc, subset = c(DISAGG_ORDER == "0" &
                                 SCH_NAME == "--District Total--"
                                 ),
                       select = c(SCH_YEAR, DIST_NAME, COHORT_NUMERATOR)
                   )
    
    #rename vars
    names (agrc)[2] <- "DISTRICT"
    names(agrc)[1] <- "Year"
    
    #match names to SAAR index
    agrc$DISTRICT[grep("Raceland-Worthington Independent", agrc$DISTRICT)] <- "Raceland Independent"
    agrc$DISTRICT[grep("Walton-Verona Independent", agrc$DISTRICT)] <- "Walton Verona Independent"
    agrc$Year <- substr(agrc$Year, start = 5, stop = 8)
    
    #long to wide
    agrc <- melt.data.frame(agrc, id.vars = c("Year", "DISTRICT"))

    #setdiff (saar$DISTRICT, agrc$DISTRICT)  7 schools missing
    #Save as R object to load in later script
    agrc
}

#get graduate data for year 2014----
get_grads_2014 <- function(){
    #import grad data from excel data sheet
    wd <- getwd()
    file <- paste (wd, "data_raw", "ACCOUNTABILITY_CCR_HIGHSCHOOL.xlsx", sep = "/")
    library(xlsx)
    agrc14 <- read.xlsx2(file = file, sheetIndex = 1, colClasses = "character",
                         stringsAsFactors = F)
    
    #import saar data
    file <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
    saar <- read.csv (file, header = T, sep = ",", as.is = T, strip.white = T)
    index <- data.frame (saar$DISTRICT)
    
    #subset
    agrc14 <- subset(agrc14, subset = c(SCH_YEAR == "20132014" &
                                   DISAGG_ORDER == "0" &
                                   SCH_NAME == "---District Total---"),
                        select = c(SCH_YEAR, DIST_NAME,
                                   NBR_GRADUATES_WITH_DIPLOMA))
    
    #name vars
    names (agrc14)[1:2] <- c("Year", "DISTRICT")
    
    #fix year
    agrc14$Year <- substr(agrc14$Year, start = 5, stop = 8)
    
    
    
    #fix dist names to match saar
    num <- grep("Raceland-Worthington Independent", agrc14$DISTRICT)
    agrc14$DISTRICT[num] <- "Raceland Independent"
    num <- grep("Walton-Verona Independent", agrc14$DISTRICT)
    agrc14$DISTRICT[num] <- "Walton Verona Independent"
    
    # #7 schools missing: "Anchorage Independent"      "East Bernstadt Independent" "Harrodsburg Independent"   
    #"Monticello Independent"     "Providence Independent"     "Science Hill Independent"  
    #"Southgate Independent"      "West Point Independent"  
    #Save as R object to load in later script
    
    #wide to long
    agrc14 <- melt.data.frame(agrc14, id.vars = c("Year", "DISTRICT"))
    agrc14
}

#combine different sets----
merge_grads_all <- function(){
    gr_03_07    <- get_grads_2003_2007()
    gr_08_12    <- get_grads_2008_2012()
    gr_13       <- get_grads_2013()
    gr_14       <- get_grads_2014()
    df          <- rbind(gr_03_07,
                         gr_08_12,
                         gr_13,
                         gr_14
    )
    #have commas!!!
    df$value <- gsub(",", "", df$value)
    df$value <- as.numeric(as.character(df$value))
    df[821, 4] <- 0  #Providence?
    df
}

#write it out into R object ----
write_GRADS_ALL <- function(){
    df <- merge_grads_all()
    #write out r object to "objects" directory
    wd <- getwd()
    dir <- "objects"
    file <- "grads_2003_2014"
    myfile <- paste(wd, dir, file, sep = "/")
    save(df, file = myfile)
}
