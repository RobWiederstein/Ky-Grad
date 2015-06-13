get_grads_2003_2007 <- function(){
    #Grad Rate 2003-2007 was sent via email from Kentucky Department of Education
    #No longer available on website. Disaggregated by school district
    #import
    wd <- getwd()
    file <- paste (wd, "data_raw", "Grad Rate 2003-2007.csv", sep = "/")
    gr <- read.csv (file = file, sep = ",", header = TRUE,
                    strip.white = TRUE,
                    colClasses = "character")
    
    #subset cols to grads only
    gr <- gr [, c(4, 5, grep ("GRADS_4YR", names (gr)))]
    gr <- gr [gr$SCHNAME == "---DISTRICT TOTAL---", ]
    gr <- gr [, -2]
    
    #rename vars
    names(gr)[2:6] <- paste ("Grad.w.Diploma.in.4.years", 2002:2006, "KDE", sep = ".")
    names(gr)[1]   <- "DISTRICT"
    gr[grep ("Walton", gr$DISTRICT),1] <- "Walton Verona Independent"
    gr
}
xyz <- get_grads_2003_2007()



get_grads_2007_2011 <- function(){
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
    
    #Diploma Recipients from KDE (DR.**) by year.  Because 1 yr lag, 2012 is for 2010-2011 school year.
    #Code uses first year of school year, contrary to KDE naming conventions
    df$Year[which(df$Year == 2012)] <- "2011a"
    df$Year[which(df$Year == "2011")] <- "2010a"
    df$Year[which(df$Year == "2010")] <- "2009a"
    df$Year[which(df$Year == "2009")] <- "2008a"
    df$Year[which(df$Year == "2008")] <- "2007a"
    df$Year <- gsub("a", "", df$Year)
    df$Year <- as.integer(df$Year)
    
    #Save as R object to load in later script
    wd <- getwd()
    path <- "objects"
    file <- "afgr.07.11"    
    file  <- paste (wd, path, file, sep = "/")
    save(df, file = file)
    df
}

zzz <- get_grads_2007_2011()
