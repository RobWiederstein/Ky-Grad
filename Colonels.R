wd <- getwd()
file <- paste (wd, "objects", "cum.2003.2012.csv", sep = "/")
cum <- read.csv (file, as.is = T, header = T)

attach (cum)
h <- cum[DISTRICT=="Henderson County", c(1, 2, 9)]
ky <- cum [School.Dist.Child.Poverty.Decile == "D03", ]
detach (cum)
kyD03 <- tapply (cum$Gr.8.Cohort.KDE, INDEX = cum$Year, mean, simply = T)
kyD03 <- kyD03[10:1]
h <- cbind (h, kyD03)

library (ggplot2)
p1 <- ggplot (h, aes(Year, Gr.8.Cohort.KDE))
p1 <- p1 + geom_line (colour = "red")
p1 <- p1 + scale_x_continuous (labels = 2003:2012, breaks = 2003:2012)
p1 <- p1 + ylim (.6, .95)
p1 <- p1 + geom_line (aes(Year, kyD03))
p1 <- p1 + xlab("") + ylab("Graduation Rate") +ggtitle("Henderson vs. Kentucky")
p1 <- p1 + annotate("text", x = 2011, y = .9, label = "Colonels", colour = "red", size = 4)
p1 <- p1 + annotate("text", x = 2011, y = .83, label = "KY (D03)", colour = "black", size = 4)
p1
