library (ggplot2)
file  <- paste (getwd(), "objects", "cum.1999.2012.csv", sep = "/")
cum   <- read.csv (file, sep = ",", header = T, as.is = T)
grad  <- cum[cum$Year == "2012", c(2,3,8)]
plot (density(grad$Diplo.KDE))
a <- grad[, rev(sort(grad$Diplo.KDE))]
a <- grad[grad$Diplo.KDE, ]

p <- ggplot(grad, aes(Diplo.KDE))
p <- p + geom_histogram(binwidth = .1)
p <- p + xlab("Diplomas \n (log10 scale)")
p <- p + ylab("School Districts")
p <- p + ggtitle ("Number of Diplomas Issued by School Districts 2012\n (N=168)")
p <- p + scale_x_log10()                       
p
path <- paste (getwd(), "figure", sep = "/")
ggsave (p, filename = "Number of Diplomas Issued by School Districts 2012.pdf", 
        path = path,
        units = c("in"),
        height = 4, width = 6,
        dpi = 300)


)
grad[rev(order(grad$Diplo.KDE)),]
grad$Decile <- cut_number(grad$Diplo.KDE, n = 10)
levels (grad$Decile) <- c(paste("D", "0", 1:9, sep = ""), "D10")
tapply (grad$Diplo.KDE, INDEX = grad$Decile, FUN = mean)
median (grad$Diplo.KDE)
mean (grad$Diplo.KDE)
table (grad$Decile)
