# set working directory
dir <- "D:/workspace/FINA5250/Assignment1"
setwd(dir)
# load data
DHSI <- read.table("DHSI.csv", header = TRUE, sep = ",")
# extract date
HSI_date <- strptime(DHSI$Date, format = "%Y/%m/%d")
# extract daily close price
HSI <- DHSI$Close
# compute log returns
DHSILR <- diff(log(HSI))

# create output file
title <- "assg1"
# author <- "Gerald_Liu"

pdf(paste(title, ".pdf", sep = ""))

# draw time series plots of the index “HSI” and the log returns “DHSILR”
par(mfrow = c(2, 1))
plot(HSI_date, HSI, type = "l", xlab = "Date",
     main = "Daily HSI index from Jan 1987 to Aug 2023")
# drop 1st element
plot(HSI_date[-1], DHSILR, type = "l", xlab = "Date",
     main = "Daily log return of HSI from Jan 1987 to Aug 2023")

# draw histograms of DHSILR with bin sizes = 20, 50, 500 and 5000
par(mfrow = c(2, 2))
hist(DHSILR, breaks = 20, freq = FALSE, col = NULL,
     main = "Histogram of DSPLR, #bins = 20")
hist(DHSILR, breaks = 100, freq = FALSE, col = NULL,
     main = "Histogram of DSPLR, #bins = 100")
hist(DHSILR, breaks = 500, freq = FALSE, col = NULL,
     main = "Histogram of DSPLR, #bins = 500")
hist(DHSILR, breaks = 5000, freq = FALSE, col = NULL,
     main = "Histogram of DSPLR, #bins = 5000")

# draw histogram of DHSILR with bin size = 100
par(mfrow = c(1, 1))
hist(DHSILR, breaks = 100, freq = FALSE, col = NULL,
     main = "Histogram of DHSILR vs Fitted Normal Density")
# superimpose the fitted normal density curve
curve(dnorm(x, mean = mean(DHSILR), sd = sd(DHSILR)), col = "red", add = TRUE)

# close output file
dev.off()

# save as txt
src_filename <- "assg1.r"
# out_filename <- paste(title, "_code_", author, ".txt", sep = "")
out_filename <- paste(title, ".txt", sep = "")
file.copy(src_filename, out_filename)
