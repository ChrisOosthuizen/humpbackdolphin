

library(marked)

# Load encounter histories from Conry Msc (humpback dolphins) and inspect
# There is no column of '1's at the end
dat = read.table("./data/multimark_input.txt", header = T)

# process to Rmark format
names(dat)
dat = data.frame(paste0(
            dat$occ1, dat$occ2, dat$occ3,
            dat$occ4, dat$occ5, dat$occ6,
            dat$occ7, dat$occ8, dat$occ9,
            dat$occ10, dat$occ11, dat$occ12,
            dat$occ13, dat$occ14, dat$occ15,
            dat$occ16))
    
head(dat)
dat$freq = 1
names(dat) = c("ch", "freq")
head(dat)


# First, process data 
js.proc <- process.data(dat, model = "JS")

# Second, make design data (from processed data)
js.ddl <- make.design.data(js.proc)

