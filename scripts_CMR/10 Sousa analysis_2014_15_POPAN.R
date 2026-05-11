
# Chris Oosthuizen

# SOusa analysis with POPAN for 2014-15 EHM data

# 20 Feb 2026

#--------------------------------------------------------------------
# Load data
#--------------------------------------------------------------------
# Load encounter histories from Conry Msc (humpback dolphins) and inspect
# There is no column of '1's at the end
CH = read.table("./data/multimark_input.txt", header = T)
CH = data.matrix(CH)
head(CH)
colnames(CH) <- NULL
head(CH)

rowSums(CH)
table(rowSums(CH))  # 25 transients
dim(CH)    # number if individuals


#------------------------------------
# RMARK
#------------------------------------

library(RMark)

# Convert each row to a character string
ch <- apply(CH, 1, paste0, collapse = "")

ch.tab <- table(ch)

mark.data <- data.frame(
  ch   = names(ch.tab),
  freq = as.numeric(ch.tab)
)


## process data 

proc <- process.data(mark.data, model = "POPAN")
ddl  <- make.design.data(proc)

model <- mark(
  proc,
  ddl,
  model.parameters = list(
    Phi   = list(formula = ~1),
    p     = list(formula = ~time),
    pent  = list(formula = ~time),
    N     = list(formula = ~1)
  ),
  delete = T
)

# There should be 1000 + 50 = 1050 individuals in superpopulation.

model$results$real



