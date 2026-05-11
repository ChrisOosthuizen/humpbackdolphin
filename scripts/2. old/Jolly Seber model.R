
library(RMark)

# Example: assume 'dolphins' is your dataframe with capture histories
# Format should have at least "ch" (capture history) column

# dolphins <- data.frame(
#   ch = c("1001", "0101", "0010", "1110", "0001"),
#   stringsAsFactors = FALSE
# )
# 
# dolphins

dolphins <- read.csv("data/2002_data.csv")
#dolphins <- read.csv("data/2002_data_actual_dates_wrong.csv")


head(dolphins)

# Collapse columns occ1...occ4 into a single string
dolphins$ch <- apply(dolphins[ , grep("occ", names(dolphins))], 1, paste0, collapse = "")

# Keep only what you need (id optional, ch required)
# dolphins <- dolphins[, c("ch")]
# head(dolphins)


# Process data for POPAN model
proc.data <- process.data(dolphins, model="POPAN")

# Create design data
ddl <- make.design.data(proc.data)

# Model 1: p constant
model.constp_time_pent <- mark(proc.data, ddl,
                     model.parameters = list(
                       Phi = list(formula = ~1),       # survival constant
                       p   = list(formula = ~1),       # detection constant
                       pent= list(formula = ~time),    # entry time-dependent
                       N   = list(formula = ~1)        # abundance constant
                     ),
                     silent=TRUE)

# Model 2: p constant
model.constp_const_pent <- mark(proc.data, ddl,
                               model.parameters = list(
                                 Phi = list(formula = ~1),       # survival constant
                                 p   = list(formula = ~1),       # detection constant
                                 pent= list(formula = ~1),    # entry time-dependent
                                 N   = list(formula = ~1)        # abundance constant
                               ),
                               silent=TRUE)

# Model 3: p time dependent
model.timep_const_pent <- mark(proc.data, ddl,
                    model.parameters = list(
                      Phi = list(formula = ~1),       # survival constant
                      p   = list(formula = ~time),   # detection time-dependent
                      pent= list(formula = ~1),
                      N   = list(formula = ~1)
                    ),
                    silent=TRUE)

# Model 2: p time dependent
model.timep_time_pent <- mark(proc.data, ddl,
                    model.parameters = list(
                      Phi = list(formula = ~1),       # survival constant
                      p   = list(formula = ~time),   # detection time-dependent
                      pent= list(formula = ~time),
                      N   = list(formula = ~1)
                    ),
                    silent=TRUE)


# Compare models with AIC
model.table <- collect.models()
model.table

# Get estimates for abundance (N)
summary(model.timep_const_pent)$reals$N

model.timep_const_pent$results$derived

model.timep_time_pent$results$derived

