# Four models are fitted, the JSSA model and simplifications
# with phi and/or p constant.

# Load the functions and the data set:
source("hetage.functions.R")
dyn.load("./hetageLL.dll")

dat = read.table("multimark_input.txt", header = T)
head(dat)

# Check out the appearance of the data set:
str(dat)
dim(dat)        
# Any leading or trailing zeros? (check if first or last column sum = 0)
# If so, remove those columns.
apply(dat,2,sum)   # 2 refers to columns (see ?apply). So this sums the EHM over columns

# There is no column labelled "freq", so it is assumed
# that all frequencies are 1. In this case, data will be 
# condensed if there are repeated rows.

hetage.process.data(dat)

# View the working data:
x.mat         # Only 50 distinct capture histories
y.vect        # Frequencies of the capture histories
sum(y.vect)   # Should be nr of animals - same as dim (dat).


# Option 1: fit the four models separately:
# -----------------------------------------
# phic.pc.out <- hetage.fit.model("phic.pc")
# phic.pt.out <- hetage.fit.model("phic.pt")
# phit.pc.out <- hetage.fit.model("phit.pc")
# phit.pt.out <- hetage.fit.model("phit.pt")
# 
# phic.pc.out   # To inspect the output.


# Option 2: set up a list of model fits.
# --------------------------------------
# Name the models:
model.names <- c("phic.pc","phic.pt","phic.ph","phic.pth",
                 #"phit.pc","phit.pt","phit.ph","phit.pth",
                 "phih.pc","phih.pt","phih.ph","phih.pth" ) #,
                 #"phith.pc","phith.pt","phith.ph","phith.pth")

no.models   <- length(model.names)
model.fits  <- vector("list",no.models)
names(model.fits) <- model.names
#ngroups <- c(1,1,2,2,1,1,rep(2,10))
ngroups <- c(1,1,2,2, 2, 2, 2, 2)

# Fit the models:
for (modno in 1:no.models) {
   model.fits[[modno]] <- hetage.fit.model(model.names[modno],G=ngroups[modno])
   print(paste("Model",model.names[modno],"fitted"))
   }

hetage.summary(model.fits,AICorder=T)  # for a detailed summary table sorted by AIC

#---------------------------------------------------------------
# Construct a summary table:
summary.table <- matrix(NA,no.models, 8)

dimnames(summary.table) <- list(model.names,
   c("MaxLL","RD","npar","AIC","relAIC","AICc","relAICc", "Nhat"))

# there is no 5 and 7 coumns here - they are inserted below:
for (modno in 1:no.models) if (!is.null(model.fits[[modno]]))  {
   summary.table[modno,1] <- model.fits[[modno]]$maxLL
   summary.table[modno,2] <- model.fits[[modno]]$RD
   summary.table[modno,3] <- model.fits[[modno]]$npar
   summary.table[modno,4] <- model.fits[[modno]]$AIC
   summary.table[modno,6] <- model.fits[[modno]]$AICc
   summary.table[modno,8] <- model.fits[[modno]]$parameters$N
   }

summary.table[,5] <- summary.table[,4] - min(summary.table[,4],na.rm=T)
summary.table[,7] <- summary.table[,6] - min(summary.table[,6],na.rm=T)

print(round(summary.table,2))

# Save the results:
corm4models.out <- model.fits

phic.pt <- hetage.fit.model("phic.pt")

d <- model.fits[[phic.pt]]$parameters$N

   
phic.pt$parameters$N
phic.pt$parameters$phi
phic.pt$parameters$p
phic.pt$parameters$pi

# you would have to get SE or CI from here, I think 
phic.pt$VC

