

#http://www.phidot.org/forum/viewtopic.php?f=1&t=2528&p=8018&hilit=coding+.+missing+occasion

#For closed abundance models, unequal intervals between sampling occasions makes no difference to estimation of primary parameters (N).
#Thus, equal capture occasions are largely redundant in the assumptions.

# Here, I show that adding 0 capture occasions (missing months) have very little influence on N


library(Rmark)

# data from Rmark
data(edwards.eberhardt)
edwards.eberhardt
dim(edwards.eberhardt)
class(edwards.eberhardt)
head(edwards.eberhardt)
str(edwards.eberhardt$ch)

#split starting at the first and ending at the second

nchar(edwards.eberhardt$ch)  # 18 capture occasions

# split the capture history into (random) sections.
# below, you can then add 0's into the 'splits' to test if the 0 occasions (missing occasions) influence N

s1 <- substr(edwards.eberhardt$ch,1,5)
s2 <- substr(edwards.eberhardt$ch,6,10)
s3 <- substr(edwards.eberhardt$ch,11,15)
s4 <- substr(edwards.eberhardt$ch,16,18)

ch = paste0(s1,"", s2,"", s3,"", s4) # this generate the original edwards.eberhardt again
ch = paste0(s1,"0", s2,"00", s3,"000", s4)  # this generate the edwards.eberhardt data with missing occasions
ch = data.frame(ch)
ch
class(edwards.eberhardt)
class(ch)
nchar(ch$ch)

edwards.eberhardt = ch


# Results
# 97.15929 # edwards.eberhardt original
# 97.15929 # edwards.eberhardt reconstituted 
# 97.62051 # with 3 missing occasions 
# 98.15431 # with more than 3 missing occasions

#
# create function that defines and runs the analyses as defined in MARK example dbf file
#
run.edwards.eberhardt=function()
{
  #
  # Define parameter models
  #
  pdotshared=list(formula=~1,share=TRUE)
  ptimeshared=list(formula=~time,share=TRUE)
  ptime.c=list(formula=~time+c,share=TRUE)
  ptimemixtureshared=list(formula=~time+mixture,share=TRUE)
  pmixture=list(formula=~mixture)
  #
  #
  # Huggins models
  #
  # p=c constant over time
  ee.huggins.m0 =mark(edwards.eberhardt,model="Huggins",model.parameters=list(p=pdotshared))
  # p constant c constant but different; this is default model for Huggins
  ee.huggins.m0.c =mark(edwards.eberhardt,model="Huggins")
  # Huggins Mt
  ee.huggins.Mt =mark(edwards.eberhardt,model="Huggins",model.parameters=list(p=ptimeshared),adjust=TRUE)
  #
  # Huggins heterogeneity models
  #
  # Mh2 - p different for mixture
  ee.huggins.Mh2 =mark(edwards.eberhardt,model="HugHet",model.parameters=list(p=pmixture))
  # Huggins Mth2 - p different for time; mixture additive
  ee.huggins.Mth2.additive =mark(edwards.eberhardt,model="HugFullHet",model.parameters=list(p=ptimemixtureshared),adjust=TRUE)
  #
  # Return model table and list of models
  #
  return(collect.models() )
}
#
# fit models in mark by calling function created above
#
ee.results=run.edwards.eberhardt()

ee.results$ee.huggins.Mth2.additive$results
ee.results[[1]]$results

