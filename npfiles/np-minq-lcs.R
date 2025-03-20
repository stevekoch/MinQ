
library(tidyverse)
library(np)
options(np.messages=TRUE,
        scipen = 999)

# not working - sometimes, there is a google drive issue...
# This data developed in the main paper draft... 
load("npfiles/lcs2014.RData")


## step 0 - Any correlation issues?
df <- lcs2014 

## Vectorise for np package?
## Here we are using factors and ordered
df$lnminq <- as.vector(df$lnminq)
df$age <- as.vector(df$age)


# Subsampling to see that the program runs
#ss <- sample(dim(df)[1], 500)
#dfss <- df[ss,]


bw.lcs <- npregbw(lnminq ~ lny + kids + adults + age + hhh.male + 
                 ethnic + marital + education + province + settle + month, 
               regtype="ll",
               ckertype="epanechnikov",
               ukertype="liracine",
               okertype="liracine",
               data = df,          #dfss
               bwmethod = "cv.ls", # cv.aic?
               nmulti=20) ##20

sreg.lcs <- npreg(bw=bw.lcs,resid=TRUE, data=df, newdata=df)
uhat.lcs <- residuals(sreg.lcs)
lnminq.hat.lcs = fitted(sreg.lcs)

lcs.npdata <- df %>%
  add_column(uhat.lcs, lnminq.hat.lcs)

save(bw.lcs, sreg.lcs, lcs.npdata,
     file="lcs-np.Rdata")




                       