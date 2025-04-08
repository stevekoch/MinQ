
library(tidyverse)
library(np)
options(np.messages=TRUE,
        scipen = 999)

# not working - sometimes, there is a google drive issue...
# This data developed in the main paper draft... 
# Does not seem to load, but can worry about that later
load(file = "/ghsdata/ghs2023.Rdata")


## step 0 - Any correlation issues?
df <- ghs2023 %>%
  zap_labels() %>%
  as.data.frame

## Vectorise for np package?
## Here we are using factors and ordered
df$lnminq <- as.vector(df$lnminq)
df$age <- as.vector(df$age)


# Subsampling to see that the program runs
#ss <- sample(dim(df)[1], 500)
#dfss <- df[ss,]


bw.ghs <- npregbw(lnminq ~ lny + kids + adults + age + hhh.male + 
                 ethnic + marital + education + province + settle + month, 
               regtype="ll",
               ckertype="epanechnikov",
               ukertype="liracine",
               okertype="liracine",
               data = df,          #dfss
               bwmethod = "cv.ls", # cv.aic?
               nmulti=15) ##20

sreg.ghs <- npreg(bw=bw.ghs,resid=TRUE, data=df, newdata=df)
uhat.ghs <- residuals(sreg.ghs)
lnminq.hat.ghs = fitted(sreg.ghs)

ghs.npdata <- df %>%
  add_column(uhat.ghs, lnminq.hat.ghs)

save(bw.ghs, sreg.ghs, ghs.npdata,
     file="ghs-np.Rdata")




                       