
# choose 1
df <- lcs2014 
df <- ghs2023

contin <- lm(lnminq ~ lny + adults + kids +  age + age.sq + 
               hhh.male + ethnic + province + settle + 
               marital + education + month + grants,
             data = df)

b1 <- summary(contin)$coefficients[,1]

contin.iv <- hetErrorsIV(lnminq ~ lny + adults + kids +  age + age.sq + 
                           hhh.male + ethnic + province + settle + 
                           marital + education + month + grants
                         | lny | 
                           IIV(age,grants),
                         data=df, 
                         verbose = FALSE)


#cbind(coef(contin)[c(1:4,49)],
#      coef(contin.iv)[c(1:4,49)], 
#      coef(contin.iv.external)[c(1:4,49)])

# just to calculate the spls for the endogeneity corrected outcomes...
# chooose one
model <- contin
model <- contin.iv
model <- contin.iv.external

b <- coef(model)[1:4] # does not count non-income and non-adult/child vars.
b1 <- b[3:4]          # just the kids and adult bit
denom <- 1-b[2]

dfdiff <- akmat %>% dplyr::select(ao,ko) %>% 
  mutate(ao = as.numeric(ao),
         ko = as.numeric(ko)) %>%
  as.matrix()

spl.base <-  exp( (b[1] + (dfdiff %*% b1)) * (1/denom) )

spl.lin.yall <- akmat %>%
  dplyr::select(ao,ko) %>%
  mutate(adults = as.integer(ao), kids = as.integer(ko)-1) %>%
  add_column(spl.linear = as.integer(spl.base)) 

spl.lin.yall.table <- spl.lin.yall %>%
  rename("spl" = "spl.linear") %>%
  dplyr::select(-c(ao,ko))
