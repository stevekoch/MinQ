


##############################################Final Code For thesis
### Load all Packages  ###

library(haven)
library(gtsummary)
library(tidyverse)
library(dplyr)
library(knitr)
library(stargazer)
library(quantreg)
library(dplyr)
library(tidyr)
library(scales)  
library(pastecs)
library(psych)
library(knitr)
library(kableExtra)

library(broom)
library(ggplot2)

library(purrr)
library(psych)
library(flextable)
library(modelsummary)
library(fixest)
library(stringr)

library(pandoc)


##################### Next Import Data Files and Merge Data ###################

#import dataset#
options(scipen = 999)
Persons_df <- read_dta("..\data\lcs-2014-2015-persons-final-v1.dta")
Total_df <- read_dta("..\data\lcs-2014-2015-total-s11-v1.dta")
Households_df <- read_dta("..\data\lcs-2014-2015-households-v1.dta")

# hhsize, number of kids (age < 15) in each household
person1 <- Persons_df %>%
  group_by(UQNO) %>%
  summarise(hhsize = n(), # household size by appearance times of each ID
            kids = sum(Q14AGE < 15), # number of child (age<15)
            adults = hhsize - kids
  ) %>%
  mutate(UQNO = as.character(UQNO))

Adata <- Persons_df %>%
  dplyr::select(UQNO, PERSONNO, 
                province_code, SETTLEMENT_TYPE) %>%
  filter(PERSONNO==1
  ) %>%
  transmute(UQNO = as.character(UQNO),
            WC = ifelse(province_code==1,1,0),
            EC = ifelse(province_code==2,1,0),
            NC = ifelse(province_code==3,1,0),
            FS = ifelse(province_code==4,1,0),
            KZN = ifelse(province_code==5,1,0),
            NW = ifelse(province_code==6,1,0),
            GP = ifelse(province_code==7,1,0),
            MP = ifelse(province_code==8,1,0),
            LP = ifelse(province_code==9,1,0),
            province = as_factor(province_code),
            urbfor = ifelse(SETTLEMENT_TYPE==1,1,0),
            urbinf = ifelse(SETTLEMENT_TYPE==2,1,0),
            tradit = ifelse(SETTLEMENT_TYPE==4,1,0),
            rurfor = ifelse(SETTLEMENT_TYPE==5,1,0),
            settle = factor(SETTLEMENT_TYPE, levels = c(1,2,4,5),
                            labels=c("Urban Formal","Urban Informal",
                                     "Traditional Area","Rural Formal")),
  )%>%
  droplevels() 

# household level variables
Fdata <- Households_df %>%
  dplyr::select(
    UQNO, income_inkind, expenditure_inkind, Q229NETINCOME
  ) %>%
  filter(Q229NETINCOME != 0,
         income_inkind != 0,
         expenditure_inkind != 0) %>%
  transmute(
    UQNO = as.character(UQNO),
    x = as.numeric(expenditure_inkind), 
    x.month = x/12, 
    y = as.numeric(income_inkind),  
    y.month = y/12,
    lnx = log(x.month+1),
    lny = log(y.month+1),
    minq = Q229NETINCOME/12, 
    lnminq = log(minq+1)
  ) %>%
  droplevels() 

LCS2014 <- Adata %>%
  full_join(person1, by="UQNO") %>%
  full_join(Fdata, by="UQNO")


# Creating binary indicators for kids and adults
Final1 <- LCS2014 %>%
  mutate(
    adult = ordered(adults),
    kid = ordered(kids),
    No_Kids = 1 * (kids == 0),
    One_Kid = 1 * (kids == 1),
    two_kids = 1 * (kids == 2),
    three_kids = 1 * (kids == 3),
    four_kids = 1 * (kids == 4),
    five_kids = 1 * (kids == 5),
    six_kids = 1 * (kids == 6),
    seven_kids = 1 * (kids == 7),
    eight_kids = 1 * (kids == 8),
    nine_kids = 1 * (kids == 9),
    ten_kids = 1 * (kids == 10),
    eleven_kids = 1 * (kids == 11),
    twelve_kids = 1 * (kids == 12),
    thirteen_kids = 1 * (kids == 13),
    fourteen_kids = 1 * (kids == 14),
    
    one_adult = 1 * (adults == 1),
    two_adults = 1 * (adults == 2),
    three_adults = 1 * (adults == 3),
    four_adults = 1 * (adults == 4),
    five_adults = 1 * (adults == 5),
    six_adults = 1 * (adults == 6),
    seven_adults = 1 * (adults == 7),
    eight_adults = 1 * (adults == 8),
    nine_adults = 1 * (adults == 9),
    ten_adults = 1 * (adults == 10),
    eleven_adults = 1 * (adults == 11),
    twelve_adults = 1 * (adults == 12),
    thirteen_adults = 1 * (adults == 13),
    fourteen_adults = 1 * (adults == 14),
    fifteen_adults = 1 * (adults == 15),
    sixteen_adults = 1 * (adults == 16),
    seventeen_adults = 1 * (adults == 17)
  )

# Creating binary indicators for kids and adults
Final2 <- LCS2014 %>%
  mutate(
    adult = poly(adults, degree = 6),  # Restrict to 6th degree for adults
    kid = poly(kids, degree = 6),
    No_Kids = 1 * (kids == 0),
    One_Kid = 1 * (kids == 1),
    two_kids = 1 * (kids == 2),
    three_kids = 1 * (kids == 3),
    four_kids = 1 * (kids == 4),
    five_kids = 1 * (kids == 5),
    six_kids = 1 * (kids == 6),
    seven_kids = 1 * (kids == 7),
    eight_kids = 1 * (kids == 8),
    nine_kids = 1 * (kids == 9),
    ten_kids = 1 * (kids == 10),
    eleven_kids = 1 * (kids == 11),
    twelve_kids = 1 * (kids == 12),
    thirteen_kids = 1 * (kids == 13),
    fourteen_kids = 1 * (kids == 14),
    
    one_adult = 1 * (adults == 1),
    two_adults = 1 * (adults == 2),
    three_adults = 1 * (adults == 3),
    four_adults = 1 * (adults == 4),
    five_adults = 1 * (adults == 5),
    six_adults = 1 * (adults == 6),
    seven_adults = 1 * (adults == 7),
    eight_adults = 1 * (adults == 8),
    nine_adults = 1 * (adults == 9),
    ten_adults = 1 * (adults == 10),
    eleven_adults = 1 * (adults == 11),
    twelve_adults = 1 * (adults == 12),
    thirteen_adults = 1 * (adults == 13),
    fourteen_adults = 1 * (adults == 14),
    fifteen_adults = 1 * (adults == 15),
    sixteen_adults = 1 * (adults == 16),
    seventeen_adults = 1 * (adults == 17)
  )

# Dropping rows where lnx, lny, or lnminq is 0
Final1 <- Final1 %>%
  filter(lnx != 0, lny != 0, lnminq != 0)  #observations dropped to 22821

# Dropping rows where lnx, lny, or lnminq is 0
Final2 <- Final2 %>%
  filter(lnx != 0, lny != 0, lnminq != 0)  #observations dropped to 22821

Final2 <- LCS2014 %>%
  mutate(
    adult = as.integer(adults),  # Convert adults to continuous variable
    kid = as.integer(kids)       # Convert kids to continuous variable
  )


####################### Cross Tabulation of LNMINQ by Kids and Adults ##########

cross_tab <- Final1 %>%
  group_by(kids, adults) %>%
  summarise(mean_lnMINQ = mean(lnminq, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = kids, values_from = mean_lnMINQ, names_prefix = "Kids_") 

print(cross_tab)

#######Plot of ln y and lnminq

ggplot(Final1, aes(x = lny, y = lnminq)) +
  geom_point(alpha = 0.4, color = "blue") +  
  geom_smooth(method = "loess", color = "red", se = TRUE) +  
  labs(
    title = "Smooth Plot of lnminq vs. lny",
    x = "Log of Monthly Income (lny)",
    y = "Log of Minimum Income (lnminq)"
  ) +
  theme_minimal()  # Use a clean theme

##########

# Plot distribution of lnminq
ggplot(Final1, aes(x = lnminq)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of lnminq",
    x = "Log of Minimum Income (lnminq)",
    y = "Density"
  ) +
  theme_minimal()

# Plot distribution of minq
ggplot(Final1, aes(x = minq)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "green", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of minq",
    x = "Minimum Income (minq)",
    y = "Density"
  ) +
  theme_minimal()

####################################################################

# Create the linear model
basemodel1 <- lm(lnminq ~ lny + I(lny^2), data = Final1)
summary(basemodel1)
model1 <- lm(lnminq ~ lny + I(lny^2) + One_Kid + two_kids + three_kids + four_kids + five_kids + six_kids  + two_adults + three_adults + four_adults + five_adults + six_adults, data = Final1)

# Summary of the model
summary(model1)


model2 <- lm(lnminq ~ lny + I(lny^2)  + One_Kid + two_kids + three_kids + four_kids + five_kids + six_kids + two_adults + three_adults + four_adults + five_adults + six_adults + KZN + EC + FS + GP + MP +LP + WC + NC, data = Final1)
summary(model2)

model3 <- lm(lnminq ~ lny+ I(lny^2)  + One_Kid + two_kids + three_kids + four_kids + five_kids + six_kids + two_adults + three_adults + four_adults + five_adults + six_adults + KZN + EC +  FS + GP + MP +LP + WC + NC + urbfor + urbinf+ tradit, data = Final1)
summary(model3)

model_original <- lm(lnminq ~ lny, 
                     data = Final1)

anova(model_original, basemodel1)

models <- list(
  "Model 0" = basemodel1,
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3
)


# Create and save the summary table to a PDF
msummary(models, 
         stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
         output = "summary_tableV1.4.docx")

# Create the linear model
model4 <- lm(lnminq ~ lny+ I(lny^2) + kid + adult, data = Final2)

# Summary of the model
summary(model4)


model5 <- lm(lnminq ~ lny + I(lny^2) + kid + adult + KZN + EC + FS + GP + MP +LP + WC + NC, data = Final2)
summary(model5)

model6 <- lm(lnminq ~ lny  + I(lny^2) + kid + adult + KZN + EC +  FS + GP + MP +LP + WC + NC + urbfor + urbinf+ tradit, data = Final2)
summary(model6)


models3 <- list(
  "(1)" = model4, 
  "(2)" = model5, 
  "(3)" = model6
)

msummary(models3, 
         stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
         output = "summary_tableV1.5.docx")
######################################################################################


#####################################################Subjective poverty line 
###################Poverty Line From Model 1

# Define the range of adults and children
num_adults <- 1:6
num_children <- 0:6

# Create a data frame for all combinations of adults and children
spl_table <- expand.grid(
  Adults = num_adults,
  Children = num_children
)

# Add the predictors for the combinations
spl_table <- spl_table %>%
  mutate(
    One_Kid = ifelse(Children == 1, 1, 0),
    two_kids = ifelse(Children == 2, 1, 0),
    three_kids = ifelse(Children == 3, 1, 0),
    four_kids = ifelse(Children == 4, 1, 0),
    five_kids = ifelse(Children == 5, 1, 0),
    six_kids = ifelse(Children == 6, 1, 0),
    two_adults = ifelse(Adults == 2, 1, 0),
    three_adults = ifelse(Adults == 3, 1, 0),
    four_adults = ifelse(Adults == 4, 1, 0),
    five_adults = ifelse(Adults == 5, 1, 0),
    six_adults = ifelse(Adults == 6, 1, 0),
    lny = 8.40  # Fixed value for log income
  )

# Predict SPL and SE for each combination
predictions <- predict(model1, newdata = spl_table, se.fit = TRUE)

# Add SPL and SE to the data frame
spl_table <- spl_table %>%
  mutate(
    SPL = predictions$fit,
    SE = predictions$se.fit
  )

# Select and arrange columns for the final table
final_spl_table <- spl_table %>%
  select(Adults, Children, SPL, SE) %>%
  arrange(Adults, Children)

# Round SPL and SE values to 3 decimal places
final_spl_table <- final_spl_table %>%
  mutate(
    SPL = round(SPL, 3),
    SE = round(SE, 3)
  )

# Print the final table
print(final_spl_table)

# Save the table to a CSV file (optional)
write.csv(final_spl_table, "subjective_poverty_line_with_SE.csv", row.names = FALSE)

ggplot(final_spl_table, aes(x = Adults, y = SPL, color = as.factor(Children))) +
  geom_smooth(aes(linetype = as.factor(Children)), method = "loess", se = FALSE, size = 0.8) +
  scale_color_manual(values = c("black", "red", "blue", "green", "purple", "pink", "orange")) +  # Customize colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "dashed", "solid", "dotted")) +  # Simplified line types
  labs(
    x = "Adults in Household",
    y = "Subjective Poverty Line (Log Monthly Values)",
    color = "Number of Children",
    linetype = "Number of Children"
  ) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(size = 10), legend.text = element_text(size = 8))




######################Baseline household from Model 2################

# Define the range of adults and children
num_adults <- 1:6
num_children <- 0:6

# Create a data frame for all combinations of adults and children
nw_spl_table <- expand.grid(
  Adults = num_adults,
  Children = num_children
)

# Add predictors for children and adults
nw_spl_table <- nw_spl_table %>%
  mutate(
    One_Kid = ifelse(Children == 1, 1, 0),
    two_kids = ifelse(Children == 2, 1, 0),
    three_kids = ifelse(Children == 3, 1, 0),
    four_kids = ifelse(Children == 4, 1, 0),
    five_kids = ifelse(Children == 5, 1, 0),
    six_kids = ifelse(Children == 6, 1, 0),
    two_adults = ifelse(Adults == 2, 1, 0),
    three_adults = ifelse(Adults == 3, 1, 0),
    four_adults = ifelse(Adults == 4, 1, 0),
    five_adults = ifelse(Adults == 5, 1, 0),
    six_adults = ifelse(Adults == 6, 1, 0),
    # Set all region dummy variables to 0 (for North West)
    KZN = 0, EC = 0, FS = 0, GP = 0, MP = 0, LP = 0, WC = 0, NC = 0,
    lny = 8.40  # Fixed value for log income
  )

# Predict SPL and SE for each combination using the adjusted model
predictions <- predict(model2, newdata = nw_spl_table, se.fit = TRUE)

# Add SPL and SE to the data frame
nw_spl_table <- nw_spl_table %>%
  mutate(
    SPL = predictions$fit,
    SE = predictions$se.fit
  )

# Select and arrange columns for the final table
nw_final_spl_table <- nw_spl_table %>%
  select(Adults, Children, SPL, SE) %>%
  arrange(Adults, Children)

# Round SPL and SE values to 3 decimal places
nw_final_spl_table <- nw_final_spl_table %>%
  mutate(
    SPL = round(SPL, 3),
    SE = round(SE, 3)
  )

# Print the final table
print(nw_final_spl_table)

# Save the table to a CSV file (optional)
write.csv(nw_final_spl_table, "north_west_subjective_poverty_line_with_SE.csv", row.names = FALSE)


ggplot(nw_final_spl_table, aes(x = Adults, y = SPL, color = as.factor(Children))) +
  geom_smooth(aes(linetype = as.factor(Children)), method = "loess", se = FALSE, size = 0.8) +
  scale_color_manual(values = c("black", "red", "blue", "green", "purple", "pink", "orange")) +  # Customize colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "dashed", "solid", "dotted")) +  # Simplified line types
  labs(
    x = "Adults in Household",
    y = "Subjective Poverty Line (Log Monthly Values)",  # Updated label to reflect log values
    color = "Number of Children",
    linetype = "Number of Children"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )



################################################# Northern Cape SPLS######
# Define the range of adults and children
num_adults <- 1:6
num_children <- 0:6

# Create a data frame for all combinations of adults and children
nc_spl_table <- expand.grid(
  Adults = num_adults,
  Children = num_children
)

# Add predictors for children and adults
nc_spl_table <- nc_spl_table %>%
  mutate(
    One_Kid = ifelse(Children == 1, 1, 0),
    two_kids = ifelse(Children == 2, 1, 0),
    three_kids = ifelse(Children == 3, 1, 0),
    four_kids = ifelse(Children == 4, 1, 0),
    five_kids = ifelse(Children == 5, 1, 0),
    six_kids = ifelse(Children == 6, 1, 0),
    two_adults = ifelse(Adults == 2, 1, 0),
    three_adults = ifelse(Adults == 3, 1, 0),
    four_adults = ifelse(Adults == 4, 1, 0),
    five_adults = ifelse(Adults == 5, 1, 0),
    six_adults = ifelse(Adults == 6, 1, 0),
    # Set all region dummy variables to 0 except for NC
    KZN = 0, EC = 0, FS = 0, GP = 0, MP = 0, LP = 0, WC = 0, NC = 1,
    lny = 8.40  # Fixed value for log income
  )

# Predict SPL and SE for each combination using the adjusted model
predictions <- predict(model2, newdata = nc_spl_table, se.fit = TRUE)

# Add SPL and SE to the data frame
nc_spl_table <- nc_spl_table %>%
  mutate(
    SPL = predictions$fit,
    SE = predictions$se.fit
  )

# Select and arrange columns for the final table
nc_final_spl_table <- nc_spl_table %>%
  select(Adults, Children, SPL, SE) %>%
  arrange(Adults, Children)

# Round SPL and SE values to 3 decimal places
nc_final_spl_table <- nc_final_spl_table %>%
  mutate(
    SPL = round(SPL, 3),
    SE = round(SE, 3)
  )

# Print the final table
print(nc_final_spl_table)

# Save the table to a CSV file (optional)
write.csv(nc_final_spl_table, "northern_cape_subjective_poverty_line_with_SE.csv", row.names = FALSE)


# Plot SPL results
library(ggplot2)

ggplot(nc_final_spl_table, aes(x = Adults, y = SPL, color = as.factor(Children))) +
  geom_smooth(aes(linetype = as.factor(Children)), method = "loess", se = FALSE, size = 0.8) +
  scale_color_manual(values = c("black", "red", "blue", "green", "purple", "pink", "orange")) +  # Customize colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "dashed", "solid", "dotted")) +  # Simplified line types
  labs(
    x = "Adults in Household", 
    y = "Subjective Poverty Line (Log Monthly Values)", 
    color = "Number of Children", 
    linetype = "Number of Children"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8)
  )


# Plot SPL results
library(ggplot2)

ggplot(spl_results_model2, aes(x = Adults, y = SPL, color = as.factor(Children))) +
  geom_smooth(aes(linetype = as.factor(Children)), method = "loess", se = FALSE, size = 0.8) +
  scale_color_manual(values = c("black", "red", "blue", "green", "purple", "pink", "orange")) +  # Customize colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "dashed", "solid", "dotted")) +  # Simplified line types
  labs(x = "Adults in Household", y = "Subjective Poverty Line (Log Monthly Values)", color = "Number of Children", linetype = "Number of Children") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(size = 10), legend.text = element_text(size = 8))


#####################################################################

######################################## Code for Model 3#######
# Define the range of adults and children
num_adults <- 1:6
num_children <- 0:6

# Create a data frame for all combinations of adults and children
nw_rural_spl_table <- expand.grid(
  Adults = num_adults,
  Children = num_children
)

# Add predictors for children and adults
nw_rural_spl_table <- nw_rural_spl_table %>%
  mutate(
    One_Kid = ifelse(Children == 1, 1, 0),
    two_kids = ifelse(Children == 2, 1, 0),
    three_kids = ifelse(Children == 3, 1, 0),
    four_kids = ifelse(Children == 4, 1, 0),
    five_kids = ifelse(Children == 5, 1, 0),
    six_kids = ifelse(Children == 6, 1, 0),
    two_adults = ifelse(Adults == 2, 1, 0),
    three_adults = ifelse(Adults == 3, 1, 0),
    four_adults = ifelse(Adults == 4, 1, 0),
    five_adults = ifelse(Adults == 5, 1, 0),
    six_adults = ifelse(Adults == 6, 1, 0),
    # Set all regional and settlement dummy variables to 0 for baseline (Northwest province, rural settlement)
    KZN = 0, EC = 0, FS = 0, GP = 0, MP = 0, LP = 0, WC = 0, NC = 0,
    urbfor = 0, urbinf = 0, tradit = 0,
    lny = 8.40  # Fixed value for log income
  )

# Predict SPL and SE for each combination using the adjusted model
predictions <- predict(model3, newdata = nw_rural_spl_table, se.fit = TRUE)

# Add SPL and SE to the data frame
nw_rural_spl_table <- nw_rural_spl_table %>%
  mutate(
    SPL = predictions$fit,
    SE = predictions$se.fit
  )

# Select and arrange columns for the final table
nw_rural_final_spl_table <- nw_rural_spl_table %>%
  select(Adults, Children, SPL, SE) %>%
  arrange(Adults, Children)

# Round SPL and SE values to 3 decimal places
nw_rural_final_spl_table <- nw_rural_final_spl_table %>%
  mutate(
    SPL = round(SPL, 3),
    SE = round(SE, 3)
  )

# Print the final table
print(nw_rural_final_spl_table)

# Save the table to a CSV file (optional)
write.csv(nw_rural_final_spl_table, "spl_results_model3_northwest_rural.csv", row.names = FALSE)


# Plot the results for Model 3
library(ggplot2)

ggplot(nw_rural_final_spl_table, aes(x = Adults, y = SPL, color = as.factor(Children))) +
  geom_smooth(aes(linetype = as.factor(Children)), method = "loess", se = FALSE, size = 0.8) +
  scale_color_manual(values = c("black", "red", "blue", "green", "purple", "pink", "orange")) +  # Customize colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "dashed", "solid", "dotted")) +  # Simplified line types
  labs(
    x = "Adults in Household", 
    y = "Subjective Poverty Line (Log Monthly Values)", 
    color = "Number of Children", 
    linetype = "Number of Children"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8)
  )

######### Households in Traditional Areas
# Define the range of adults and children
num_adults <- 1:6
num_children <- 0:6

# Create a data frame for all combinations of adults and children
urban_spl_table <- expand.grid(
  Adults = num_adults,
  Children = num_children
)

# Add predictors for children and adults
urban_spl_table <- urban_spl_table %>%
  mutate(
    One_Kid = ifelse(Children == 1, 1, 0),
    two_kids = ifelse(Children == 2, 1, 0),
    three_kids = ifelse(Children == 3, 1, 0),
    four_kids = ifelse(Children == 4, 1, 0),
    five_kids = ifelse(Children == 5, 1, 0),
    six_kids = ifelse(Children == 6, 1, 0),
    two_adults = ifelse(Adults == 2, 1, 0),
    three_adults = ifelse(Adults == 3, 1, 0),
    four_adults = ifelse(Adults == 4, 1, 0),
    five_adults = ifelse(Adults == 5, 1, 0),
    six_adults = ifelse(Adults == 6, 1, 0),
    # Set settlement type to urban formal (urbfor = 1)
    urbfor = 1, 
    urbinf = 0, 
    tradit = 0,
    # Set all regional dummy variables to 0 for baseline province
    KZN = 0, EC = 0, FS = 0, GP = 0, MP = 0, LP = 0, WC = 0, NC = 0,
    lny = 8.40  # Fixed value for log income
  )

# Predict SPL and SE for each combination using the adjusted model
predictions <- predict(model3, newdata = urban_spl_table, se.fit = TRUE)

# Add SPL and SE to the data frame
urban_spl_table <- urban_spl_table %>%
  mutate(
    SPL = predictions$fit,
    SE = predictions$se.fit
  )

# Select and arrange columns for the final table
urban_final_spl_table <- urban_spl_table %>%
  select(Adults, Children, SPL, SE) %>%
  arrange(Adults, Children)

# Round SPL and SE values to 3 decimal places
urban_final_spl_table <- urban_final_spl_table %>%
  mutate(
    SPL = round(SPL, 3),
    SE = round(SE, 3)
  )

# Print the final table
print(urban_final_spl_table)

# Save the table to a CSV file (optional)
write.csv(urban_final_spl_table, "urban_spl_results_model3.csv", row.names = FALSE)

# Plot the results for Model 3
library(ggplot2)

ggplot(urban_final_spl_table, aes(x = Adults, y = SPL, color = as.factor(Children))) +
  geom_smooth(aes(linetype = as.factor(Children)), method = "loess", se = FALSE, size = 0.8) +
  scale_color_manual(values = c("black", "red", "blue", "green", "purple", "pink", "orange")) +  # Customize colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "dashed", "solid", "dotted")) +  # Simplified line types
  labs(x = "Adults in Household", y = "Subjective Poverty Line (Log Monthly Values", color = "Number of Children", linetype = "Number of Children") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(size = 10), legend.text = element_text(size = 8))





############################################################################## Comparison 

# Define the national poverty line (replace with actual value if available)
national_poverty_line <- 647  # Example: Monthly poverty line in rand

# Convert log SPLs to rands for Model 2 results
spl_results_model2 <- spl_results %>%
  mutate(
    SPL_Rand = exp(SPL)  # Convert log SPLs back to rands
  )

# Step 1: Identify households where MINQ is below the predicted SPL
households_below_SPL <- Final1 %>%
  rowwise() %>%
  mutate(
    SPL_Household = ifelse(
      nrow(spl_results_model2 %>% filter(Adults == adults & Children == kids)) > 0,
      spl_results_model2 %>%
        filter(Adults == adults & Children == kids) %>%
        pull(SPL_Rand),
      NA_real_  # Fallback value if no match is found
    ),
    Below_SPL = ifelse(!is.na(SPL_Household) & minq < SPL_Household, 1, 0)
  )

# Step 2: Summarize results
summary_below_SPL <- households_below_SPL %>%
  summarize(
    Total_Households = n(),
    Households_Below_SPL = sum(Below_SPL, na.rm = TRUE),
    Percent_Below_SPL = mean(Below_SPL, na.rm = TRUE) * 100
  )

# Print summary
print(summary_below_SPL)

# Step 3: Summarize results by province
summary_by_province <- households_below_SPL %>%
  group_by(province) %>%
  summarize(
    Total_Households = n(),
    Households_Below_SPL = sum(Below_SPL, na.rm = TRUE),
    Percent_Below_SPL = mean(Below_SPL, na.rm = TRUE) * 100
  )

# Print province summary
print(summary_by_province)






# Convert log SPLs to rands for Model 2 results
spl_results_model2 <- spl_results %>%
  mutate(
    SPL_Rand = exp(SPL)  # Convert log SPLs back to rands
  )

# Step 1: Identify households where MINQ is below the predicted SPL
households_below_SPL <- Final1 %>%
  rowwise() %>%
  mutate(
    SPL_Household = ifelse(
      nrow(spl_results_model2 %>% filter(Adults == adults & Children == kids)) > 0,
      spl_results_model2 %>%
        filter(Adults == adults & Children == kids) %>%
        pull(SPL_Rand),
      NA_real_  # Assign NA if no match is found
    ),
    Below_SPL = ifelse(!is.na(SPL_Household) & minq < SPL_Household, 1, 0),
    Below_Food_Poverty_Line = ifelse(minq < 447, 1, 0)  # Compare against food poverty line (447)
  )


# Step 2: Summarize results by household composition
summary_by_composition <- households_below_SPL %>%
  group_by(adults, kids) %>%
  summarize(
    SPL_Rand = mean(SPL_Household, na.rm = TRUE),
    Total_Households = n(),
    Percent_Below_SPL = mean(Below_SPL, na.rm = TRUE) * 100,
    Percent_Below_Food_Poverty_Line = mean(Below_Food_Poverty_Line, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Print summary by household composition
print(summary_by_composition)

# Step 3: Save the summary to a CSV file
write.csv(summary_by_composition, "Summary_By_Composition.csv", row.names = FALSE)

# Step 4: Visualize results (optional)
library(ggplot2)

ggplot(summary_by_composition, aes(x = factor(adults), y = SPL_Rand, fill = factor(kids))) +
  geom_col(position = "dodge") +
  labs(
    title = "SPL by Household Composition",
    x = "Number of Adults",
    y = "SPL (Rand)",
    fill = "Number of Children"
  ) +
  theme_minimal()





#################################################################

# Step 1: Convert log SPLs to rands for Model 2 results
spl_results_model2 <- spl_results_model2 %>%
  mutate(
    SPL_Rand = exp(SPL)  # Convert log SPLs back to rands
  )

# Step 2: Identify households where MINQ is below the predicted SPL and poverty lines
households_below_SPL <- Final1 %>%
  rowwise() %>%
  mutate(
    SPL_Household = ifelse(
      nrow(spl_results_model2 %>% filter(Adults == adults & Children == kids)) > 0,
      spl_results_model2 %>%
        filter(Adults == adults & Children == kids) %>%
        pull(SPL_Rand),
      NA_real_  # Assign NA if no match is found
    ),
    Below_SPL = ifelse(!is.na(SPL_Household) & minq < SPL_Household, 1, 0),
    Below_Food_Poverty_Line = ifelse(minq < 447, 1, 0),  # Compare against food poverty line (R447)
    Below_Lower_Bound_Poverty_Line = ifelse(minq < 647, 1, 0)  # Compare against lower bound poverty line (R647)
  )

# Step 3: Summarize results by household composition
summary_by_composition <- households_below_SPL %>%
  group_by(adults, kids) %>%
  summarize(
    SPL_Rand = mean(SPL_Household, na.rm = TRUE),
    Total_Households = n(),
    Percent_Below_SPL = mean(Below_SPL, na.rm = TRUE) * 100,
    Percent_Below_Food_Poverty_Line = mean(Below_Food_Poverty_Line, na.rm = TRUE) * 100,
    Percent_Below_Lower_Bound_Poverty_Line = mean(Below_Lower_Bound_Poverty_Line, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Print summary by household composition
print(summary_by_composition)

# Step 4: Save the summary to a CSV file
write.csv(summary_by_composition, "Summary_By_Compositionv1.2.csv", row.names = FALSE)

# Step 5 (Optional): Summarize overall percentages
overall_summary <- households_below_SPL %>%
  summarize(
    Total_Households = n(),
    Percent_Below_SPL = mean(Below_SPL, na.rm = TRUE) * 100,
    Percent_Below_Food_Poverty_Line = mean(Below_Food_Poverty_Line, na.rm = TRUE) * 100,
    Percent_Below_Lower_Bound_Poverty_Line = mean(Below_Lower_Bound_Poverty_Line, na.rm = TRUE) * 100
  )

# Print overall summary
print(overall_summary)

# Step 6 (Optional): Save the overall summary to a CSV file
write.csv(overall_summary, "Overall_Summary.csv", row.names = FALSE)







