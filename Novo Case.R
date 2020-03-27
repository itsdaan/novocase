###################################
###### Novo case study prep #######
###################################

#Loadiing packages
library("tidyverse")

#Nordic Letters
getOption("encoding")

#Modifiy the encoding
options(encoding = "ISO-8859-1")

# Loading "Scrap" Dataset ------------------------------------------------------

#Loading the "scrap" file
scrap <- read.csv("Scrap.csv", sep = "\t", header = TRUE) 

# Using the pipe function to perform next ...
scrap <- scrap %>% 
  mutate(KeyMaterialBatch = paste(Material, Batch, sep = "#")) %>%  # creating new variable Metrail#Batch
  group_by(KeyMaterialBatch, Material, Batch) %>%  # Grouping values by varaibles
  summarize(Scrap.qty = sum(Quantity)) # Summarizing the quantity by batch and material

str(scrap)
View(scrap)


# Loading "Produciton Volumes" dataset ------------------------------------

#Loading the "Produciton Volumes" file
pv <- read.csv("Production Volumes.csv", sep = "\t", header = TRUE) 

str(pv)
View(pv)

# Using the pipe function to perform next ...
pv1 <- pv %>%
  mutate(reg.year = substr(Registration.date, 1,4), # creating new variable only for the year the product was produced
         Material = as.character(Material), # changing the format of variable "material" from int to chr
         Batch = as.character(Batch), # same but for batch
         KeyMaterialBatch = paste(Material, Batch, sep = "#")) %>% # new variable material batch
  group_by(Material, Batch, KeyMaterialBatch, reg.year, Registration.date) %>% # grouping variables by variables
  summarize(Produced.qty = sum(Produced.qty)) %>% # summarizing by quantity
  left_join(scrap, by = c("KeyMaterialBatch" = "KeyMaterialBatch"), suffix = c("", ".y")) %>% # left joining with scrap dataset
  mutate(Scrap.qty = replace_na(Scrap.qty, 0)) %>% #replacing NA with 0
  dplyr::select(-Material.y, -Batch.y) %>% # removing material and batch variables from scrap data set
  mutate(Produced_without_scrap = Produced.qty - Scrap.qty) # displaying the quantity without scrap quantity

str(pv1)
View(pv1)

write_csv(pv1, "pv1.csv")

# Loading "Product list" dataset ------------------------------------

#Loading the "product list" file
pl <- read.csv("Product list.csv", sep = "\t", header = TRUE) 

str(pl)
View(pl)

pl1 <- pl%>% 
  mutate(Material = as.character(Product)) %>% #chaning the type of "product" form int to variable
  left_join(pv1, by = c("Material" = "Material"), suffix = c("", ".y")) %>% # left joining with pv1 
  dplyr::select(-Batch, -Material, -KeyMaterialBatch, -reg.year, -Registration.date) %>% # removing following variables from joined dataset
  mutate(Produced.qty = replace_na(Produced.qty, 0), #replacing na to 0
         Scrap.qty = replace_na(Scrap.qty, 0), #replacing na to 0
         Produced_without_scrap = replace_na(Produced_without_scrap, 0)) %>% #replacing na to 0
  group_by(Product, Description, Production.site, Price.DKK) %>%  #grouping by next variables ...
  summarize(Produced.qty = sum(Produced.qty), # summarizing next variables
            Scrap.qty = sum(Scrap.qty),
            Produced_without_scrap = sum(Produced_without_scrap)) %>% 
  mutate(Scrap_value = Price.DKK * Scrap.qty) # creating yield


str(pl1)
View(pl1)

write_csv(pl1, "pl1.csv")

# Question 1 --------------------------------------------------------------

# products with total production lower than 400 pieces in 2017 

in_2017 <- pv1 %>% 
  filter(reg.year == 2017) %>% 
  group_by(Material) %>% 
  summarize(Produced.qty = sum(Produced.qty)) %>% 
  filter(Produced.qty <= 400)



# Havent been produced in last three years

below_0 <- pv1 %>% 
  group_by(Material) %>% 
  summarize(Produced.qty = sum(Produced.qty)) %>% 
  filter(Produced.qty <= 0)




# Question #2 -------------------------------------------------------------

#Question 2a - top 10 production sites by scrap value

Question_2a_raw <- pl1 %>% 
  group_by(Production.site) %>% 
  summarize(Scrap_value = sum(Scrap_value))

Question_2a <- Question_2a_raw %>% 
  arrange(desc(Scrap_value)) %>% 
  top_n(10)



#Question 2b - the top 10 production sites by yield
Question_2b_raw <- pl1 %>% 
  group_by(Production.site) %>% 
  summarize(Produced_without_scrap = sum(Produced_without_scrap))

Question_2b<- Question_2b_raw %>% 
  arrange(desc(Produced_without_scrap)) %>% 
  top_n(10)
