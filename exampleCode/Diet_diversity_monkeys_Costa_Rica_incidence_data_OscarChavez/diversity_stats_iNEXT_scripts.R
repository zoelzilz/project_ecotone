Sys.time()

[1] "2022-06-17 14:57:19 CST"

#import the programs

library(iNEXT) 
library(ggplot2) 
library(devtools)
library( httr)

    #Step 1: open the dataframes for each of the four species

#Alouatta palliata palliata

diet.ap=read.table("diet_incidence_Ap.txt", header=T) # this dataset have some errors


#Ateles geoffroyi

diet.at=read.table("diet_incidence_Ate.txt", header=T)


#Cebus imitator

diet.ce=read.table("diet_incidence_Ceb.txt", header=T)


#Saimiri oerstedii

diet.sa=read.table("diet_incidence_Sai.txt", header=T)


#all data pooled for gamma diversity

diet.all=read.table("diet_incidence_all.txt", header=T)



#########################

    #Step 2: convert the data to a list of 4 matrices to run iNEXT



#a) make a matrix from diet.ap as type "integer"

diet1 <- as.matrix(apply(diet.ap[,-1],2,as.integer))

# use your species names as row names
row.names(diet1) <- diet.ap[,1]


#b) make a matrix from diet.at as type "integer"

diet2 <- as.matrix(apply(diet.at[,-1],2,as.integer))

# use your species names as row names
row.names(diet2) <- diet.at[,1]


#c) make a matrix from diet.ce as type "integer"

diet3 <- as.matrix(apply(diet.ce[,-1],2,as.integer))

# use your species names as row names
row.names(diet3) <- diet.ce[,1]


#d) make a matrix from diet.sa as type "integer"

diet4 <- as.matrix(apply(diet.sa[,-1],2,as.integer))

# use your species names as row names
row.names(diet4) <- diet.sa[,1]


#d) make a matrix from diet.sa as type "integer"

diet4 <- as.matrix(apply(diet.sa[,-1],2,as.integer))

# use your species names as row names
row.names(diet4) <- diet.sa[,1]



#e) make a matrix from diet.all as type "integer"
#this dataset is for all the monkey species pooled

diet5 <- as.matrix(apply(diet.all[,-1],2,as.integer))

# use your species names as row names
row.names(diet5) <- diet.all[,1]


#########################################


     #Step 3: create a list of your matrices (4 matrices in this case)

diet.diver = list(Alouatta = diet1, Ateles = diet2, Cebus = diet3, Saimiri = diet4, all=diet5)



# considering the four monkey species pooled (i.e. gamma diversity): 

diet.all = list(all = diet5) # this dataset can be use to analyze the gamma diversity separately



################################

    # Step 4) use the iNEXT function to estimate the95% CI

#observations:

#1) we also can to specify the endpoint and se as follow:
#out.inc=iNEXT(diet.diver, q=0, datatype="incidence_raw", endpoint=F, se=TRUE)

#2) In these cases I am only interest in the order q=0 because the small number of species


iNEXT(diet.diver, q=0, datatype="incidence_raw") # see resuls in the results file
