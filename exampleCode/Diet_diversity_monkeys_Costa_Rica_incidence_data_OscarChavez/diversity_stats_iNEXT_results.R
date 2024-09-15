Sys.time()

[1] "2022-06-17 14:57:19 CST"

#import the programs

library(iNEXT) 
library(ggplot2) 
library(devtools)
library( httr)


##database for diet diversity Costa Rican primates (incidence_raw) using updated data
#on Alouatta palliata (correction of the dataset from Larose 1996 Thesis) and the pooled dataset
#Now we will used the dataset 'data_incidence_Ap3' for howlers


#Step 1: open the dataframes for each of the four species

#Alouatta palliata palliata

diet.ap=read.table("diet_incidence_Ap3.txt", header=T) # this dataset have some errors


#Ateles geoffroyi

diet.at=read.table("diet_incidence_Ate.txt", header=T)


#Cebus imitator

diet.ce=read.table("diet_incidence_Ceb.txt", header=T)


#Saimiri oerstedii

diet.sa=read.table("diet_incidence_Sai.txt", header=T)


#all data pooled for gamma diversity

diet.all=read.table("diet_incidence_all2.txt", header=T)



#########################

#Step 2: covert the data to a list of 4 matrices to run iNEXT


#Some instructions on the topic can be found in stackoverflow webpage:

#https://stackoverflow.com/questions/41241369/converting-data-into-the-correct-format-for-inext-analysis


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

str(diet.diver)

edit=edit(diet.diver) # All look OK



# considering the four monkey species pooled: 

diet.all = list(all = diet5)

str(diet.all)

edit=edit(diet.all) # All look OK


################################
#############################

# Step 4) use the iNEXT function to estimate the95% CI

# set a series of sample sizes (m) for R/E computation if we have a large list
m <- c(1, 3, 5, 7, 9, 11, 13, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60 )#no used here argument 'size=m'

m= c(3, 6, 10, 13, 15, 18, 20, 23, 25, 28, 30, 33, 35, 38, 40)

out.inc=iNEXT(diet.diver, q=0, datatype="incidence_raw")
out.inc

#observations:

#1) we also can to specify the endpoint and se as follow:
#out.inc=iNEXT(diet.diver, q=0, datatype="incidence_raw", endpoint=F, se=TRUE)

#2) In these case I am only interest in the order q=0 because the small number of species

iNEXT(diet.diver, q=0, datatype="incidence_raw")

out.inc

$DataInfo: basic data information
site  T  U      S.obs     SC  Q1  Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10
1 Alouatta 33 1034   439 0.7334 280  71 29 11  6  5  7  3  5   5
2   Ateles  8  469   329 0.5288 237  60 22  7  1  1  1  0  0   0
3    Cebus 15  470   191 0.8922  59  68 31 13  4  7  4  5  0   0
4  Saimiri  5  219   183 0.3199 157  17  8  1  0  0  0  0  0   0
5      all 59 2062   818 0.7610 497 120 62 39 25 10 14  6  6   4

$iNextEst: diversity estimates with rarefied and extrapolated samples.
$Alouatta
t            method order      qD  qD.LCL  qD.UCL    SC SC.LCL SC.UCL
1   1 interpolated     0  31.333  29.834  32.832 0.172  0.152  0.192
10 16 interpolated     0 271.962 256.463 287.460 0.633  0.612  0.654
20 33     observed     0 439.000 412.588 465.412 0.733  0.711  0.756
30 49 extrapolated     0 558.086 521.943 594.228 0.793  0.765  0.821
40 66 extrapolated     0 655.733 607.773 703.692 0.841  0.811  0.871

$Ateles
t       method order      qD  qD.LCL  qD.UCL    SC SC.LCL SC.UCL
1   1 interpolated     0  58.625  54.100  63.150 0.130  0.102  0.158
2   1 interpolated     0  58.625  54.100  63.150 0.130  0.102  0.158
3   1 interpolated     0  58.625  54.100  63.150 0.130  0.102  0.158
10  4 interpolated     0 195.971 181.112 210.831 0.363  0.317  0.409
11  4 interpolated     0 195.971 181.112 210.831 0.363  0.317  0.409
12  4 interpolated     0 195.971 181.112 210.831 0.363  0.317  0.409
20  8     observed     0 329.000 302.879 355.121 0.529  0.473  0.584
30 12 extrapolated     0 428.820 392.060 465.580 0.644  0.583  0.704
31 12 extrapolated     0 428.820 392.060 465.580 0.644  0.583  0.704
40 16 extrapolated     0 504.312 456.652 551.971 0.730  0.671  0.790

$Cebus
t       method order      qD  qD.LCL  qD.UCL    SC SC.LCL SC.UCL
1   1 interpolated     0  31.333  28.561  34.106 0.185  0.159  0.210
2   1 interpolated     0  31.333  28.561  34.106 0.185  0.159  0.210
10  7 interpolated     0 136.798 128.680 144.917 0.671  0.638  0.704
20 15     observed     0 191.000 180.062 201.938 0.892  0.860  0.924
30 22 extrapolated     0 206.670 192.315 221.025 0.963  0.934  0.992
40 30 extrapolated     0 212.461 193.857 231.066 0.989  0.969  1.000

$Saimiri
t       method        order   qD  qD.LCL  qD.UCL    SC SC.LCL SC.UCL
1   1 interpolated     0  43.800  39.001  48.599 0.107  0.059  0.155
2   1 interpolated     0  43.800  39.001  48.599 0.107  0.059  0.155
3   1 interpolated     0  43.800  39.001  48.599 0.107  0.059  0.155
4   1 interpolated     0  43.800  39.001  48.599 0.107  0.059  0.155
5   1 interpolated     0  43.800  39.001  48.599 0.107  0.059  0.155
6   1 interpolated     0  43.800  39.001  48.599 0.107  0.059  0.155
7   2 interpolated     0  82.900  73.798  92.002 0.187  0.126  0.248
8   2 interpolated     0  82.900  73.798  92.002 0.187  0.126  0.248
9   2 interpolated     0  82.900  73.798  92.002 0.187  0.126  0.248
10  2 interpolated     0  82.900  73.798  92.002 0.187  0.126  0.248
11  2 interpolated     0  82.900  73.798  92.002 0.187  0.126  0.248
12  2 interpolated     0  82.900  73.798  92.002 0.187  0.126  0.248
20  5     observed     0 183.000 161.078 204.922 0.320  0.239  0.401
26  7 extrapolated     0 241.045 210.356 271.733 0.388  0.295  0.481
27  7 extrapolated     0 241.045 210.356 271.733 0.388  0.295  0.481
28  7 extrapolated     0 241.045 210.356 271.733 0.388  0.295  0.481
29  7 extrapolated     0 241.045 210.356 271.733 0.388  0.295  0.481
30  7 extrapolated     0 241.045 210.356 271.733 0.388  0.295  0.481
40 10 extrapolated     0 317.404 273.001 361.807 0.478  0.372  0.583

$all
t       method order           qD   qD.LCL   qD.UCL    SC SC.LCL SC.UCL
1    1 interpolated     0   34.949   33.466   36.432 0.107  0.096  0.118
10  29 interpolated     0  523.735  499.603  547.867 0.665  0.646  0.683
20  59     observed     0  818.000  776.865  859.135 0.761  0.743  0.779
30  87 extrapolated     0 1027.615  971.084 1084.147 0.810  0.790  0.831
40 118 extrapolated     0 1209.427 1134.701 1284.152 0.853  0.833  0.874


$AsyEst: asymptotic diversity estimates along with related statistics.
Site                Diversity Observed  Estimator    s.e.      LCL      UCL
1  Alouatta  Species richness  439.000   974.382  93.098  820.723 1189.895
2  Alouatta Shannon diversity  265.558   423.622  20.045  384.335  462.909
3  Alouatta Simpson diversity  158.911   181.933   8.146  165.967  197.899
4    Ateles  Species richness  329.000   738.566  77.694  612.319  921.067
5    Ateles Shannon diversity  289.097   624.744  35.178  555.797  693.691
6    Ateles Simpson diversity  245.219   448.853  26.010  397.875  499.830
7     Cebus  Species richness  191.000   214.889   8.425  203.212  237.733
8     Cebus Shannon diversity  157.575   197.699   6.264  185.423  209.976
9     Cebus Simpson diversity  131.020   169.256   6.657  156.210  182.303
10  Saimiri  Species richness  183.000   762.976 170.108  513.282 1201.441
11  Saimiri Shannon diversity  169.991   644.696  88.863  470.528  818.864
12  Saimiri Simpson diversity  153.230   406.515  48.959  310.557  502.473
13      all  Species richness  818.000  1829.760 133.345 1600.304 2126.517
14      all Shannon diversity  489.043   757.854  25.894  707.104  808.605
15      all Simpson diversity  285.781   325.996  11.692  303.079  348.913

#Observed sampling completeness SC

# For Alouatta: 73% (n=33, CI= 71-76), Ateles: 53% (n=8, 95% CI= 487-58),
# Cebus: 89% (n=15, 95% CI= 86-92), Saimiri: 32% (n=5, 95% CI= 24-40)
#all species: 85% (n=59, CI= 83-87)


##### FINISH ############################################



