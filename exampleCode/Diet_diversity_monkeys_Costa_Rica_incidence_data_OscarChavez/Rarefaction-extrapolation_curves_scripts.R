Sys.time()
[1] "2022-06-18 15:02:44 CST"

#import the programs

library(iNEXT) 
library(ggplot2) 
library(devtools)
library( httr)
library(grid) # to configure the size of points and lines of the final graphs


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

   #Step 2: covert the data to a list of 4 matrices to run iNEXT


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

diet.diver = list(Alouatta = diet1, Ateles = diet2, Cebus = diet3, Saimiri = diet4)



# considering the four monkey species pooled or gamma diversity

diet.all = list(all = diet5)



######################################


    # Step 4) Customize the final figures for publication

m <- c(10, 20, 30, 40, 50, 60, 100)#optative argument 'size=m' in iNEXT ()


#fig alpha diversity 

out1 <- iNEXT(diet.diver, datatype="incidence_raw")


fig.alpha1= ggiNEXT(out1, type=1)+
  theme_bw(base_size = 18) + theme(legend.position="none")+
  scale_shape_manual(values=c(19, 15, 17,18))+
  scale_colour_manual(values=c("gray30","brown1", "deepskyblue","darkgoldenrod"))+
  scale_fill_manual (values=c("gray30","brown1", "deepskyblue","darkgoldenrod")) +
  
  
  ##adding axis titles
  #labs (x="Number of monkey groups")+
  
  labs (x=NULL, y=NULL)+
  
  #custumize the graph text
  theme(axis.title.y=element_text(size=17.5, vjust =1, colour="black"))+
  theme(axis.title.x=element_text(size=17.5, vjust=1, colour="black"))+
  theme(axis.ticks.length = unit(0.15, "cm"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  
  
  #to see and customize the legend with the name of each curve you can use:

  #theme(legend.position = "bottom")+
  #theme(legend.text=element_text(size=10),
  #legend.title=element_text(size=0, color="black"))+
  
  #However, as the graph will be edited in the program Inskcape or photoshop,
  #I prefer do not include the legend in these figures.
  
  
  ##adjusting the margins and position of axis titles
  theme(plot.margin = unit(c(0.8,0.2,0.3,0.3), "cm"))


fig.alpha1c <- ggplot_build(fig.alpha1)
fig.alpha1c$data[[1]]$size <- 0 #to change the point size (default size is 5)
fig.alpha1c$data[[2]]$size <- 1.2 # to change line size (default size is 5)
fig.alpha1c <- ggplot_gtable(fig.alpha1c)

grid.draw(fig.alpha1c)



################

# fig alpha diversity by order 1

out2 <- iNEXT(diet.diver, q= 1, datatype="incidence_raw")

fig.alpha2= ggiNEXT(out2, type=1)+
  theme_bw(base_size = 18) + theme(legend.position="none")+
  scale_shape_manual(values=c(19, 15, 17,18))+
  scale_colour_manual(values=c("gray30","brown1", "deepskyblue","darkgoldenrod"))+
  scale_fill_manual (values=c("gray30","brown1", "deepskyblue","darkgoldenrod")) +
  
  #set the y-axis in the same scale that the species richness q=0. 
  
  scale_y_continuous(name=NULL, limits=c(0, 600))+
  
  
  labs (x=NULL, y=NULL)+
  
  #custumize the graph text
  theme(axis.title.y=element_text(size=17.5, vjust =1, colour="black"))+
  theme(axis.title.x=element_text(size=17.5, vjust=1, colour="black"))+
  theme(axis.ticks.length = unit(0.15, "cm"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  
  
  
  ##adjusting the margins and position of axis titles
  theme(plot.margin = unit(c(0.8,0.2,0.3,0.3), "cm"))


#Customizing point/line size using 'ggplot_built' function

fig.alpha2c <- ggplot_build(fig.alpha2)
fig.alpha2c$data[[1]]$size <- 0 #to change the point size (default size is 5)
fig.alpha2c$data[[2]]$size <- 1.2 # to change line size (default size is 5)
fig.alpha2c <- ggplot_gtable(fig.alpha2c)

grid.draw(fig.alpha2c)



##################################


  # fig alpha diversity by order 2

out3 <- iNEXT(diet.diver, q= 2, datatype="incidence_raw")

fig.alpha3= ggiNEXT(out3, type=1)+
  theme_bw(base_size = 18) + theme(legend.position="none")+
  scale_shape_manual(values=c(19, 15, 17,18))+
  scale_colour_manual(values=c("gray30","brown1", "deepskyblue","darkgoldenrod"))+
  scale_fill_manual (values=c("gray30","brown1", "deepskyblue","darkgoldenrod")) +
  
  #set the y-axis in the same scale that the species richness q=0. 
  
  scale_y_continuous(name=NULL, limits=c(0, 600))+
  
  
  labs (x=NULL, y=NULL)+
  
  #custumize the graph text
  theme(axis.title.y=element_text(size=17.5, vjust =1, colour="black"))+
  theme(axis.title.x=element_text(size=17.5, vjust=1, colour="black"))+
  theme(axis.ticks.length = unit(0.15, "cm"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  
  
  ##adjusting the margins and position of axis titles
  theme(plot.margin = unit(c(0.8,0.2,0.3,0.3), "cm"))


#Customizing point/line size using 'ggplot_built' function

fig.alpha3c <- ggplot_build(fig.alpha3)
fig.alpha3c$data[[1]]$size <- 0 #to change the point size (default size is 5)
fig.alpha3c$data[[2]]$size <- 1.2 # to change line size (default size is 5)
fig.alpha3c <- ggplot_gtable(fig.alpha3c)

grid.draw(fig.alpha3c)


# print the three graphs as a single figure

library(ggpubr)

ggarrange(fig.alpha1c, fig.alpha2c, fig.alpha3c,ncol=1,nrow=3)
ggsave("Fig_alpha.tiff", width = 14, height = 26, units="cm", dpi=600)



############################
########################



#Now, we will construct a independe figure for gamma diversity (i.e. the plant meta-community diversity considering all the study groups)


#we can also to contruct add other two curves with the diversity 
#of genera and families

out.g <- iNEXT(diet.all, datatype="incidence_raw")

fig.gamma= ggiNEXT(out.g, type=1)+
  theme_bw(base_size = 18) + theme(legend.position="none")+
  scale_shape_manual(values=c(19, 15, 17,18))+
  scale_colour_manual(values=c("palegreen4"))+
  scale_fill_manual (values=c("palegreen4")) +
  
  #set the y-axis in the same scale that the species richness q=0. 
  
  scale_x_continuous(name="Number of study groups", limits=c(0, 120))+
  
  scale_y_continuous(name="Gamma diversity of plants in the diet", limits=c(0, 1200))+
  
  ##adding axis titles
  labs (x="Number of study groups",y="Gamma diversity of plants in the diet")+
  
  #labs (x=NULL, y=NULL)+
  
  #custumize the graph text
  theme(axis.title.y=element_text(size=17.5, vjust =1, colour="black"))+
  theme(axis.title.x=element_text(size=17.5, vjust=1, colour="black"))+
  theme(axis.ticks.length = unit(0.15, "cm"))+
  theme(axis.text.x = element_text(size=15, colour="black"))+
  theme(axis.text.y = element_text(size=15, colour="black"))+
  
  
  ##adjusting the margins and position of axis titles
  theme(plot.margin = unit(c(0.8,0.7,0.6,0.5), "cm"))


#Customizing point/line size using 'ggplot_built' function

fig.gammac <- ggplot_build(fig.gamma)
fig.gammac$data[[1]]$size <- 0 #to change the point size (default size is 5)
fig.gammac$data[[2]]$size <- 1.2 # to change line size (default size is 5)
fig.gammac <- ggplot_gtable(fig.gammac)

grid.draw(fig.gammac)

ggsave("Fig_gamma.tiff", width = 17, height = 16, units="cm", dpi=600)

#This graph can be edited in the program Inskcape to add the the symbols and other
#interesting information (e.g. sample coverage, sample size, etc.)

##################################FINISH ##################################



