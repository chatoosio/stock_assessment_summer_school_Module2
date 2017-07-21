#' ---
#' title: "Reference Points with FLBRP, Summer School in Quantative Fisheries Stock Assessment, Capo Granitola"
#' author: "Alessandro Ligas"
#' date: "July 13th, 2017"
#' ---


# load the library
library(FLBRP) 


# Load the stock object 
# we are going to use one of the stock objects from the HKE_09_10_11 stock assessment
load("HKE9_10_11.xsa.Rdata")

# let's rename it:
hke<-HKE.stk

# Create the corresponding FLBRP object 
hkebrp <- FLBRP(hke) 

summary(hkebrp)

# The FLBRP class has information on: 

# selection Patterns 
catch.sel(hkebrp) 
# discards.sel(hkebrp)  

ggplot(catch.sel(hkebrp), aes( age, data))+geom_point()
# ggplot(discards.sel(hkebrp), aes( age, data))+geom_point() 


# mass-at-age 
stock.wt(hkebrp) 
catch.wt(hkebrp) 
# discards.wt(hkebrp) 

# biological parameters 
m(hkebrp) 
mat(hkebrp) 


# and other quantities by age 
xyplot(data~age|qname, data=FLQuants(swt=stock.wt(hkebrp), cwt =catch.wt(hkebrp), mat= mat(hkebrp), m = m(hkebrp)), type="l",scale="free") 


# we have not provided a SR relationship yet
# so analyses will be per-recruit 


# All *.obs slots hold the observations from FLStock 
fbar.obs(hkebrp) 

# Once an FLBRP object has been created then equilibrium 
# quantities can be estimated 

# we estimate equilibrium quantities 
hkebrp <- brp(hkebrp) 


# and we get a table of reference points 
refpts(hkebrp) 


plot(refpts(hkebrp)) 
# In this case, Fmsy is the same as Fmax, since the assumed stock recruitment 
# relationship is mean recruitment 


refpts(hkebrp)[c('msy', ('fmax')), ] 


# Thus plotting the reference points and expected quantities 
plot(hkebrp) 


# The derived reference points would be used to compare the fishing mortality (from Fbar) with the  
# reference point of choice. 
fbar(hke)[,"2014"]  
refpts(hkebrp)['f0.1','harvest'] 


# F/F0.1 ? 
fbar(hke)[,"2014"]  / refpts(hkebrp)['f0.1','harvest']  


# SR 
 
# Now we provide a stock-recruit relationship

model = "geomean"
srr <- fmle(as.FLSR(hke, model = model))

#Reference points
hkebrpgm <- brp(FLBRP(hke, sr = srr))
ref_points<- refpts(hkebrpgm)
ref_points
# Also in this case, Fmsy is the same as Fmax, since the assumed stock recruitment 
# relationship is the geometric mean of recruitment 


plot(refpts(hkebrpgm)) 

plot(hkebrpgm)

rp_table = data.frame(ref_points@.Data)[,1:5]
temp <- rownames(rp_table)
rp_table = data.frame(rp_table, row.names = NULL)
rp_table <- cbind(temp, rp_table)
colnames(rp_table) =c("", "F","Total Yield","Recruitment","SSB","Biomass")

write.table(rp_table,file="Ref_points_HKE.csv",sep=";",row.names=FALSE, col.names=T)

################################################

# Let's add a SR relationship (Beverton and Holt)

hkesr <- as.FLSR(hke, model=bevholt) 
hkesr <- fmle(hkesr) 

plot(hkesr) 

# and provide it when constructing FLBRP 
hkebrp <- FLBRP(hke, sr=hkesr) 

# let's have a look at the formula
model(hkebrp)
# and parameters a and b
params(hkebrp) 


# and we refit FLBRP 
hkebrp <- brp(hkebrp) 


# and see the difference in RPs 
refpts(hkebrp) 


# and relationships 
plot(hkebrp) 


# EXERCIZE 01
# Make a sensitivity analysis on a range of M to see how it affects the estimation of the main reference point(F0.1).
hkebrpa <- hkebrp
m(hkebrpa)<-c(1.38,	0.56,	0.27,	0.22,	0.19,	0.18,	0.17) # borrowed from GSA16

hkebrpb <- hkebrp
m(hkebrpb)<-c(1.03,	0.51,	0.33,	0.26,	0.22,	0.2, 0.2) # borrowed from GSA7






