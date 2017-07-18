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


# selectivity 
xyplot(data~age,data=catch.sel(hkebrp),type=c('l', 'p')) 


# and other quantities by age 
xyplot(data~age|qname, data=FLQuants(sel=catch.sel(hkebrp), 
                                     swt=stock.wt(hkebrp), 
                                     cwt =catch.wt(hkebrp), mat= mat(hkebrp), m = m(hkebrp)), 
       type="l",scale="free") 


# we have not provided a SR relationship yet
# so analyses wll be per-recruit 


# All *.obs slots hold the observations from FLStock 
fbar.obs(hkebrp) 

# Once an FLBRP object has been created then equilibrium 
# quantities can be estimated 

# we estimate equilibrium quantities 
hkebrp <- brp(hkebrp) 

# and a set of equilibirum quantities for a range of F values 

# fishing mortality  
fbar(hkebrp) 
harvest(hkebrp) 

# abundance-at-age 
stock.n(hkebrp) 

# catch-at-age 
catch.n(hkebrp) 

# plus some age-aggregated values 
yield.hat(hkebrp) 


# mean recruitment 
rec.hat(hkebrp) 


# and we get a table of reference points 
refpts(hkebrp) 


plot(refpts(hkebrp)) 
# In this case, Fmsy is the same as Fmax, since the assumed stock recruitment 
# relationship is mean recruitment 


refpts(hkebrp)[c('msy', ('fmax')), ] 


# Thus plotting the reference points ans expected quantities 
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

# we can add a SR fitted model 

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



