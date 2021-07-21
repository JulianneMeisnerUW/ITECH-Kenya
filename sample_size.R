library(dplyr)
library(samplingbook)

sc_data<-read.csv("subcounty_pop.csv")
sc_data$subcounty<-sc_data$Sub.County.Total..................Male..............Female...................Total.....Conventional
sc_data<-sc_data[,c(1,3,81)]
sc_data$Total<-as.numeric(gsub(",","",sc_data$Total))
sc_data$subcounty_unique<-paste(sc_data$County, sc_data$subcounty, sep="_")

county_data<-sc_data%>%
  group_by(County)%>%summarize(population=sum(Total, na.rm=T))%>%as.data.frame()

#2014 DHS:
#-Sampling frame: Fifth National Sample Survey and Evaluation Programme (NASSEP V) (developed 2012)
#-5,360 clusters split into four equal subsample (draw from EAs in the 2009 Census)


#2009 Kenya Population and Housing Census. 
#-96,251 enumeration areas (EAs) 

#Nairobi and Mombasa are urban only

#https://www.knbs.or.ke/?cat=469
#-Seems like there were 130,000 EAs in the 2019 Population and Housing Census

#Number of households

#2.5% are over 65: https://data.worldbank.org/indicator/SP.POP.65UP.TO.ZS?locations=KE
#Assume additional 1% when you add in 58-64 and comorbidities

#https://mics.unicef.org/files?job=W1siZiIsIjIwMTUvMDQvMDIvMDgvMDAvMTkvODEwL01JQ1MzX0NoYXB0ZXJfNF9fX0Rlc2lnbmluZ19hbmRfU2VsZWN0aW5nX3RoZV9TYW1wbGVfMDYwMjE5LnBkZiJdXQ&sha=3d97a05358bb0e37

#Step 1: number of households to sample
households<-function(r, f, p, nh, prec, nonresp){
  nr<-1+nonresp
  num<-4*r*(1-r)*f*nr
  denom<-((prec*r)^2)*p*nh
  res<-num/denom
  return(res)
}
#From: https://mics.unicef.org/files?job=W1siZiIsIjIwMTUvMDQvMDIvMDgvMDAvMTkvODEwL01JQ1MzX0NoYXB0ZXJfNF9fX0Rlc2lnbmluZ19hbmRfU2VsZWN0aW5nX3RoZV9TYW1wbGVfMDYwMjE5LnBkZiJdXQ&sha=3d97a05358bb0e37

r<-0.25 #anticipated vaccine coverage in this population
f<-1.3 #design effect, conservative estimate for individual-level indicator
p<-0.035 #proportion of the population in >58 or with comorbidities 
nh<-4.5 #average household size
prec<-0.1 #desired precision
nonresp<-0.05

ssize<-households(r, f, p, nh, prec, nonresp)

#Option 1: allocate households to subcounties
sc_nonmiss<-sc_data[which(!is.na(sc_data$Total)),]
pops<-sc_nonmiss$Total

sc_samples<-t(stratasamp(n=ssize, Nh=pops, type='prop'))
sc_nonmiss$sample_households<-sc_samples[,2]
sc_nonmiss$PSUs<-sc_nonmiss$sample_households/10

write.csv(file="subcounties.csv", sc_nonmiss)

#Option 2: allocate households to subcounties
pops<-county_data$population
c_samples<-t(stratasamp(n=ssize, Nh=pops, type='prop'))
county_data$sample_households<-c_samples[,2]
county_data$PSUs<-county_data$sample_households/10

write.csv(file="counties.csv", county_data)

#Step 3: decide on number of PSUs
#Same number of households per PSU (10 households each?), so just comes down to logistics

#https://nces.ed.gov/FCSM/pdf/2009FCSM_Krenzke_IX-C.pdf