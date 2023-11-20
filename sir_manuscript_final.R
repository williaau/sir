#SIR manuscript - revised Sept 2023 
# 1) trends in SIR over time 
# 2) heat impacts 
# 3) COVID impacts 

sir_original <- read.csv("~/Library/CloudStorage/OneDrive-SUNYUpstateMedicalUniversity/OSH/occ_heat/January2015toApril2022.csv")

sir_full <- read.csv("~/Library/CloudStorage/OneDrive-SUNYUpstateMedicalUniversity/OSH/January2015toFebruary2023.csv")

library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)

library(tidyr)
library(weathermetrics)
library(Hmisc)
library(gam)
library(gamm4)
library(mgcv)
library(epitools)
library(stats)

#organize data
sir_full$ID<-as.factor(sir_full$ID)
sir_full$date<-as.POSIXct(strptime(sir_full$EventDate,format="%m/%d/%Y"))
sir_full$city<-as.factor(sir_full$City)
sir_full$state<-as.factor(sir_full$State)
sir_full$zip<-as.factor(sir_full$Zip)
sir_full$naics<-as.factor(sir_full$Primary.NAICS)
sir_full$nature<-as.factor(sir_full$Nature)
sir_full$event<-as.factor(sir_full$Event)
sir_full$bodypart<-as.factor(sir_full$Part.of.Body)
sir_full$source<-as.factor(sir_full$Source)

sir<-sir_full[,c(27,28,29,30,31,32, 33, 34, 35, 13, 14)] 
#keeps date, city, state, zip, naics, nature, event, bodypart, source, hospitalization, amputation

library(lubridate)

sir$year<-as.factor(year(sir$date))
sir$month<-as.factor(month(sir$date))
sir$dow<-as.factor(wday(sir$date))
sir$doy<-as.factor(yday(sir$date))
sir$warmseason<-as.factor(with(sir,ifelse((month==5| month==6| month==7| month==8| month==9),1,0)))

sir$number_of_injuries<-as.numeric(sir$Hospitalized)+(sir$Amputation)

sir$naics2<-sir$naics

#NAICS 
sir$naics6<-sir$naics
unique(sir$naics6)#1276 

sir$naics4<-substr(sir$naics,1,4)
sir$naics4<-as.factor(sir$naics4)
unique(sir$naics4)#326 

sir$naics2<-substr(sir$naics,1,2)
sir$naics2<-as.factor(sir$naics2)
unique(sir$naics2)#25 

    #first 2 digits 
    #11: ag, forest, fish
    #21: mining, quarry, oil and gas extract
    #22: utilities 
    #23: construction 
    #31-33: manufacturing 
    #42: wholesale trade 
    #44-45: retail trade
    #48-49: transport, warehouse
    #51: information 
    #52: finance, insurance 
    #53: real estate, rental, leasing
    #54: professional, scientific, technical services
    #55: management of companies, enterprises
    #56: admin, support, waste management, remediation 
    #61: education
    #62: health care and social assistance
    #71: arts, entertainment, rec
    #72: accommodation, food services
    #81: other (except public) 
    #92: public administration 

#nature -- Nature of Injury or illness - the physical characteristics of the disabling injury or illness, such as cuts and lacerations, fractures, sprains and strains, or electrocution(e.g., fractures, burns)
unique(sir$nature) #197

#event -- Event title - —the manner in which the injury or illness was produced or inflicted, such as caught in running equipment; slips, trips, or falls; overexertion; or contact with electrical current (e.g., injuried by physical contact, ignition of vapors)
unique(sir$event) #353

#source -- source of injury - the object, substance, exposure, or bodily motion that was responsible for producing or inflicting the disabling condition, such as machinery, ground, patient, or electrical wiring (e.g., coworker, blow torch)
unique(sir$source) #1082

#body part -- part of body affected - the part of body directly linked to the nature of injury or illness cited, such as finger, arm, back, or body systems (e.g., lower leg)
unique(sir$bodypart) #126

######
    #original analyses - subset to pre-COVID 
    #restrict years to 2015-2019 to remove any COVID impact 
    #sir_subset<-subset(sir,year==2015|year==2016|year==2017|year==2018|year==2019)

#instead, keep all years, 2015 - 2022 

#add climate zone 
#From NCDC: https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/regional_monitoring/regions.shtml
#Climate zones used in national summaries 
sir$climatezone<-0
sir$climatezone<-with(sir,ifelse((state=="WASHINGTON"|state=="OREGON"|state=="IDAHO"),"Northwest",climatezone))
sir$climatezone<-with(sir,ifelse((state=="MONTANA"|state=="WYOMING"|state=="NORTH DAKOTA"|state=="SOUTH DAKOTA"|state=="NEBRASKA"),"West North Central",climatezone))
sir$climatezone<-with(sir,ifelse((state=="CALIFORNIA"|state=="NEVADA"),"West",climatezone))
sir$climatezone<-with(sir,ifelse((state=="UTAH"|state=="COLORADO"|state=="ARIZONA"|state=="NEW MEXICO"),"Southwest",climatezone))
sir$climatezone<-with(sir,ifelse((state=="TEXAS"|state=="KANSAS"|state=="OKLAHOMA"|state=="ARKANSAS"|state=="LOUISIANA"|state=="MISSISSIPPI"),"South",climatezone))
sir$climatezone<-with(sir,ifelse((state=="MINNESOTA"|state=="IOWA"|state=="WISCONSIN"|state=="MICHIGAN"),"East North Central",climatezone))
sir$climatezone<-with(sir,ifelse((state=="MISSOURI"|state=="ILLINOIS"|state=="INDIANA"|state=="OHIO"|state=="KENTUCKY"|state=="TENNESSEE"|state=="WEST VIRGINIA"),"Central",climatezone))
sir$climatezone<-with(sir,ifelse((state=="ALABAMA"|state=="GEORGIA"|state=="FLORIDA"|state=="SOUTH CAROLINA"|state=="NORTH CAROLINA"|state=="VIRGINIA"),"Southeast",climatezone))
sir$climatezone<-with(sir,ifelse((state=="MARYLAND"|state=="DELAWARE"|state=="NEW JERSEY"|state=="PENNSYLVANIA"|state=="NEW YORK"|state=="VERMONT"|state=="CONNECTICUT"|state=="MASSACHUSETTS"|state=="NEW HAMPSHIRE"|state=="RHODE ISLAND"|state=="MAINE"),"Northeast",climatezone))
sir$climatezone<-with(sir,ifelse((state=="ALASKA"),"Alaska",climatezone))
sir$climatezone<-with(sir,ifelse((state=="HAWAII"),"Hawaii",climatezone))

#keep 50 states 
sir<-subset(sir,climatezone!=0)
#excludes American Samoa, DC, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands
#these aren't in an NCDC climate zone 

#fix zipcode and leading 0 
sir$zipcode<-as.character(sir$zip)
sir$zipcode<-with(sir,ifelse(nchar(zipcode)<5,paste0(0,zipcode),zipcode))
sir$zipcode<-as.factor(sir$zipcode)

#categories of OIICS variables 
sir$eventcat<-substr(sir$event,1,1)
sir$eventcat<-as.factor(sir$eventcat)

sir$naturecat<-substr(sir$nature,1,1)
sir$naturecat<-as.factor(sir$naturecat)

#because body part has a 9 and a 9999 code, need to differentiate 
sir$bodypart2<-sir$bodypart
sir$bodypart2<-as.character(sir$bodypart2)
sir$bodypart2<-with(sir,ifelse(bodypart2=="9999","non",bodypart2))
sir$bodypartcat<-substr(sir$bodypart2,1,1)
sir$bodypartcat<-as.factor(sir$bodypartcat)

sir$sourcecat<-substr(sir$source,1,1)
sir$sourcecat<-as.factor(sir$sourcecat)

#heat related as defined by source? 
sir$heat<-0
sir$heat<-with(sir,ifelse((source=="926" | source=="9260"| source=="9262"),1,heat))

#COVID time period 
sir$month_year<-paste0(sir$month,"/", sir$year)
sir$covid<-0 
sir$covid<-with(sir, ifelse(c(month_year=="3/2020" | month_year=="4/2020" | month_year=="5/2020" | month_year=="6/2020" | 
                                month_year=="7/2020" | month_year=="8/2020" | month_year=="9/2020" | month_year=="10/2020" | 
                                month_year=="11/2020" | month_year=="12/2020" | month_year=="1/2021" | month_year=="2/2021" | 
                                month_year=="3/2021" |month_year=="4/2021" |month_year=="5/2021" |month_year=="6/2021" |
                                month_year=="7/2021" |month_year=="8/2021" |month_year=="9/2021" |month_year=="10/2021" |
                                month_year=="11/2021" |month_year=="12/2021" |month_year=="1/2022" |month_year=="2/2022" |
                                month_year=="3/2022" |month_year=="4/2022" |month_year=="5/2022" |month_year=="6/2022" |
                                month_year=="7/2022" |month_year=="8/2022" |month_year=="9/2022" |month_year=="10/2022" |
                                month_year=="11/2022" |month_year=="12/2022" |month_year=="1/2023" |month_year=="2/2023"),1,covid))
sir$covid_peak<-0
sir$covid_peak<- with(sir, ifelse(c(month_year=="3/2020" | month_year=="4/2020" | month_year=="5/2020" | month_year=="6/2020" | 
                                      month_year=="7/2020" | month_year=="8/2020" | month_year=="9/2020" | month_year=="10/2020" | 
                                      month_year=="11/2020" | month_year=="12/2020" | month_year=="1/2021" | month_year=="2/2021" | 
                                      month_year=="3/2021" |month_year=="4/2021" |month_year=="5/2021" |month_year=="6/2021" |
                                      month_year=="7/2021" |month_year=="8/2021" |month_year=="9/2021" |month_year=="10/2021" |
                                      month_year=="11/2021" |month_year=="12/2021" |month_year=="1/2022" |month_year=="2/2022" |
                                      month_year=="3/2022"),1,covid_peak))

#expand rows based on # of injuries
sir_repeats<-sir
sir_repeats$nb_times<-as.integer(sir_repeats$number_of_injuries,na.rm=TRUE)
sir_repeats$nb_times<-with(sir_repeats,ifelse(is.na(nb_times),1,nb_times))

sir_expanded<-data.frame(lapply(sir_repeats,rep,sir_repeats$nb_times))
sir_expanded$count<-1

sir_expanded<-subset(sir_expanded,year!="2023")
sir2<-complete(sir_expanded)

write.csv(sir_expanded,"sir_expanded_nov18.csv")

#add in state plan state info 
#0=fed osha state
#1=some type of state plan 
sir_expanded$stateplan2<-0
sir_expanded$stateplan2<-with(sir_expanded,ifelse((state=="WASHINGTON"|state=="OREGON"|state=="CALIFORNIA"|state=="NEVADA"|state=="ARIZONA"|state=="UTAH"|state=="WYOMING"|state=="NEW MEXICO"|state=="MINNESOTA"|state=="IOWA"|state=="ILLINOIS"|state=="MICHIGAN"|state=="INDIANA"|state=="KENTUCKY"|state=="TENNESSEE"|state=="SOUTH CAROLINA"|state=="NORTH CAROLINA"|state=="VIRGINIA"|state=="MARYLAND"|state=="NJ"|state=="NEW JERSEY"|state=="NEW YORK"|state=="MASSACHUSETTS"|state=="VERMONT"|state=="MAINE"|state=="HAWAII"|state=="ALASKA"),1,stateplan2))

table(sir_expanded$stateplan2)

#add in more detailed state plan state info 
#0=fed osha state
#1=public only state plan (IL, NY, ME, MA, CT, NJ)
#2=private + public state plan 
sir_expanded$stateplan3<-0
sir_expanded$stateplan3<-with(sir_expanded,ifelse((state=="ILLINOIS"|state=="NEW JERSEY"|state=="CONNECTICUT"|state=="NEW YORK"|state=="MASSACHUSETTS"|state=="MAINE"),1,stateplan3))
sir_expanded$stateplan3<-with(sir_expanded,ifelse((state=="WASHINGTON"|state=="OREGON"|state=="CALIFORNIA"|state=="NEVADA"|state=="ARIZONA"|state=="UTAH"|state=="WYOMING"|state=="NEW MEXICO"|state=="MINNESOTA"|state=="IOWA"|state=="MICHIGAN"|state=="INDIANA"|state=="KENTUCKY"|state=="TENNESSEE"|state=="SOUTH CAROLINA"|state=="NORTH CAROLINA"|state=="VIRGINIA"|state=="MARYLAND"|state=="VERMONT"|state=="HAWAII"|state=="ALASKA"),2,stateplan3))

table(sir_expanded$stateplan3)

#restrict to those states under federal OSHA coverage
sir_fed<-subset(sir_expanded, sir_expanded$stateplan3==0 | sir_expanded$stateplan3==1)

sir_fed$covid3<-"Pre-COVID-19 Pandemic"
sir_fed$covid3<-with(sir_fed, ifelse(c(month_year=="3/2020" | month_year=="4/2020" | month_year=="5/2020" | month_year=="6/2020" | 
                                month_year=="7/2020" | month_year=="8/2020" | month_year=="9/2020" | month_year=="10/2020" | 
                                month_year=="11/2020" | month_year=="12/2020" | month_year=="1/2021" | month_year=="2/2021" | 
                                month_year=="3/2021" |month_year=="4/2021" |month_year=="5/2021" |month_year=="6/2021" |
                                month_year=="7/2021" |month_year=="8/2021" |month_year=="9/2021" |month_year=="10/2021" |
                                month_year=="11/2021" |month_year=="12/2021" |month_year=="1/2022" |month_year=="2/2022" |
                                month_year=="3/2022"),"Peak COVID-19 Pandemic",covid3))
sir_fed$covid3<-with(sir_fed, ifelse(c(month_year=="4/2022" |month_year=="5/2022" |month_year=="6/2022" |
                                month_year=="7/2022" |month_year=="8/2022" |month_year=="9/2022" |month_year=="10/2022" |
                                month_year=="11/2022" |month_year=="12/2022" |month_year=="1/2023" |month_year=="2/2023"),"Post-COVID-19 Peak",covid3))
sir_fed$covid3<-as.factor(sir_fed$covid3)

#analyses 
sum(sir_fed$number_of_injuries,na.rm=T)
sum(sir_fed$count)

sum(sir_fed$count[sir_fed$Amputation==1],na.rm=TRUE)
sum(sir_fed$count[sir_fed$Hospitalized==1],na.rm=TRUE)

#2921 days in this period 
#daily mean SIR = 
83338/2921

#overall injury rate 
83338 / 73844325 *100000

#average annual injury rate
83338 / 73844325 / 7 *100000


sir_fed$naics2_name<-0
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==11, "Agriculture, Forestry, Fishing and Hunting",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==21, "Mining, Quarrying, and Oil and Gas Extraction",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==22, "Utilities",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==23, "Construction",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse((naics2==31 | naics2==32 | naics2==33), "Manufacturing ",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==42, "Wholesale Trade",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse((naics2==44 | naics2==45), "Retail Trade ",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse((naics2==48 | naics2==49), "Transportation and Warehousing ",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==51, "Information",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==52, "Finance and Insurance",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==53, "Real Estate and Rental and Leasing",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==54, "Professional, Scientific, and Technical Services",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==55, "Management of Companies and Enterprises",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==56, "Administrative and Support and Waste Management and Remediation Services",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==61, "Educational Services",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==62, "Health Care and Social Assistance",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==71, "Arts, Entertainment, and Recreation",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==72, "Accommodation and Food Services",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==81, "Other Services (except Public Administration)",naics2_name))
sir_fed$naics2_name<-with(sir_fed,ifelse(naics2==92, "Public Administration (not covered in economic census)",naics2_name))

round(prop.table(table(sir_fed$naics2_name))*100,1)
round(prop.table(table(sir_fed$eventcat))*100,1)
round(prop.table(table(sir_fed$bodypartcat))*100,1)
round(prop.table(table(sir_fed$naturecat))*100,1)
  #what is common within traumatic injuries? 
round(prop.table(table(sir_fed$nature))*100,1)
  #111 - 29.8% - Fractures
  #1972 - 7.0% - Soreness, pain, hurt—nonspecified injury
round(prop.table(table(sir_fed$climatezone))*100,1)
round(prop.table(table(sir_fed$year))*100,1)

sir_yearly<-ddply(sir_fed,.(year,month),summarise,num_injuries=sum(count))
sir_yearly$month_name<-0
sir_yearly$month_name<-with(sir_yearly,ifelse(month==1,"Jan",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==2,"Feb",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==3,"Mar",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==4,"Apr",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==5,"May",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==6,"Jun",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==7,"Jul",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==8,"Aug",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==9,"Sep",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==10,"Oct",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==11,"Nov",month_name))
sir_yearly$month_name<-with(sir_yearly,ifelse(month==12,"Dec",month_name))
sir_yearly$month_date<-paste0(sir_yearly$month_name,sep="-",sir_yearly$year)
#sir_yearly$month_date<-(strptime(sir_yearly$month_date,format="%b-%Y"))
sir_yearly$seq<-1:96
st <- ymd("2015-01-01")
en <- ymd("2022-12-01")
sir_yearly$month_date<-st %m+% months(seq(0, round(interval(st, en) / months(1)), 1))

#keep months in order 
sir_yearly$month_name<-factor(sir_yearly$month_name, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#make a single average for each month 
sir_monthly<-ddply(sir_fed,.(month),summarise,num_injuries=sum(count),avg_injuries=(sum(count))/7)

sir_monthly$month<-with(sir_monthly,ifelse(month==1,"Jan",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==2,"Feb",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==3,"Mar",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==4,"Apr",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==5,"May",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==6,"Jun",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==7,"Jul",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==8,"Aug",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==9,"Sep",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==10,"Oct",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==11,"Nov",month))
sir_monthly$month<-with(sir_monthly,ifelse(month==12,"Dec",month))
sir_monthly$month<-factor(sir_monthly$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#figure 2 
ggplot(sir_monthly,aes(month, avg_injuries,group=1)) + 
  geom_line(size=1.5)+geom_point(size=2.5)+
  # scale_linetype_manual(values=("solid"))+
  theme_classic(base_size=35)+labs(y="Average Number of Severe Injuries",x="Month",linetype="month")
  # theme(legend.key.width=unit(2,'cm'))
ggsave("fig2-final.jpeg",plot=last_plot(),width=20,height=10,dpi=300)

#Alternate Figure 2 - faceted by year 
ggplot(sir_yearly,aes((month_name),num_injuries,group=factor(year))) + 
  geom_line(size=1.5)+geom_point(size=2.5)+
  #scale_color_manual(values=c("black", "black","black","black","azure4","azure4","azure4","azure4"))+
  #scale_linetype_manual(values=c("solid","longdash","dotted","twodash","solid","longdash","dotted","twodash"))+
  theme_classic(base_size=35)+labs(y="Number of Severe Injuries",x="Month",linetype="Year")+
  theme(legend.key.width=unit(2,'cm'))+facet_wrap(~year,ncol=1)
ggsave("fig2-alt1-final.jpeg",plot=last_plot(),width=20,height=40,dpi=300)

#Alternate Figure2 - too many lines on one graph 
ggplot(sir_yearly,aes(month_name,num_injuries,group=factor(year),linetype=factor(year),color=factor(year))) +
  geom_line(size=1.5)+geom_point(size=2.5)+
  scale_color_manual(values=c("black", "black","black","black","azure4","azure4","azure4","azure4"))+
  scale_linetype_manual(values=c("solid","longdash","dotted","twodash","solid","longdash","dotted","twodash"))+
  theme_classic(base_size=35)+labs(y="Number of Severe Injuries",x="Month",linetype="Year")+
  theme(legend.key.width=unit(2,'cm'))
ggsave("fig2-alt2-final.jpeg",plot=last_plot(),width=20,height=10,dpi=300)

#Figure 4a
ggplot(sir_yearly,aes(month_date,num_injuries)) + 
  geom_line(size=1.25)+geom_point(size=2.5)+
  theme_classic(base_size=15)+labs(y="Number of Severe Injuries",x="Month")+
  scale_x_date(date_breaks="2 months")+
  theme(axis.text.x = element_text(angle=-25))
ggsave("fig4a-final.jpeg",plot=last_plot(),width=20,height=10,dpi=300)

#Figure 5
pre_post<-ddply(sir_fed,.(covid3,naics2_name),summarise,num_injury=sum(count),avg_injuries=(sum(count))/7)
write.csv(pre_post,"pre_post.csv")
pre_post_wider<-pivot_wider(pre_post,names_from="covid3",values_from="num_injury")
write.csv(pre_post_wider,"pre-post_wider.csv")
  #remove weird cell, reimport
pre_post_wider <- read.csv("~/Library/CloudStorage/OneDrive-SUNYUpstateMedicalUniversity/Manuscripts/SIR/sir/pre-post_wider.csv")
pre_post_wider$difference<-pre_post_wider$Pre.COVID.19.Pandemic/pre_post_wider$Peak.COVID.19.Pandemic

pre_post$covid3<-factor(pre_post$covid3, levels = c("Pre-COVID-19 Pandemic","Peak COVID-19 Pandemic","Post-COVID-19 Peak"))
pre_post$naics2_name<-factor(pre_post$naics2_name,levels=c("Ag, Forest, Fish, Hunting","Mining, Quar, Oil, Gas Extract.","Utilities","Construction","Manufact","Wholesale Trade","Retail Trade","Trans, Warehouse","Information","Fin., Insur.","Real Estate, Rental, Leas.","Prof, Sci, & Tech Srvs","Mngmt of Comp & Enterp.","Admin, Support, Waste Mngmt","Edu Services","Health Care & Social Assist","Arts, Ent, Rec","Accom, & Food Srvs","Other Servs","Public Admin","Missing"))

ggplot(pre_post,aes(naics2_name,num_injury,fill=covid3,group=covid3))+
  geom_bar(position="dodge",stat="identity")+
  theme_classic(base_size=15)+labs(y="Number of Severe Injuries",x="Industry")+
  theme(axis.text.x = element_text())
ggsave("fig5-final.jpeg",plot=last_plot(),width=20,height=10,dpi=300)
#reorged in excel 

#stats test
pre_post_perc_cast <- read_excel("pre-post-perc-cast.xlsx")

prop.test(pre_post_perc_cast$percent, as.factor(pre_post_perc_cast$covid3))
one.way <- aov(percent ~ as.factor(covid3), data = pre_post_perc_cast)
summary(one.way)
two.way <- aov(percent ~ as.factor(covid3)+as.factor(naics2_name), data = pre_post_perc_cast)
summary(two.way)

pre_post_perc_cast$naics2_name_num<-as.numeric(as.factor(pre_post_perc_cast$naics2_name))
pre_post_perc_cast$covid3_num<-as.numeric(as.factor(pre_post_perc_cast$covid3))
three.way <-aov(percent ~ (covid3_num)*(naics2_name_num), data = pre_post_perc_cast)
summary(three.way)

#seasonality 
round(prop.table(table(sir_fed$warmseason))*100,1)
chisq.test(sir_fed$warmseason,sir_fed$number_of_injuries)

season<-ddply(sir_fed,.(warmseason,naics2_name,eventcat,naturecat,bodypartcat,climatezone),summarise,num_injuries=sum(count),avg_injuries=(sum(count))/7)
#season_sub<-subset(season,(season$naics2_name=="Manufacturing"| season$naics2_name=="Construction" | season$naics2_name=="Transportation and Warehousing"| season$naics2_name=="Retail Trade"))
# ggplot(season,aes(naics2_name,num_injuries))+
#   geom_col()+facet_wrap(~warmseason)+
#   theme_classic(base_size=15)+labs(y="Number of Severe Injuries",x="Industry")+
#   theme(axis.text.x=element_text(angle=-25))
wilcox.test(number_of_injuries~ warmseason, data = sir_fed, exact = FALSE)

prop.table(table(season$warmseason,season$naics2_name))*100
chisq.test(season$warmseason,season$naics2_name)
prop.table(table(season$warmseason,season$eventcat))*100
chisq.test(season$warmseason,season$eventcat)
prop.table(table(season$warmseason,season$naturecat))*100
chisq.test(season$warmseason,season$naturecat)
prop.table(table(season$warmseason,season$bodypartcat))*100
chisq.test(season$warmseason,season$bodypartcat)

sum(sir_fed$count[sir_fed$heat==1])
sum(sir_fed$count[sir_fed$heat==0])
wilcox.test(number_of_injuries~ heat, data = sir_fed, exact = FALSE)

#what about within heat 
sir_fed_heat<-subset(sir_fed,heat==1)
table(sir_fed_heat$climatezone)
round(prop.table(table(sir_fed_heat$climatezone))*100,1)
table(sir_fed_heat$nature) #all heat specific 
table(sir_fed_heat$event) #exposures to temp extremes 
table(sir_fed_heat$bodypart2) #most are body system, 4-313, 1-130
table(sir_fed_heat$naics2_name)
round(prop.table(table(sir_fed_heat$naics2_name))*100,1)

table(sir_fed_heat$naics6)
table(sir_fed_heat$naics4)

#Covid
round(prop.table(table(sir_fed$covid))*100,1)
round(prop.table(table(sir_fed$covid_peak))*100,1)
sum(sir_expanded$count[sir_expanded$covid==1])
sum(sir_expanded$count[sir_expanded$covid_peak==1])
wilcox.test(number_of_injuries~ covid_peak, data = sir_fed, exact = FALSE)

sir_fed_beforecovid<-subset(sir_fed,covid==0)
sir_fed_covid<-subset(sir_fed,covid==1)
sir_fed_covid_peak<-subset(sir_fed,covid_peak==1)

table(sir_fed_beforecovid$climatezone)
table(sir_fed_covid$climatezone)
table(sir_fed_covid_peak$climatezone)

table(sir_fed_beforecovid$naics2_name)
table(sir_fed_covid$naics2_name)
table(sir_fed_covid_peak$naics2_name)

#map - injury rates by state 
state<-ddply(sir_fed,.(state),summarise,num_injuries=sum(count),avg_injuries=(sum(count))/7)
write.csv(state,"state.csv")


#covid - what industries saw a decline post-covid start 




