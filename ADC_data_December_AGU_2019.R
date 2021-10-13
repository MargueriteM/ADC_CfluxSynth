##############################################################################
#             ADC Data: Northern Permafrost CO2 flux synthesis               #
#    Preliminary data preparation and exploration for October Workshop       #
#                  written by: Marguerite Mauritz, October 2018              #
#    data contributed by: many (see siteinfo Citation and Data_contributor)  #
#    data prepared by: Anna Virkkala and Marguerite Mauritz                  #
##############################################################################

# Data INFO: 
# NEE convention: postive values are release, negative values are uptake
# start year is incomplete for Chamber data

# import libraries
library(data.table)
library(ggplot2)
library(sp)
library(maps)
library(raster)


fig_dir <- ("/Users/memauritz/Desktop/SchuurLab/AGU_2019/Figures")

# edit log: 
# 6 October 2018: import initial merged version of all data
#   *replace all ',' with ';'
#   *make sure excel files don't have special symbols, eg: # or '
#   *save as .csv UTF=8 encoding to read umlauts, and then use UTF-8 encoding to read file in R
#   *date columns - split into month and year
#   *Interval and duration column updated with info for all rows possible.

# 28 October 2018: 
#   * fixed some errors in Duration/Interval/Date columns because some of the Annuals were wrong

# ISSUES: 
# * Site overlap in some cases hard to determine
# * overlap in time. Some provided Winter, Summer, Annual for same site and year
# * lots of missing metadata. Almost always got the required data
# * Depth of measurement  (eg: Soil_moisture_depth, Perc_C_depth) reported in a range, how to get around this?
# * Veg information incomplete
# * SOL_depth_cm: variable reporting
# * Permafrost: Yes/No is incomplete

# * T_air_ann has an error somewhere. One Temp is > 200! 
# * GPP has some sign errors (have +ve and -ve values, and that doesn't make sense)

# setwd
setwd('/Users/memauritz/Desktop/SchuurLab/NACP/ADC_DataAggregation_December/R')



# define the na strings
na.strings.data <- c("NA","NaN","","missing","?","-","no aggregate data")

# Tower: time varying data: use fread which gets rid of so many issues from read.table
data_t <- as.data.table(fread(file="CO2flux_ADC_Synthesis_Metadata_TOWER_20191204_timeData.csv",
                                   sep=",", header=TRUE, skip=1,
                                   na.strings=na.strings.data))


# fix the Duration column, where duration is missing
data_t[, ':=' (start_date_1 = as.Date(paste(Start_day_meas,Start_month_meas,Start_year_meas,sep="-"),format="%d-%m-%Y"),
       end_date_1 = as.Date(paste(End_day_meas,End_month_meas,End_year_meas,sep="-"),format="%d-%m-%Y"))]

data_t[,Duration := as.integer(Duration)]
data_t[is.na(Duration),Duration := as.integer(end_date_1 - start_date_1)]

# there are a few entries where the data is an annual average of several years
# and the start/end date capture hte full range of the years
# in this case make the duration = 365
# there is be one with Duration = 488 because it went from 01-May-2015 to 31-Aug-2016 (Study_ID: Alekseychik_Mukhrino_tower_1_Virkkala)
# there is be one with Duration = 730 because it went from 01-May-2015 to 31-Aug-2016 (Study_ID:  Aurela_PallasSammaltunturi_tower_1_Virkkala)
# for now make it 

data_t[Duration>365 ,Duration:=365L]

# Tower: time invariant data
siteinfo_t <- as.data.table(fread(file="CO2flux_ADC_Synthesis_Metadata_TOWER_20191204_siteInfo.csv",
                                       sep=",", header=TRUE,skip=1,
                                       na.strings = na.strings.data))



# CHAMBER: time varying data
data_c <- as.data.table(fread(file="CO2flux_ADC_Synthesis_Metadata_CHAMBER_20181207_timeData.csv",
                                   sep=",", header=TRUE, skip=1,
                                   na.strings=na.strings.data))


# CHAMBER: time invariant data
siteinfo_c <- as.data.table(fread(file="CO2flux_ADC_Synthesis_Metadata_CHAMBER_20181207_siteInfo.csv",
                                       sep=",", header=TRUE, skip=1,
                                       na.strings=na.strings.data))


# prepare data for merging

# # look at column names
# colnames(data_t)
# colnames(siteinfo_t)
# colnames(data_c)
# colnames(siteinfo_c)

# remove duplicates in the Study_ID from the site ID and subset only relevant data to merge to time varying
siteinfo_merge_t <- copy(siteinfo_t[!(duplicated(siteinfo_t[,.(Study_ID)]))])

siteinfo_merge_c <- copy(siteinfo_c[!(duplicated(siteinfo_c[,.(Study_ID)]))])

# merge data and site info for chambers and towers
all_t <- merge(data_t,siteinfo_merge_t,by=c("Study_ID"),all.x=TRUE)
all_c <- merge(data_c,siteinfo_merge_c,by=c("Study_ID"),all.x=TRUE)

# combine chamber and tower
all <- rbind(all_c,all_t, fill=TRUE)

# make sure years are integers
all[,Meas_year := as.integer(Meas_year)]

# there are some GPP data entered with the wrong sign. Make them all negative
all[GPP_gC_m2>0, GPP_gC_m2 := GPP_gC_m2*-1]


#################################
#  Make figures for AGU poster  #
#################################

# Make the intervals: Summer, Winter, Annual
all[,Interval := factor(Interval,levels=c("Summer","Winter","Annual"))]

# box plot of Co2 flux over years Winter/Summer
ggplot(all[Interval=="Summer"&Biome!="Other"&!is.na(Meas_year)],
       aes(Meas_year,NEE_gC_m2/Duration,colour=Biome))+
  geom_point()

ggplot(all[Biome!="Other"&!is.na(Meas_year)],
       aes(Meas_year,NEE_gC_m2/Duration))+
  geom_point()

# calculate mean, max, min NEE/duration for graphing (easier than boxplot)
all_box <- all[(Interval%in% c("Summer","Winter","Annual"))&Biome!="Other"&!is.na(Meas_year),
               list(mean_NEE_duration=mean(NEE_gC_m2/Duration, na.rm=TRUE),
                    min_NEE_duration=min(NEE_gC_m2/Duration,na.rm=TRUE),
                    max_NEE_duration=max(NEE_gC_m2/Duration,na.rm=TRUE)),
               by="Meas_year,Biome,Interval"]


# make a numeric year column so that Tundra and Boreal can be slightly offset in the figure
all_box[Biome=="Tundra",Meas_year_num := as.numeric(Meas_year)]
all_box[Biome=="Boreal",Meas_year_num := as.numeric(Meas_year+0.3)]

# figure of mean, max, min NEE/Duration by Biome and Interval
# make without max/min bars
fig1 <- ggplot(all_box[!is.na(mean_NEE_duration),],aes(Meas_year_num,mean_NEE_duration,colour=Biome, shape=Biome))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=min_NEE_duration,ymax=max_NEE_duration),width=0)+
  #ylim(c(-2.5,5))+
  scale_x_continuous(limits=c(1965,2020),breaks=seq(1965,2020,by=5))+
  scale_colour_manual(values=c("#333300","#66CC00"))+
  scale_shape_manual(values=c(17,16))+
  geom_hline(yintercept=0)+
  facet_grid(Interval~.)+
  labs(y=expression(paste('NEE (gCm'^-2,'day'^-1,')',sep='')),
       x="Year")+
  theme(title = element_text(size=21),
    axis.text.y = element_text(size=18,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(size=13,margin=unit(c(4,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.length=unit(-1.0,"mm"),
    legend.background = element_blank(),    
    legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
    legend.position=c(0.9,0.96),
        strip.text = element_text(size=16),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,1,1,1),"mm")) #top,right,bottom,left)

fig1

setwd(fig_dir)
#ggsave(fig1,file="Fig1_NEE", width=170,height=200,
#      units= "mm",dpi=500, device="jpeg")



fig1_zoom <- ggplot(all_box[!is.na(mean_NEE_duration),], #[Meas_year>1990&!is.na(mean_NEE_duration),],
                    aes(Meas_year_num,mean_NEE_duration,colour=Biome,shape=Biome))+
  geom_point()+
  geom_errorbar(aes(ymin=min_NEE_duration,ymax=max_NEE_duration),width=0)+
  scale_x_continuous(limits=c(1990,2020),breaks=seq(1990,2020,by=5))+
  scale_colour_manual(values=c("#333300","#66CC00"))+
  scale_shape_manual(values=c(17,16))+
  geom_hline(yintercept=0)+
  facet_grid(Interval~.)+
  labs(y=expression(paste('NEE (gCm'^-2,'day'^-1,')',sep='')),
       x="Year")+
  theme(title = element_text(size=21),
        axis.text.y = element_text(size=18,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(size=13,margin=unit(c(4,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.length=unit(-1.0,"mm"),
        legend.background = element_blank(),    
        legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.position=c(0.9,0.96),
        strip.text = element_text(size=16),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,1,1,1),"mm"))

fig1_zoom

setwd(fig_dir)
#ggsave(fig1_zoom,file="Fig1_NEE_1990", width=170,height=200,
#       units= "mm",dpi=500, device="jpeg")


# number of data points at each year
# use the Study_ID to get only the number of sites, not multiple entries
# assume that before 2007 anything in Belshe is also in McGuire (this is rough...would have to double check)

counts <- all[(!is.na(Meas_year)&Meas_year<=2007 & !(Extraction_source %in% c("Belshe")))|
                       (!is.na(Meas_year)&Meas_year>2007) ,
                     list(datapoint = length(unique(Study_ID))),
              by="Meas_year,Flux_method,Interval"]


counts_all <- expand.grid(Meas_year=1966:2018,
                                 Flux_method=c("Chamber","EC"),
                                 Interval=c("Summer","Winter","Annual"))

counts_all <- merge(counts_all,counts,by=c("Meas_year","Flux_method","Interval"),
                           all.x=TRUE)

fig2 <- ggplot(counts_all,
       aes(Meas_year,datapoint,fill=Flux_method))+
  geom_col(position="dodge",colour="black")+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits=c(1965,2020),breaks=seq(1965,2020,by=5))+
  scale_fill_manual(values=c("#6633FF","#CC33CC"))+
  facet_grid(Flux_method~Interval)+
  labs(y="# flux measurements",
       x="Year")+
  theme(title = element_text(size=21),
        axis.text.y = element_text(size=18,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(angle=45,size=13,margin=unit(c(4,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.length=unit(-1.0,"mm"),
        legend.background = element_blank(),    
        legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.position=c(0.9,0.96),
        strip.text = element_text(size=16),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,1,1,1),"mm"))

fig2

setwd(fig_dir)
#ggsave(fig2,file="Fig2_Sites_number", width=200,height=120,
#      units= "mm",dpi=500, device="jpeg")


# years by latitude
# with arctic circle: 66.563167
fig3 <- ggplot(all[!(Biome %in% c("Other"))&!is.na(Biome)&!is.na(Meas_year)&
                     Interval %in% c("Summer","Winter","Annual")],
       aes(Meas_year,Latitude,colour=Biome,shape=Biome))+
  geom_point(size=2)+
  geom_hline(yintercept=66.563167)+
  scale_colour_manual(values=c("#333300","#66CC00"))+
  scale_shape_manual(values=c(17,16))+
  scale_x_continuous(limits=c(1965,2020),breaks=seq(1965,2020,by=5))+
  facet_grid(Flux_method~Interval)+
  labs(y="Latitude",
       x="Year")+
  theme(title = element_text(size=21),
        axis.text.y = element_text(size=18,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(angle=45,size=13,margin=unit(c(4,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.ticks.length=unit(-1.0,"mm"),
        legend.background = element_blank(),    
        legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.position=c(0.9,0.96),
        strip.text = element_text(size=16),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,1,1,1),"mm"))


fig3

setwd(fig_dir)
#ggsave(fig3,file="Fig3_Sites_Latitude", width=200,height=170,
 #           units= "mm",dpi=500, device="jpeg")


# get some additional info in the data: 
# number of citations
View(levels(factor(all$Citation)))

# number of active contributors
levels(factor(all$Data_contributor))
# 31 active contributors (Meyer and Hurkuck contributed same site, therefore: 30!!)
# [1] "Andrej Varlagin"            "Anna Virkkala"              "Belshe"                     "Bokhorst"                  
# [5] "Carolina Voigt"             "Celis"                      "Charlotte Sigsgaard"        "Craig Emmerton"            
# [9] "Elyn Humphreys"             "Euskirchen"                 "Gesa Meyer"                 "Goeckede"                  
# [13] "Harazono"                   "Holl"                       "Hurkuck"                    "Järvi Järveoja"            
# [17] "Jinshu Chi"                 "Julia Boike"                "Kobayashi"                  "Kochendorfer"              
# [21] "Kotani"                     "Kwon"                       "Lafleur"                    "M Torn/S Dengel"           
# [25] "Machimura"                  "Maija E. Marushchak"        "Matsuura"                   "Matthias Peichl"           
# [29] "Mauritz"                    "McGuire"                    "Mikaell Ottosson-Löfvenius" "Namyi Chae"                
# [33] "Sang-Jong PARK"             "Ueyama"                     "Virkkala" 
# 
# number of FluxNet data entries
(siteinfo_t[grepl("Fluxnet|FluxNet",Extraction_source),.(Study_ID)])
Study_ID
# 15 FluxNet sites represented

# 1:                CAMan_tower_1_FluxNet2015
# 2:                RUChe_tower_1_FluxNet2015
# 3:                RUHa1_tower_1_FluxNet2015
# 4:                CAGro_tower_1_FluxNet2015
# 5:                CAQfo_tower_1_FluxNet2015
# 6:                DKZah_tower_1_FluxNet2015
# 7:                RUCok_tower_1_FluxNet2015
# 8:                USIvo_tower_1_FluxNet2015
# 9:                FILom_tower_1_FluxNet2015
# 10:                DKZaf_tower_1_FluxNet2015
# 11:                RUVrk_tower_1_FluxNet2015
# 12:                SESt1_tower_1_FluxNet2015
# 13:                USAtq_tower_1_FluxNet2015
# 14: Milyukova_Fyodorovskoye_tower_1_Varlagin
# 15:               Boike_NO-Blv_tower_1_Boike

# number of sites up to 2018
sites_2018 <- all[End_year_meas>2017 | Meas_year >2017,]

# 5!! 

# make a figure with McGuire's RECCAP model-observation comparison
reccap <- data.table(source=factor(c("Observation", "Regional Process Model", "Global Process Model", "Inverison"),
                                   levels=c("Observation", "Regional Process Model", "Global Process Model", "Inverison")),
                     mean=c(-103,-177,-86,-96),
                     LL=c(-297,-284,-205,-331),
                     UL=c(89,-41,-1,173))

ggplot(reccap, aes(source, mean))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=LL, ymax=UL),width=0.2)+
  labs(y=expression("TgCyr"^"−1"), x="Method")+
  theme_bw()+
  theme(axis.text.x = element_text(size=14, angle = 90),
        axis.text.y = element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=16))

# make figure of new site additions after Belshe and McGuire
# This unique stuff doesn't make sense because of overlaps between syntheses!!
# dat_unique_measured <- copy(all[!(duplicated(all, by=c("Study_ID")))&((!is.na(Meas_year)&Meas_year<=2007 & !(Extraction_source %in% c("Belshe")))|
#                                                                         +              (!is.na(Meas_year)&Meas_year>2007)),])
# # number of unique chamber and tower observations
# 
# nrow(dat_unique_measured[Flux_method=="EC"])
# # 163
# 
# nrow(dat_unique_measured[Flux_method=="Chamber"])
# # 278

# all measurements
ggplot()+
  geom_point(aes(x=Meas_year,y=NEE_gC_m2/Duration),
             data=all[Data_contributor %in% c('Belshe','McGuire')&!is.na(Flux_method)],colour="black")+
  geom_point(aes(x=Meas_year,y=NEE_gC_m2/Duration,colour=Data_contributor), alpha=0.3,
             data=all[!(Data_contributor %in% c('Belshe','McGuire'))&!is.na(Flux_method)])+
  facet_grid(~Flux_method)+
  geom_hline(yintercept=0)+
  labs(y=expression(paste('NEE (gCm'^-2,'day'^-1,')',sep='')),
       x="Year")+
  theme(axis.text.y = element_text(size=12,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(size=12,margin=unit(c(4,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14), #element_text(size=12),
        axis.ticks.length=unit(-1.0,"mm"),
        legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        strip.text = element_text(size=14),
        legend.position="none",
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(1,1,1,1),"mm")) #top,right,bottom,left)





