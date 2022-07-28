

# This file provides replication guidelines for the article 
# Pianta & Brutschin:
# "Emissions Lock-in, Capacity, and Public Opinion: How Insights From Political Science Can Inform Climate Modeling Efforts
# Politics and Governance (ISSN: 2183-2463)
# 2022, Volume 10, Issue 3, Pages X–X
# https://doi.org/10.17645/pag.v10i3.5462



# Please note that, as the code downloads the most recent population data 
# and some of the datasets from online sources, 
# the reported numbers for regional aggregation might change


# rm(list=ls())


# In case you do not have some of these packages, please install them

pkgs <- c("vroom", "tidyverse", "here", "zoo", "haven", "countrycode", "ggrepel", "WDI", "hrbrthemes", "cowplot", "scatterplot3d")
load<-lapply(pkgs, library, character.only=TRUE)


# Set working directory to the folder with key datasets (see folder data)

setwd("/Users/silviapianta/Documents/GitHub/polsci-climate-modelling")


# Table matching countries to IAM regions

regions <- vroom("region_conversion.csv") %>%
  rename(iso3c=iso)


# Function to standardize measurements (will be used later)

range01 <- function(x){(x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))}


# Colours used in visualizations

colours=c("ASIA"="firebrick", "OECD"="forestgreen", "LAM" ="royalblue", "MAF"="grey", "REF" = "purple")

colours3=c("high"="royalblue", "low"="orange")


# Note: the code downloads most data directly from online sources, 
# as we might not be allowed to distribute them


#################################

#Figure 2 data and visualizations

#Figure 2A

# Download data on fossil share of electricity generation and fossil rent 
# Source: World Bank World Development Indicators (WDI)


wdi2 <- WDI(country = "all",
          indicator = c(
            'coal_rent'='NY.GDP.COAL.RT.ZS', 
            'co2_cap'='EN.ATM.CO2E.PC', 'population'='SP.POP.TOTL',
            'gas_rent'='NY.GDP.NGAS.RT.ZS',
            'oil_rent'='NY.GDP.PETR.RT.ZS',
            'coal_share'= 'eg.elc.coal.zs',
            'gas_share'= 'EG.ELC.NGAS.ZS',
            'oil_share'='eg.elc.petr.zs'),
          start = 1960,end = 2020,extra = TRUE) %>%
  as_tibble() %>%
  filter(region!="Aggregates") %>%
  arrange(iso3c, year) %>%
  fill(coal_rent,.direction = "down") %>%
  fill(co2_cap,.direction = "down") %>%
  fill(gas_rent,.direction = "down") %>%
  fill(oil_rent,.direction = "down") %>%
  fill(coal_share,.direction = "down") %>%
  fill(gas_share,.direction = "down") %>%
  fill(oil_share,.direction = "down") 

fossil_sector <- wdi2 %>%
  filter(year==2020) %>%
  select(iso3c,country,coal_rent, gas_rent, oil_rent, co2_cap, population, coal_share, gas_share, oil_share) %>%
  left_join(regions, by=c("iso3c")) %>%
  #filter(population>10*10^6) %>%
  mutate(fossil_rent=coal_rent+oil_rent+gas_rent) %>%
  mutate(fossil_share=coal_share+gas_share+oil_share) %>%
  filter(fossil_share<90) %>%
  mutate(fossil_rent_norm=100*range01(fossil_rent)) %>%
  mutate(co2_cap_norm=100*range01(co2_cap)) %>%
  mutate(fossil_share_norm=100*range01(fossil_share)) 

fossil_rent_norm_q <- quantile(fossil_sector$fossil_rent_norm, probs = c(0.7), na.rm = T)
fossil_share_norm_q <- quantile(fossil_sector$fossil_share_norm, probs = c(0.7), na.rm = T)



fossil_sector_fig <- fossil_sector %>%
  filter(!(is.na(regions))) %>%
  mutate(country_viz=ifelse(fossil_rent_norm>fossil_rent_norm_q|fossil_share_norm>fossil_share_norm_q, iso3c,NA)) %>%
  ggplot() +
  geom_point(aes(x=fossil_rent, y=fossil_share, color=regions), size=4, alpha=0.6)+
  geom_text_repel(aes(x=fossil_rent, y=fossil_share, label=country_viz))+
  theme_ipsum()+
  xlab("Fossil Rent % of GDP (Coal+Gas+Oil)")+
  ylab("Fossil fuels in electricity generation % (Coal+Gas+Oil)")+
  ggtitle("Carbon Lock-in")+
  scale_colour_manual(values=colours)

fossil_sector_fig


# Figure 2B

# Dowload data on the share of methane emissions in agriculture
# Source: https://zenodo.org/record/5497833 


methane <-vroom("emissions.csv") %>%
           filter(year==2019) %>%
  filter(sector_title=="AFOLU") %>%
  filter(gas=="CH4") %>%
  group_by(ISO) %>%
  mutate(value=value*gwp100_ar6) %>%
  summarise(methane_emissions=sum(value)) %>%
  rename(iso3c=ISO)


# Dowload data on population and the share agriculture in GDP 
# Source: World Bank World Development Indicators (WDI)

wdi1 <- WDI(country = "all",
                 indicator = c(
                   'population'='SP.POP.TOTL',
                   'share_agriculture'='NV.AGR.TOTL.ZS'),
                 start = 1960,end = 2020,extra = TRUE) %>%
  as_tibble() %>%
  filter(region!="Aggregates") %>%
  arrange(iso3c, year) %>%
  fill(share_agriculture,.direction = "down")

agri_sector <- wdi1 %>%
             filter(year==2020) %>%
             left_join(methane, by=c("iso3c")) %>%
             left_join(regions, by=c("iso3c")) %>%
             filter(country!="Somalia") %>%
             filter(country!="Chad") %>%
             mutate(methane_capita=methane_emissions/population) %>%
             mutate(agri_gdp_norm=100*range01(share_agriculture)) %>%
             mutate(methane_cap_norm=100*range01(methane_capita)) 


#Calculations not to add labels only to higher percentile countries
meth_cap_q <- quantile(agri_sector$methane_cap_norm, probs = c(0.7), na.rm = T)
agri_gdp_norm_q <- quantile(agri_sector$agri_gdp_norm, probs = c(0.7), na.rm = T)


agri_sector_fig <- agri_sector %>%
                 filter(!(is.na(regions))) %>%
                 mutate(country_viz=ifelse(methane_cap_norm>meth_cap_q|agri_gdp_norm>agri_gdp_norm_q, iso3c,NA)) %>%
  ggplot() +
  geom_point(aes(x=share_agriculture, y=methane_capita, color=regions), size=4, alpha=0.6)+
  geom_text_repel(aes(x=share_agriculture, y=methane_capita, label=country_viz))+  
  theme_ipsum()+
  xlab("Share of Agriculture in GDP %")+
  ylab("Methane Emissions (AFOLU) per Capita in tCO2e")+
  ggtitle("Methane Lock-in")+
  scale_colour_manual(values=colours)


agri_sector_fig


# Arrange Figure 2A and 2B

country_plot <- plot_grid(fossil_sector_fig,agri_sector_fig,labels=c("A", "B"), ncol = 1)

country_plot

#Save Figure

ggsave("Figure2.png", units="in", width=7, height=9, dpi=300)


########################################################################################
#Figure 3 - Governance 

#Please download the data yourself and save it in the data folder (see source in the SM)

gov_rl<-vroom("governance_2021.csv") %>%
  select(countryname, year,rle) %>%
  arrange(countryname, year) %>%
  mutate(iso3c=countrycode(countryname, "country.name", "iso3c")) %>%
  select(-countryname)



gov_eff<-vroom("governance_2021.csv") %>%
  select(countryname, year, gee) %>%
  mutate(iso3c=countrycode(countryname, "country.name", "iso3c")) %>%
  left_join(population_data, by=c("iso3c", "year")) %>%
  left_join(regions, by=c("iso3c")) %>%
  filter(!(is.na(regions))) %>%
  left_join(gov_rl, by=c("year", "iso3c")) %>%
  group_by(year) %>%
  mutate(ge_norm=range01(gee)*100) %>%
  mutate(rl_norm=range01(rle)*100) %>%
  arrange(countryname, year) %>%
  filter(year==2020) %>%
  mutate(instit_index=(ge_norm+rl_norm)/2) 


ge_norm50<-quantile(gov_eff$ge_norm, probs = c(0.5), na.rm = T)
rl_norm50<-quantile(gov_eff$rl_norm, probs = c(0.5), na.rm = T)
instit_index50<-quantile(gov_eff$instit_index, probs = c(0.5), na.rm = T)


#institutional figure
institutional_fig<-gov_eff %>%
  filter(iso3c!="KOR") %>%
  mutate(country_viz=ifelse(ge_norm>ge_norm50|rl_norm>rl_norm50, iso3c,NA)) %>%
  ggplot() +
  geom_point(aes(y=ge_norm, x=rl_norm, color=regions), size=4, alpha=0.6)+
  geom_text_repel(aes(y=ge_norm, x=rl_norm, label=country_viz))+
  theme_ipsum()+
  ylab("Government Effectiveness Score")+
  xlab("Rule of Law Score")+
  scale_colour_manual(values=colours)


institutional_fig

ggsave("Figure3.png", units="in", width=7, height=4, dpi=300)



#######################################
# Figure 4 - Economic capabilities 

population_data<-population_data %>%
  select(-year)


gdppc<-WDI(country = "all",
           indicator = c(
             'gdp_capita'="NY.GDP.PCAP.PP.KD", 
             'ease_business'="IC.BUS.DFRN.XQ"),
           start = 1960,end = 2020,extra = TRUE) %>%
  as_tibble()%>%
  filter(region!="Aggregates") %>%
  arrange(iso3c, year) %>%
  left_join(population_data, by=c("iso3c")) %>%
  # filter(population>10*10^6) %>%
  mutate(gdp_log=log(gdp_capita+1)) %>%
  fill(ease_business,.direction = "down") %>%
  filter(year==2020) %>%
  mutate(gdp_score=range01(gdp_capita)*100) 


gdp_capita70<-quantile(gdppc$gdp_capita, probs = c(0.7), na.rm = T)
ease_business70<-quantile(gdppc$ease_business, probs = c(0.7), na.rm = T)



economic_fig<-gdppc %>%
  left_join(regions, by=c("iso3c")) %>%
  filter(!is.na(regions)) %>%
  mutate(country_viz=ifelse(gdp_capita>gdp_capita70|ease_business>ease_business70, iso3c,NA)) %>%
  ggplot() +
  geom_point(aes(y=ease_business, x=gdp_capita, color=regions), size=4, alpha=0.6)+
  geom_text_repel(aes(y=ease_business, x=gdp_capita, label=country_viz))+
  theme_ipsum()+
  xlab("GDP per capita (PPP constant 2017 international $)")+
  ylab("Ease of doing business (score)")+
  scale_colour_manual(values=colours)


economic_fig

ggsave("Figure4.png", units="in", width=7, height=4, dpi=300)



########################################################################################
#Figure 5 - Technology 

population_data<-agri_sector %>%
                 select(iso3c, year, population)

wdi3<-WDI(country = "all",
          indicator = c(
            'rd_gdp'='GB.XPD.RSDV.GD.ZS', 'population'='SP.POP.TOTL'),
          start = 1960,end = 2020,extra = TRUE) %>%
  as_tibble() %>%
  filter(region!="Aggregates") %>%
  arrange(iso3c, year) %>%
  fill(rd_gdp,.direction = "down") 


science<-vroom("gii.csv") %>%
  #Graduates in science and engineering, % of total tertiary graduates
  #Please download this data yourself
  filter(Indicator=="Graduates in science and engineering", `Subindicator Type`=="Percent") %>%
  select(-c(`Indicator Id`, `Indicator`, `Subindicator Type`, country)) %>%
  pivot_longer(!c(iso3c), names_to = "year", values_to = "science_percent") %>%
  filter(year!=2020) %>%
  mutate(year=as.numeric(year)) 

technology<-wdi3 %>%
  left_join(science, by=c("year", "iso3c")) %>%
  left_join(regions, by=c("iso3c")) %>%
  group_by(iso3c) %>%
  arrange(iso3c, year) %>%
  fill(science_percent,.direction = "down") %>%
  filter(iso3c!="MMR") %>%
  filter(iso3c!= "SYC") %>%
  filter(iso3c!="BTN") %>%
  filter(iso3c!="PHL") %>%
  filter(iso3c!="BRN")%>%
  filter(year==2019) %>%
  mutate(science_percent=ifelse(iso3c=="CHN", 40, science_percent))


#we used this article to justify 40% for China
#https://www.forbes.com/sites/niallmccarthy/2017/02/02/the-countries-with-the-most-stem-graduates-infographic/?sh=6b642631268a

rd_gdp70<-quantile(technology$rd_gdp, probs = c(0.7), na.rm = T)
science_percent70<-quantile(technology$science_percent, probs = c(0.7), na.rm = T)


technology_fig<-technology %>%
  filter(!(is.na(regions)))  %>%
  filter(iso3c!="DJI")%>%
  mutate(region=factor(region, levels=c("REF","MAF", "LAM","ASIA", "OECD")))%>%
  mutate(country_viz=ifelse(rd_gdp>rd_gdp70|science_percent>science_percent70, iso3c,NA)) %>%
  ggplot() +
  geom_point(aes(x=science_percent, y=rd_gdp, color=regions), size=4, alpha=0.6)+
  geom_text_repel(aes(x=science_percent, y=rd_gdp, label=country_viz))+
  theme_ipsum()+
  xlab("Graduates in science and engineering % all graduates")+
  ylab("Research and development (R&D) % GDP")+
  scale_colour_manual(values=colours)

technology_fig

ggsave("Figure5.png", units="in", width=12, height=5, dpi=300)


##################################################
#Figure 6 - Public Support - Please download the data yourself, see sources and variable names in SM

ivs7 <-read_dta("IVS7_env.dta") %>%
  mutate(iso3c = countrycode(iso2c, 'iso2c', 'iso3c')) %>%
  mutate(iso3c = ifelse(iso2c == "GB-GBN", "GBR", iso3c)) %>%
  left_join(population_data, by=c("iso3c")) %>%
  left_join(regions, by=c("iso3c")) %>%
  filter(!(is.na(regions))) %>%
  mutate(postmat_norm=range01(postmat)*100) %>%
  mutate(envgrowth_norm=range01(envgrowth)*100) %>%
  mutate(engo_norm=range01(engo)*100) %>%
  mutate(attitudes_index=(postmat_norm+envgrowth_norm)/2) 


postmat70<-quantile(ivs7$postmat, probs = c(0.7), na.rm = T)
envgrowth70<-quantile(ivs7$envgrowth, probs = c(0.7), na.rm = T)




#attitudes figure
attitudes_fig<-ivs7 %>%
  mutate(country_viz=ifelse(postmat>postmat70|envgrowth>envgrowth70, iso3c,NA)) %>%
  ggplot() +
  geom_point(aes(y=envgrowth, x=postmat, color=regions), size=4, alpha=0.6)+
  geom_text_repel(aes(y=envgrowth, x=postmat, label=country_viz))+
  theme_ipsum()+
  ylab("Environment more important than growth (mean, 0-1)")+
  xlab("Postmaterialism (mean, 1-3)")+
  scale_colour_manual(values=colours)

attitudes_fig

ggsave("Figure6.png", units="in", width=7, height=4, dpi=300)

################################################################
#Calculations for Figure 7 - It was prepared in Power Point


agri_index_data<-agri_sector %>%
  mutate(agri_index=(agri_gdp_norm+methane_cap_norm)/2) 

fossil_index_data<-fossil_sector %>%
  mutate(fossil_index=(fossil_share_norm+co2_cap_norm)/2)

technology_index_data<- technology%>%
  ungroup() %>%
  group_by(year) %>%
  mutate(science_norm=100*range01(science_percent)) %>%
  mutate(rd_gdp_norm=100*range01(rd_gdp)) %>%
  mutate(tech_index=(science_norm+rd_gdp_norm)/2)

economic_index_data<- gdppc%>%
  left_join(regions, by=c("iso3c")) %>%
  filter(!is.na(regions)) %>%
  ungroup() %>%
  mutate(econ_index=(gdp_score+ease_business)/2)



agri<-agri_index_data %>%
  select(iso3c, population, agri_index)

fossil<-fossil_index_data %>%
  ungroup() %>%
  select(iso3c,fossil_index)

econ<-economic_index_data %>%
  ungroup() %>%
  select(iso3c,econ_index)

tech<-technology_index_data %>%
  ungroup() %>%
  select(iso3c,tech_index)

instit<-gov_eff %>%
  ungroup() %>%
  select(iso3c,instit_index)

attitudes<-ivs7 %>%
  ungroup() %>%
  select(iso3c,attitudes_index)

overall_evaluation<-agri %>%
  filter(iso3c!="KOR") %>%
  left_join(fossil) %>%
  left_join(econ) %>%
  left_join(tech) %>%
  left_join(instit) %>%
  left_join(attitudes) %>%
  left_join(regions) %>%
  rename(region=regions) %>%
  mutate(capabilities_index=(econ_index+tech_index+instit_index)/3) %>%
  mutate(emissions_index=(fossil_index+agri_index+instit_index)/2)



fossil_region<-overall_evaluation %>%
  ungroup()%>%
  group_by(region) %>%
  summarise(fossil_index_reg=weighted.mean(fossil_index,population, na.rm=T)) %>%
  filter(!is.na(region))



agri_region<-overall_evaluation %>%
  ungroup()%>%
  group_by(region) %>%
  summarise(agri_index_reg=weighted.mean(agri_index,population, na.rm=T)) %>%
  filter(!is.na(region))


tech_region<-overall_evaluation %>%
  ungroup()%>%
  filter(!is.na(population)) %>%
  group_by(region) %>%
  summarise(tech_index_reg=weighted.mean(tech_index,population, na.rm=T)) %>%
  filter(!is.na(region))

econ_region<-overall_evaluation %>%
  ungroup()%>%
  group_by(region) %>%
  summarise(econ_index_reg=weighted.mean(econ_index,population, na.rm=T)) %>%
  filter(!is.na(region))

instit_region<-overall_evaluation %>%
  ungroup()%>%
  filter(!is.na(population)) %>%
  group_by(region) %>%
  summarise(instit_index_reg=weighted.mean(instit_index,population, na.rm=T)) %>%
  filter(!is.na(region))

attitudes_region<-overall_evaluation %>%
  ungroup()%>%
  group_by(region) %>%
  summarise(attitudes_index_reg=weighted.mean(attitudes_index,population, na.rm=T)) %>%
  filter(!is.na(region))


regional_evaluation<-fossil_region %>%
  left_join(agri_region) %>%
  left_join(instit_region) %>%
  left_join(econ_region) %>%
  left_join(tech_region) %>%
  left_join(attitudes_region) 

write.csv(regional_evaluation, "regional_evaluation.csv")

#Check the country level 50% median

agri_50<-quantile(agri_index_data$agri_index, probs = c(0.5), na.rm = T)
fossil_50<-quantile(fossil_index_data$fossil_index, probs = c(0.5), na.rm = T)
tech_50<-quantile(technology_index_data$tech_index, probs = c(0.5), na.rm = T)
attitudes_index50<-quantile(ivs7$attitudes_index, probs = c(0.5), na.rm = T)
instit_index50<-quantile(gov_eff$instit_index, probs = c(0.5), na.rm = T)
econ_50<-quantile(economic_index_data$econ_index, probs = c(0.5), na.rm = T)
