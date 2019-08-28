library(data.table)
library(dplyr)
library(tidyr)
library(xlsx)
library(ggplot2)
library(corrplot)
indicators <- read.csv("Indicators.csv")
macro_indicator <- c("Expenditure on education as % of total government expenditure (%)",
                     "Internet users (per 100 people)",
                     "Food production index (2004-2006 = 100)",
                     "Foreign direct investment, net outflows (% of GDP)",
                     "Urban population growth (annual %)",
                     "Social contributions (% of revenue)",
                     "GDP per capita growth (annual %)")

macroeconomy <- indicators %>% filter(IndicatorName %in% macro_indicator)

cpi_a <- read.csv("~/Documents/busics/Business Strategy and Analytics/Assignment1/cpi_final_a.csv")
df <- melt(cpi_a[1:18], id = "Country")
df$variable <- as.numeric(substr(df$variable, 2, 5))
colnames(df) <- c("Country", "Year", "Value")

countries <- macroeconomy %>% distinct(CountryCode, CountryName)
colnames(countries) <- c("Country", "CountryCode")
df1 <- merge(x = df, y = countries, by = "Country", all.y = TRUE)
colnames(df1) <- c("CountryName", "Year", "Value", "CountryCode")
df1[,"IndicatorName"] <- "Corruption Perception Index"
df1[,"IndicatorCode"] <- "TI.CPI.WWD"
df1 <- df1[, c(1,4,5,6,2,3)]
actual_me <- macroeconomy[macroeconomy$Year>1997, ]
me_filtered <- actual_me %>% filter(CountryName %in% cpi_a$Country)
me_filtered <- rbind(me_filtered, df1)
macreo_e_w <- spread(data=me_filtered,key=IndicatorName,value=Value) %>% select(-c(CountryCode,IndicatorCode))
macreo_e_w <- as.data.table(unite(macreo_e_w,"Country_Year",c(CountryName,Year)) %>% group_by(Country_Year))
colnames(macreo_e_w)<-c("Country_Year","Education_Expenditure", "Internet_usage", "Food_Production", "Foreign_Investment",
                        "Urban_Population", "Social_Contributions", "GDP_PerCapita", "CPI")
macreo_e_w <- macreo_e_w[, list(Education_Expenditure = sum(Education_Expenditure, na.rm = TRUE), 
                                Food_Production = sum(Food_Production, na.rm = TRUE), 
                                Foreign_Investment = sum(Foreign_Investment, na.rm = TRUE), 
                                GDP_PerCapita = sum(GDP_PerCapita, na.rm = TRUE), 
                                Internet_usage = sum(Internet_usage, na.rm = TRUE),
                                Social_Contributions = sum(Social_Contributions, na.rm = TRUE), 
                                Urban_Population = sum(Urban_Population, na.rm = TRUE), 
                                CPI = sum(CPI, na.rm = TRUE)), by = Country_Year]
rownames(macreo_e_w) <- macreo_e_w$Country_Year
macreo_e_w <- select(macreo_e_w,-Country_Year)
macreo_e_w[macreo_e_w==0] <- NA

M <- cor(macreo_e_w, use="complete.obs") %>% round(3)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(macreo_e_w)

##corrplot(M, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.05)
edu_expenditure_cpi <- subset(me_filtered, IndicatorCode %in% c("SE.XPD.TOTL.GB.ZS", "TI.CPI.WWD") & CountryCode %in% c("AZE", "CMR", "IDN", "LVA", "URY", "VNM"))%>%
  droplevels(.) %>%
  select(CountryName, IndicatorCode, Year, Value)
levels(edu_expenditure_cpi$IndicatorCode) <- c("EDUCATION EXPENDITURE", "CORRUPTION PERCEPTION")
edu_expenditure_cpi$CountryName <- paste("CPI increasing", edu_expenditure_cpi$CountryName, sep=" - ")

edu_expenditure_cpi_down <- subset(me_filtered, IndicatorCode %in% c("SE.XPD.TOTL.GB.ZS", "TI.CPI.WWD") & CountryCode %in% c("DNK", "FIN", "LUX", "SDN", "TUN", "VEN"))%>%
  droplevels(.) %>%
  select(CountryName, IndicatorCode, Year, Value)
levels(edu_expenditure_cpi_down$IndicatorCode) <- c("EDUCATION EXPENDITURE", "CORRUPTION PERCEPTION")
edu_expenditure_cpi_down$CountryName <- paste("CPI decreasing", edu_expenditure_cpi_down$CountryName, sep=" - ")

edu_expenditure_cpi <- rbind(edu_expenditure_cpi, edu_expenditure_cpi_down)

ggplot(data = edu_expenditure_cpi, aes(Year, Value)) +
  geom_line(aes(color = IndicatorCode), size = 1) +
  facet_wrap(~CountryName, ncol = 2) +
  scale_x_continuous(breaks = seq(1998, 2016, 2)) +
  ylab("Education Expenditure vs PCI (%)") +
  theme(panel.spacing = unit(1, "lines"))

inet_usage_cpi <- subset(me_filtered, IndicatorCode %in% c("IT.NET.USER.P2", "TI.CPI.WWD") & CountryCode %in% c("AZE", "CMR", "IDN", "LVA", "URY", "VNM"))%>%
  droplevels(.) %>%
  select(CountryName, IndicatorCode, Year, Value)
levels(inet_usage_cpi$IndicatorCode) <- c("INTERNET USAGE", "CORRUPTION PERCEPTION")
inet_usage_cpi$CountryName <- paste("CPI increasing", inet_usage_cpi$CountryName, sep=" - ")

inet_usage_cpi_down <- subset(me_filtered, IndicatorCode %in% c("IT.NET.USER.P2", "TI.CPI.WWD") & CountryCode %in% c("DNK", "FIN", "LUX", "SDN", "TUN", "VEN"))%>%
  droplevels(.) %>%
  select(CountryName, IndicatorCode, Year, Value)
levels(inet_usage_cpi_down$IndicatorCode) <- c("INTERNET USAGE", "CORRUPTION PERCEPTION")
inet_usage_cpi_down$CountryName <- paste("CPI decreasing", inet_usage_cpi_down$CountryName, sep=" - ")

inet_usage_cpi <- rbind(inet_usage_cpi, inet_usage_cpi_down)

ggplot(data = inet_usage_cpi, aes(Year, Value)) +
  geom_line(aes(color = IndicatorCode), size = 1) +
  facet_wrap(~CountryName, ncol = 2) +
  scale_x_continuous(breaks = seq(1998, 2016, 2)) +
  ylab("Internet Usage vs PCI (%)") +
  theme(panel.spacing = unit(1, "lines"))

urban_grow_cpi <- subset(me_filtered, IndicatorCode %in% c("SP.URB.GROW", "TI.CPI.WWD") & CountryCode %in% c("AZE", "CMR", "IDN", "LVA", "URY", "VNM"))%>%
  droplevels(.) %>%
  select(CountryName, IndicatorCode, Year, Value)
levels(urban_grow_cpi$IndicatorCode) <- c("URBAN GROW POINTS", "CORRUPTION PERCEPTION")
urban_grow_cpi$CountryName <- paste("CPI increasing", urban_grow_cpi$CountryName, sep=" - ")

urban_grow_cpi_down <- subset(me_filtered, IndicatorCode %in% c("SP.URB.GROW", "TI.CPI.WWD") & CountryCode %in% c("DNK", "FIN", "LUX", "SDN", "TUN", "VEN"))%>%
  droplevels(.) %>%
  select(CountryName, IndicatorCode, Year, Value)
levels(urban_grow_cpi_down$IndicatorCode) <- c("URBAN GROW POINTS", "CORRUPTION PERCEPTION")
urban_grow_cpi_down$CountryName <- paste("CPI decreasing", urban_grow_cpi_down$CountryName, sep=" - ")

urban_grow_cpi <- rbind(urban_grow_cpi, urban_grow_cpi_down)
grow_rate <- abs(urban_grow_cpi[urban_grow_cpi$IndicatorCode=="URBAN GROW POINTS", "Value"])
urban_grow_cpi[urban_grow_cpi$IndicatorCode=="URBAN GROW POINTS", "Value"] <- scale(grow_rate, center = FALSE, scale = max(grow_rate, na.rm = TRUE)/100)

ggplot(data = urban_grow_cpi, aes(Year, Value)) +
  geom_line(aes(color = IndicatorCode), size = 1) +
  facet_wrap(~CountryName, ncol = 2) +
  scale_x_continuous(breaks = seq(1998, 2016, 2)) +
  ylab("Scaled Urban Grow Rate vs PCI") +
  theme(panel.spacing = unit(1, "lines"))
