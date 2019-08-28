library(Kendall)
library(ggplot2) 
library(reshape2)

cpi <- read.csv("cpi_final.csv")
scores = c()
pvalues = c()
for (i in 1:length(cpi[,1])){
  country_row <- cpi[i, ]
  country_values <- as.numeric(country_row[1, 2:length(country_row)])
  country_values <- country_values[country_values!=0]
  mk <- MannKendall(country_values)
  scores[i] <- mk$S
  pvalues[i] <- mk$sl
}

cpi["scores"] <- scores
cpi["pvalues"] <- pvalues
cpi <- cpi[order(pvalues), ]

## country_row <- cpi[cpi$Country=="Mexico", ]
## plot(1998:2016, as.numeric(country_row[1, 2:20]), xlab="Years", ylab="CPI Mexico") 

top6_uw <- head(cpi[cpi$scores>0, ], 6)
top6_dw <- head(cpi[cpi$scores<0, ], 6)
nonworking <- tail(cpi[(cpi$X1998>0 & cpi$X1999>0 & cpi$X2000>0), ], 6)

df <- melt(top6_uw[1:20], id = "Country")
df$variable <- as.numeric(substr(df$variable, 2, 5))
colnames(df) <- c("Country", "Year", "Value")
ggplot(data = df, aes(Year, Value)) +
  geom_line(aes(color = Country)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous(breaks = seq(1998, 2016, 2)) +
  ggtitle("COUNTRIES IMPROVING THEIR ANTI-CORRUPTION POLICIES") +
  ylab("CPI Score")

df <- melt(top6_dw[1:20], id = "Country")
df$variable <- as.numeric(substr(df$variable, 2, 5))
colnames(df) <- c("Country", "Year", "Value")
ggplot(data = df, aes(Year, Value)) +
  geom_line(aes(color = Country)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous(breaks = seq(1998, 2016, 2)) +
  ggtitle("COUNTRIES SHOWING SETBACK ON THEIR CORRUPTION EFFORTS") +
  ylab("CPI Score")

df <- melt(nonworking[1:20], id = "Country")
df$variable <- as.numeric(substr(df$variable, 2, 5))
colnames(df) <- c("Country", "Year", "Value")
ggplot(data = df, aes(Year, Value)) +
  geom_line(aes(color = Country)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous(breaks = seq(1998, 2016, 2)) +
  ggtitle("COUNTRIES NOT WORKING ON THEIR CORRUPTION SYSTEMS") +
  ylab("CPI Score")
