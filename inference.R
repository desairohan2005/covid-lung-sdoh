#####SETUP#####
library(tidyverse)
library(corrplot)
library(cowplot)
library(psych)

#####INPUT#####
classdata <- read_csv("data/classdata.csv", col_types = cols())
nci_lung_incidence <- read_csv("data/nci_lung_incidence.csv", col_types = cols())
nci_all_incidence <- read_csv("data/nci_all_incidence.csv", col_types = cols())
ohio_covid <- read_csv("data/ohio_covid_2021may22.csv", col_types = cols())

#nci_incidence <- read_csv("https://www.statecancerprofiles.cancer.gov/incidencerates/index.php?stateFIPS=39&areatype=county&cancer=047&race=00&sex=0&age=001&stage=999&year=0&type=incd&sortVariableName=rate&sortOrder=desc&output=1",
#                         skip = 8) %>%
#  filter(!is.na(FIPS)) %>%
#  filter(!endsWith(FIPS, "00"))
ohio_counties <- map_data("county") %>%
  filter(region == "ohio") %>%
  left_join(classdata, by = c("subregion" = "County")) %>%
  left_join(nci_all_incidence, by = c("subregion" = "County")) %>%
  left_join(ohio_covid, by = c("subregion" = "County"))

# map setup
g <- ggplot(data = ohio_counties, aes(x = long, y = lat, group = group)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank()) +
  coord_map()

# map median income
g +
  geom_polygon(aes(fill = Incidence), color = "black", size = 0.5)

ohio_covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/10-10-2021.csv") %>%
  filter(Country_Region == "US") %>%
  filter(Province_State == "Ohio") %>%
  filter(!is.na(Admin2)) %>%
  filter(!is.na(Case_Fatality_Ratio)) %>%
  select(FIPS, Admin2, Province_State, Confirmed, Deaths, Incident_Rate, Case_Fatality_Ratio) %>%
  rename(County = Admin2,
         COVID_Confirmed = Confirmed,
         COVID_Incident_Rate = Incident_Rate,
         COVID_Case_Fatality_Ratio = Case_Fatality_Ratio) %>%
  select(County, COVID_Confirmed, COVID_Incident_Rate, COVID_Case_Fatality_Ratio) %>%
  mutate(County = tolower(County))
write_csv(ohio_covid, "data/ohio_covid_2021oct10.csv")

#####DATA MERGE#####
merged_set <- classdata %>%
  inner_join(nci_lung_incidence,
            by = "County") %>%
  inner_join(ohio_covid,
             by = "County")

#####VISUALIZATION#####
merged_set %>%
  rename(`Median Income` = "Median_Income",
         `COVID Mortality` = "COVID_Case_Fatality_Ratio") %>%
  select(`Median Income`, `Lung_CA_Incidence`, `COVID Mortality`) %>%
  pairs.panels(method = "spearman",
               smooth = FALSE)

#####INFERENCE#####
# calculate correlation matrix
corr_set <- merged_set %>%
  
  # remove non-numeric columns
  select(-County, -Lung_CA_Trend) %>%
  
  # remove missing values
  slice(complete.cases(.) %>% which) %>%
  
  # select only truly relevant socioeconomic variables and incidence
  select(Pov_Percent, Unemployment, Median_Income, GDP_capita,
         Lung_CA_Incidence, COVID_Incident_Rate, COVID_Case_Fatality_Ratio)

corr_vals <- cor(corr_set)

# draw and save correlation matrix
png("inference-corrplot.png",
    width = 600, height = 600)

corrplot(corr_vals,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45)

dev.off()

# correlation inference (Pearson)
cor.test(corr_set$Median_Income, corr_set$Lung_CA_Incidence, method = "pearson")
cor.test(corr_set$Median_Income, corr_set$COVID_Case_Fatality_Ratio, method = "pearson")

#####SCATTERS#####
income_labels <- seq(40, 100, by = 20) %>% as.character
# lung CA vs. median income
ggplot(corr_set, aes(x = Median_Income, y = Lung_CA_Incidence)) +
  geom_point() +
  xlab("Median county-level income ($000s)") +
  scale_x_continuous(labels = income_labels) +
  ylab("Lung cancer county-level incidence (per 100K)") +
  theme_bw()
ggsave("SuppFigure1.png")

# COVID case fatality vs. median income
ggplot(corr_set, aes(x = Median_Income, y = COVID_Case_Fatality_Ratio)) +
  geom_point() +
  xlab("Median county-level income ($000s)") +
  scale_x_continuous(labels = income_labels) +
  ylab("COVID-19 county-level case fatality (%)") +
  theme_bw()
ggsave("SuppFigure2.png")

# lung CA vs. COVID case fatality
ggplot(corr_set, aes(x = COVID_Case_Fatality_Ratio, y = Lung_CA_Incidence)) +
  geom_point() +
  xlab("COVID-19 county-level case fatality (%)") +
  ylab("Lung cancer county-level incidence (per 100K)") +
  theme_bw()
ggsave("SuppFigure3.png")

#####COMPOUND BAR#####
reordered <- merged_set %>%
  select(County, Median_Income, COVID_Case_Fatality_Ratio, Lung_CA_Incidence) %>%
  arrange(desc(Median_Income)) %>%
  mutate(Adj_Income = Median_Income - median(Median_Income)) %>%
  mutate(County = factor(County, levels = County))
ggplot(reordered, aes(x = County, y = Adj_Income)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  xlab("County") +
  ylab("Income difference from median") +
  theme_cowplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x= element_blank())
