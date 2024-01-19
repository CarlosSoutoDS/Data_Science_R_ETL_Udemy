#Environment empty
rm(list = ls())
## ==========================================================================================
##            Data Science in R: ETL part 1- Import and Structure Data (PROJECT)
##                                  UDEMY
## ==========================================================================================

# load the packages
library(tibble)
library(tidyr)
library(dplyr)
library(xlsx)
library(magrittr)
library(ggplot2)

# Specify the file path, sheet number, start row, and end row
file_path <- "01_Utilizacao_da_Internet.xlsx"
sheet_number <- 35
start_row <- 5
end_row <- 47

# Read the specific sheet and specify the correct row range
proj_etl <- read.xlsx(file_path, sheetIndex = sheet_number, startRow = start_row, endRow = end_row)

# Print the first few rows of the data for verification
head(proj_etl)
str(proj_etl)

# convert to tibble
proj_etl <- as_tibble(proj_etl)
str(proj_etl)

#Criando as duas partições
partition_1<-proj_etl[,1:8]
partition_2<-proj_etl[,9:16]

# Print the first few rows of the partitions for verification
head(partition_1)
head(partition_2)

#Replace names of columns
names(partition_1) <- c("region", "total", "<4yrs", "40-7yrs", "8-10yrs", "11-14 yrs", "15+yrs","not_determined")
names(partition_2) <- c("region", "total", "<4yrs", "40-7yrs", "8-10yrs", "11-14 yrs", "15+yrs","not_determined")

#check the names
head(partition_1)
head(partition_2)

#os valores da variável ANOS DE ESTUDO estão espalhados nas colunas (Tipo 4 para Tipo 1) ....
a <- partition_1 %>% gather("<4yrs", "40-7yrs", "8-10yrs", "11-14 yrs", "15+yrs","not_determined", key = "years_study", value = "quantity")
b <- partition_2 %>% gather("<4yrs", "40-7yrs", "8-10yrs", "11-14 yrs", "15+yrs","not_determined", key = "years_study", value = "percentage")

print(a)
print(b)

#building the final data frame
proj_etl_prepared <- cbind(a,b)
head(proj_etl_prepared)

#select the specific columns 
proj_etl_prepared <- proj_etl_prepared[,c(1,3,4,8)]

proj_etl_prepared<-as_tibble(proj_etl_prepared)
head(proj_etl_prepared)

#values of region column
print(proj_etl_prepared$region)

# Filter rows for the first bar chart
df_chart1 <- subset(proj_etl_prepared, region %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará",
                                                     "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia",
                                                     "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina",
                                                     "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"))

# Filter rows for the second bar chart
df_chart2 <- subset(proj_etl_prepared, region %in% c("Região Metropolitana de Belém", "Região Metropolitana de Fortaleza", "Região Metropolitana de Recife",
                                                     "Região Metropolitana de Salvador", "Região Metropolitana de Belo Horizonte",
                                                     "Região Metropolitana do Rio de Janeiro", "Região Metropolitana de São Paulo",
                                                     "Região Metropolitana de Curitiba", "Região Metropolitana de Porto Alegre"))

# Filter rows for the third bar chart
df_chart3 <- subset(proj_etl_prepared, region %in% c("Brasil", "Norte", "Nordeste", "Sul", "Sudeste", "Centro-Oeste"))

# Create bar charts with titles and subtitles
chart1 <- ggplot(df_chart1, aes(x = years_study, y = as.numeric(percentage), fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Internet Usage by Education Level in Federation Units (2015)",
       subtitle = "Distribution of people aged 10 or over in the last three months") +
  xlab("Years of Study") +
  ylab("Percentage (%)")

chart2 <- ggplot(df_chart2, aes(x = years_study, y = as.numeric(percentage), fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Internet Usage by Education Level in Major Regions (2015)",
       subtitle = "Distribution of people aged 10 or over in the last three months") +
  xlab("Years of Study") +
  ylab("Percentage (%)")

chart3 <- ggplot(df_chart3, aes(x = years_study, y = as.numeric(percentage), fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Internet Usage by Education Level in Brazil and Regions (2015)",
       subtitle = "Distribution of people aged 10 or over in the last three months") +
  xlab("Years of Study") +
  ylab("Percentage (%)")

# Display the charts
print(chart1)
print(chart2)
print(chart3)

# Save as CSV with semicolon delimiter
write.table(proj_etl_prepared, file = "proj_etl_prepared.csv", sep = ";", row.names = FALSE)
