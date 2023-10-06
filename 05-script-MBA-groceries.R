
# Paquetes
# install.packages("gsheet")
# install.packages("tidyverse")
# install.packages("arules")
# install.packages("arulesViz")
# install.packages("viridis")

library(gsheet)
library(tidyverse)
library(arules)
library(arulesViz)
library(viridis)

# Importar el data_frame

DF <- read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/1NTjA8nrmcWltvZn4oq5KJK7-R4Mb-_is_5vVsoBDCv0/edit?usp=sharing",
                                  format = "csv"),
               stringsAsFactors = T)

# Crear las canastas ----

## Asignar un ID de canastas

DF1 <- DF %>%
  mutate(basket.id = paste(Member_number, Date, sep = "_")) %>% 
  select(basket.id, itemDescription)



## Crear una lista

DF.list <- split(x = DF1$itemDescription,
                 f = DF1$basket.id)


## Formato transacción

DF.trans <- as(object = DF.list,
               Class = "transactions")

class(DF.trans)



# Análisis descriptivo

itemFrequencyPlot(x = DF.trans,
                  type = "relative",
                  topN = 10,
                  horiz = T,
                  col = viridis(n = 10))



# Reglas

rules <- apriori(data = DF.trans,
                 parameter = list(supp = .0005,
                                  conf = .15,
                                  minlen = 2,
                                  maxlen = 3))


# Resultados

inspect(rules)

plot(rules,
     method = "graph",
     engine = "htmlwidget")














