# Food Data Central:


library(dplyr)
library(reshape2)
library("wordcloud")
library("tm")
library(plotly)
library(reshape2)
require(factoextra)
library(dplyr)
library(reshape2)
library(factoextra)
require(MASS)
require(caret)
require(randomForest)
require(e1071)
require(dplyr)
library(klaR)
library(plotly)
library(pander)
library(ggcorrplot)
library(stringr)

#Lectura de datos y de las tablas que se van a utilizar. ---------------------------------

food <- read.csv("~/UB- BigData/TFM/Datos/FoodData_Central_csv_2022-04-28/food.csv")
food_component <- read.csv("~/UB- BigData/TFM/Datos/FoodData_Central_csv_2022-04-28/food_component.csv")

branded_food <- read.csv("~/UB- BigData/TFM/Datos/FoodData_Central_csv_2022-04-28/branded_food.csv")
food_nutrient <- read.csv("~/UB- BigData/TFM/Datos/FoodData_Central_csv_2022-04-28/food_nutrient.csv")


nutrient_incoming_name <- read.csv("~/UB- BigData/TFM/Datos/FoodData_Central_csv_2022-04-28/nutrient_incoming_name.csv")


#Unión de los productos con la información de sus nutrientes: ---------------------------------

food_brnd <- branded_food %>% 
  merge(food, by = "fdc_id") %>% 
  merge(food_nutrient, by = "fdc_id") %>% 
  merge(nutrient_incoming_name, by = "nutrient_id")

#Eliminar los otros dataframes para guardar memoria: ---------------------------------

rm(food)
rm(branded_food)
rm(food_nutrient)

#Búsqueda de productos veganos en el dataframe: ---------------------------------


vegan_products <- food_brnd %>% 
  filter(grepl("VEGGIE|VEGAN|MEAT-FREE|VEGETARIAN",toupper(description))) %>% 
  arrange(publication_date) %>% 
  distinct(gtin_upc, .keep_all = TRUE)


#Análisis de términos más frecuentes en la descripción del producto: ---------------------------------

food_text <- Corpus(VectorSource(vegan_products$description))


food_text_clean <- tm_map(food_text, removePunctuation)
food_text_clean <- tm_map(food_text_clean, content_transformer(tolower))
food_text_clean <- tm_map(food_text_clean, removeNumbers)
food_text_clean <- tm_map(food_text_clean, stripWhitespace)

food_text_clean <- tm_map(food_text_clean, removeWords, stopwords('english'))

# Representación de las palabras más frecuentes mediante una nube de palabras: ---------------------------------

wordcloud(food_text_clean, scale = c(2, 1), min.freq = 100, colors = rainbow(30))

paperCorp <- tm_map(food_text_clean, removeWords, c("veggie","vegan","veggies"))

wordcloud(paperCorp, scale = c(2, 1), min.freq = 100, colors = rainbow(30))

#Extracción de las palabras más frecuentes: ---------------------------------


find_freq_terms_fun <- function(corpus_in){
  doc_term_mat <- TermDocumentMatrix(corpus_in)
  freq_terms <- findFreqTerms(doc_term_mat)[1:max(doc_term_mat$nrow)]
  terms_grouped <- doc_term_mat[freq_terms,] %>%
    as.matrix() %>%
    rowSums() %>%
    data.frame(Term=freq_terms, Frequency = .) %>%
    arrange(desc(Frequency)) %>%
    mutate(prop_term_to_total_terms=Frequency/nrow(.))
  return(data.frame(terms_grouped))
}

freq_words <- find_freq_terms_fun(food_text_clean)

#Análisis de hamburguesa veganas: ---------------------------------

vegan_burger <- vegan_products %>% 
  filter(grepl("BURGER",toupper(description))) %>% 
  merge(nutrient_incoming_name, by = "nutrient_id") %>% 
  mutate(is_vegan = 
           grepl("VEGGIE|VEGAN",toupper(description)))


burguers <- food_brnd %>% 
  filter(grepl("BURGER",toupper(description))) %>% 
  # filter(grepl("VEGGIE|VEGAN|MEAT-FREE|VEGETARIAN",toupper(description))) %>% 
  arrange(publication_date) %>% 
  distinct() %>% 
  mutate(is_vegan = 
           grepl("VEGGIE|VEGAN|MEAT FREE|VEGETARIAN|MEAT ANALOG|PROTEIN BURGER|BEAN BURGER|MORNINGSTAR|GARDENBURGER",toupper(description))) 


# Categorías más frecuentes:


cat <- burguers %>%   
  distinct(fdc_id, branded_food_category,description) %>% 
  group_by(branded_food_category) %>% 
  summarise(n = n())


#Exclusión de algunas categorías que no son hamburguesas. Filtrado de los datos:

exclud <- c(  "Pasta Dinners","Biscuits/Cookies (Shelf Stable)","Breads & Buns","Chips/Crisps/Snack Mixes - Natural/Extruded (Shelf Stable)", "Pickles, Olives, Peppers & Relishes","Pizza")

includ <- c("Vegetable Based Products / Meals - Not Ready to Eat (Frozen)",	
            " Frozen Patties and Burgers",	
            "Other Meats", "Frozen Patties and Burgers")

burguers <- burguers %>% 
  filter(!branded_food_category %in% exclud)
# filter(branded_food_category %in% includ)

# Selección de nutrientes. Ana´lisis de valores faltantes:

burg_nut <-  burguers %>% dplyr::select(fdc_id,name, amount) %>% 
  dcast(fdc_id ~ name)

food_na <- burg_nut %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  melt("fdc_id") %>%  filter(value < 50)

burg_nut2 <- burguers %>% dplyr::select(fdc_id,name, amount) %>% 
  filter(name %in% food_na$variable) %>% 
  dcast(fdc_id ~ name)


#Se eligen los nutrientes:
vec <- c("Ca", "CALORIES","Carbohydrate","Cholesterol","Dietary  Fiber", "FAT","Fat, saturated" ,"Fe","Na", "Protein","Total Trans FA","Vitamin A", "Vitamin C","K") 

burg_nut3 <- burguers %>% dplyr::select(fdc_id,name, amount) %>% 
  filter(name %in% vec) %>% 
  dcast(fdc_id ~ name) %>% 
  replace(is.na(.), 0) %>% # SE VA A ASUMIR QUE LOS NO IMPUTADOS SON 0
  merge(burguers %>% dplyr::select(fdc_id, is_vegan, description), by = "fdc_id")


#Exploración de las diferencias entre nutrientes según si el producto es vegano o no: ---------------------------------

box_nut <- burg_nut3 %>% 
  melt(c("fdc_id","is_vegan","description")) %>% 
  mutate(is_vegan = case_when(
    is_vegan == TRUE ~ "Vegan",
    TRUE ~ "Meat Based"
    
  ))

fig <- plot_ly(box_nut,x = ~variable, y = ~value, color = ~is_vegan, type = "box", text = ~ description)%>%
  layout(boxmode = "group")


calories <- plot_ly(box_nut %>%  filter(variable == "CALORIES"), y = ~value, color = ~is_vegan, type = "box", text = ~ description)%>%
  layout(boxmode = "group", yaxis = list(title= "Kcal"), xaxis = list(title= "Calorias"))

calories

saturadas <- plot_ly(box_nut %>%  filter(variable == "Fat, saturated"), y = ~value, color = ~is_vegan, type = "box", text = ~ description)%>%
  layout(boxmode = "group", yaxis = list(title= "G"), xaxis = list(title= "Grasas saturadas"))

saturadas

fiber <- plot_ly(box_nut %>%  filter(variable == "Dietary  Fiber"), y = ~value, color = ~is_vegan, type = "box", text = ~ description)%>%
  layout(boxmode = "group", yaxis = list(title= "G"), xaxis = list(title= "Fibra"))

fiber

sodium <- plot_ly(box_nut %>%  filter(variable == "Na") %>% filter(grepl("HELPER",toupper(description)) == FALSE), y = ~value, color = ~is_vegan, type = "box", text = ~ description)%>%
  layout(boxmode = "group", yaxis = list(title= "MG"), xaxis = list(title= "Sodio"))

sodium

protein <- plot_ly(box_nut %>%  filter(variable == "Protein") %>% filter(grepl("HELPER",toupper(description)) == FALSE), y = ~value, color = ~is_vegan, type = "box", text = ~ description)%>%
  layout(boxmode = "group", yaxis = list(title= "MG"), xaxis = list(title= "Proteina"))

protein


#Análisis cluster: ---------------------------------

# Reescalo las variables creando un nuevo dataframe ---------------------------------

burg_nut4 <- burg_nut3 %>% dplyr::select(-description) %>% 
  filter(fdc_id != 745130) %>%  #Son especias
  filter(fdc_id != 896456) %>%  #burger de salmon 754720
  filter(!fdc_id %in% c(761332,771494,507606,753538,484018)) %>%  #salsas
  
  distinct(fdc_id, .keep_all = TRUE) %>% 
  mutate(is_vegan = case_when(
    is_vegan == TRUE ~ "Vegan",
    TRUE ~ "Meat Based" )) %>% 
  # mutate(row_n = paste(is_vegan)) %>% 
  mutate(row_n = paste(fdc_id,is_vegan)) %>%
  dplyr::select(-fdc_id, -is_vegan ) %>% 
  dplyr::select(row_n, everything())


data_sca <- as.data.frame(scale(burg_nut4[2:15]))
desc <- burg_nut4[,1]

rownames(data_sca) <- desc

# Sumarizo las variables
summary(data_sca)


# Visualización del elbow method ---------------------------------

fviz_nbclust(x = data_sca, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(data_sca, method = "euclidean"), nstart = 50)

# Visualización del dendrograma ---------------------------------

hc_euclidea_completo <- hclust(d = dist(x = data_sca, method = "euclidean"),
                               method = "complete")

fviz_dend(x = hc_euclidea_completo, cex = 0.5, main = "Linkage completo",
          sub = "Distancia euclídea") +
  theme(plot.title =  element_text(hjust = 0.5, size = 15))


# Creacion de los clusters ---------------------------------
km_clusters <- kmeans(x = data_sca, centers = 4, nstart = 50)


n5 <-fviz_cluster(object = km_clusters, data = data_sca, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, labelsize = 8) +
  labs(title = "Resultados clustering K-means") +
  theme_bw()

n5

extract5 <- as.data.frame(km_clusters$cluster) %>% 
  rename("cluster" = "km_clusters$cluster") %>% 
  mutate(nam = lapply(km_clusters$cluster, attributes)) %>% 
  mutate(nam = row.names(.)) %>% 
  mutate_at(vars("nam"), ~str_replace_all(.,"[0-9]| ","")) %>% 
  mutate_at(vars("cluster"), as.character) %>% 
  group_by(cluster,nam) %>% 
  summarise(n = n())


prod_clus <-as.data.frame(km_clusters$cluster) %>% 
  rename("cluster" = "km_clusters$cluster") %>% 
  mutate(nam = lapply(km_clusters$cluster, attributes)) %>% 
  mutate(nam = row.names(.)) %>% 
  mutate(vegan = str_replace_all(nam,"[0-9]| ","")) %>% 
  mutate_at(vars("nam"), ~str_extract(.,"[0-9]{1,10}")) %>% 
  mutate_at(vars("cluster"), as.character) %>% 
  merge(burg_nut3 %>% distinct(fdc_id, .keep_all = TRUE) %>% dplyr::select(fdc_id,description), by.x = "nam" ,by.y = "fdc_id", all.x = TRUE)



nam <-  lapply(km_clusters$cluster, attributes)


# Burguers with DBSCAN ---------------------------------------------------

# PCA to reduce dimensionality

#https://stats.stackexchange.com/questions/235946/pca-before-cluster-analysis
#https://gmaclenn.github.io/articles/airport-pca-analysis/
#https://rpubs.com/elias_jurgen/605966
# https://es.acervolima.com/diferencia-entre-k-means-y-clustering-dbscan/

set.seed(1234)

pca_nci <- prcomp(burg_nut4 %>% dplyr::select(-row_n), scale = TRUE)

summary(pca_nci)

transform <- as.data.frame(pca_nci$x) #Save PCA transform Data
rownames(transform) <- desc



library(FactoMineR)

fviz_screeplot(pca_nci, addlabels = TRUE, ylim = c(0, 20))

fviz_contrib(pca_nci, choice = "var", axes = 2, top = 10)

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)

  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

pcaCharts(pca_nci)

#Try with less PC

transform2 <- transform %>% 
  dplyr::select(PC1,PC2,PC3,PC4,PC5)
   # dplyr::select(-PC11,-PC12,-PC12, -PC14)

rownames(transform2) <- desc


transform2 <- data_sca %>% 
  dplyr::select(Cholesterol, contains("Fat)"),contains("FAT"),contains("Fiber"),CALORIES,Fe,Na,Carbohydrate,Protein)


#DBSCAN cluster:
library(dbscan)
library(fpc)

eps_plot = dbscan::kNNdistplot(transform2, k=5)
eps_plot %>% abline(h = 1.75, lty = 2)

#with al principal components and 2 eps, 5 min pts works, although several outliers detected
#with PC components 1 to 5, and 1.5/1.75 eps, 5 min pts works, less outliers detected

d <- fpc::dbscan(transform2, eps = 1.8,MinPts = 5, scale = FALSE, 
                 method = c("hybrid", "raw", "dist"))



plot(d, transform2, main = "DBSCAN", frame = FALSE)

fviz_cluster(d, transform2, stand = FALSE, frame = FALSE, geom = "point")+
  labs(title = "Resultados clustering PCA-DBSCAN") 

fviz_cluster(object = d, data = transform2, stand = FALSE, frame = FALSE,
              star.plot = TRUE, repel = TRUE, labelsize = 8) +
  labs(title = "Resultados clustering PCA-DBSCAN") +
  theme_bw()





extract <- as.data.frame(d$cluster) %>% 
  rename("cluster" = "d$cluster") %>% 
  mutate(nam = desc) %>% 
  # mutate(nam = row.names(.)) %>% 
  mutate_at(vars("nam"), ~str_replace_all(.,"[0-9]| ","")) %>% 
  mutate_at(vars("cluster"), as.character) %>% 
  group_by(cluster,nam) %>% 
  summarise(n = n())


prod_clus <-as.data.frame(d$cluster) %>% 
  rename("cluster" = "d$cluster") %>% 
  mutate(nam = desc) %>% 
  mutate(vegan = str_replace_all(nam,"[0-9]| ","")) %>% 
  mutate_at(vars("nam"), ~str_extract(.,"[0-9]{1,10}")) %>% 
  mutate_at(vars("cluster"), as.character) %>% 
  merge(burg_nut3 %>% distinct(fdc_id, .keep_all = TRUE) %>% dplyr::select(fdc_id,description), by.x = "nam" ,by.y = "fdc_id", all.x = TRUE)


#Análisis LDA: ---------------------------------

burg_nut5 <- burg_nut3 %>% 
  distinct(fdc_id, .keep_all = TRUE) %>% 
  dplyr::select(-description,-fdc_id) %>% 
  mutate(is_vegan = case_when(
    is_vegan == TRUE ~ "Vegan",
    TRUE  ~  "No_vegan"
  )) %>% 
  dplyr::select(is_vegan, everything()) 


maxs <- apply( burg_nut5[,2:15], 2, max )
mins <- apply( burg_nut5[,2:15], 2, min )
data <- as.data.frame( scale( burg_nut5[,2:15], center = mins, scale = maxs - mins ))

data_food <- cbind( data, "is_vegan" = burg_nut5$is_vegan )

set.seed(1234)
index <- sample( 1:nrow( data_food ), round( nrow( data_food )*0.7 ), replace = FALSE )
X_train <- data_food[ index, ]

# Creo subconjunto de testing (30% de las observaciones)
test <- data_food[ -index, ]






# Creo nuevo dataset con las variables normalizadas y la etiqueta a predecir ---------------------------------


set.seed(1234)
model <- lda( is_vegan ~ ., data = X_train )

# Grafico las dos nuevas dimensiones creadas por el modelo LDA
projected_data <- as.matrix( X_train[, 1:14] ) %*% model$scaling 

plot( projected_data, col = X_train[15], pch = 15 )

corre <- as.data.frame(cor(X_train[, 1:13]))

X_train2 <- X_train %>% 
  mutate(col = case_when(
    is_vegan == "Vegan" ~ "Vegan",
    TRUE ~ "Meat Based"))

data_plot <- as.data.frame(projected_data)

plot <- plot_ly(data = data_plot, y = ~ LD1, color =  X_train2$col, type = "scatter", title = "")

plot


# Modelo logit para la variable vegano/ no vegano. ---------------------------------

burg_nut6 <- burg_nut5 %>% 
  mutate(is_vegan = case_when(
    is_vegan == "Vegan" ~ 1,
    TRUE ~ 0))

modelo_comp <- glm(formula = is_vegan ~ ., data = burg_nut6, family = binomial(link = "logit"))

pander(summary(modelo_comp))


cr <- cor(burg_nut5 %>% select_if(is.numeric), use="complete.obs")
ggcorrplot(cr, hc.order = TRUE,type = "lower",lab = TRUE)


# Repetimos ejercicio pero con carnicos en general. ---------------------------------

# Analizar las hamburguesas ---------------------------------


vegan_explore <- food_brnd %>% 
  # filter(grepl("BURGER",toupper(description))) %>% 
  filter(grepl("VEGGIE|VEGAN|MEAT-FREE|VEGETARIAN",toupper(description))) %>% 
  distinct(description)

vegan_meat <- vegan_products %>% 
  # filter(grepl("BURGER|SAUSAGE|CHICKEN|CHIKN|TURKEY",toupper(description))) %>% 
  filter(grepl("BURGER",toupper(description))) %>%
  merge(nutrient_incoming_name, by = "nutrient_id") %>% 
  mutate(is_vegan = 
           grepl("VEGGIE|VEGAN",toupper(description)))


meat <- food_brnd %>% 
  # filter(grepl("BURGER|SAUSAGE|CHICKEN|CHIKN|TURKEY",toupper(description))) %>% 
  # filter(grepl("BURGER",toupper(description))) %>%
  # filter(grepl("VEGGIE|VEGAN|MEAT-FREE|VEGETARIAN",toupper(description))) %>% 
  arrange(publication_date) %>% 
  distinct() %>% 
  mutate(is_vegan = 
           grepl("VEGGIE|VEGAN|MEAT FREE|VEGETARIAN|MEAT ANALOG|PROTEIN BURGER|BEAN BURGER|MORNINGSTAR|GARDENBURGER",toupper(description)))  #MEATLESS


cat <- meat %>%   
  distinct(fdc_id, branded_food_category,description) %>% 
  group_by(branded_food_category) %>% 
  summarise(n = n())


exclud <- c(  "Pasta Dinners","Biscuits/Cookies (Shelf Stable)","Breads & Buns","Chips/Crisps/Snack Mixes - Natural/Extruded (Shelf Stable)", "Pickles, Olives, Peppers & Relishes","Pizza")

includ <- c("Vegetable Based Products / Meals - Not Ready to Eat (Frozen)",	
            " Frozen Patties and Burgers",	
            "Other Meats", "Frozen Patties and Burgers","Poultry, Chicken & Turkey","Sausages, Hotdogs & Brats","Meat/Poultry/Other Animals  Prepared/Processed",
            "Meat/Poultry/Other Animals Sausages  Prepared/Processed"	,
            "Bacon, Sausages & Ribs","Canned Meat", "Vegetarian Frozen Meats", "Cooked & Prepared")

at2 <- at %>% 
  mutate(catasa = branded_food_category %in% includ)

meats <- meat %>% 
  # filter(!branded_food_category %in% exclud)
  filter(branded_food_category %in% includ)


#select nutrients with less than 50 NAs

meat_nut <-  meats %>% dplyr::select(fdc_id,name, amount) %>% 
  dcast(fdc_id ~ name)

cats_select <- meat_nut %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  melt("fdc_id") %>%  filter(value < 50)

meat_nut2 <- meats %>% dplyr::select(fdc_id,name, amount) %>% 
  filter(!name %in% cats_select$variable) %>% 
  dcast(fdc_id ~ name)

vec <- c("Ca", "CALORIES","Carbohydrate","Cholesterol","Dietary  Fiber", "FAT","Fat, saturated" ,"Fe","Na", "Protein","Total Trans FA","Vitamin A", "Vitamin C","K") 

meat_nut3 <- meats %>% dplyr::select(fdc_id,name, amount) %>% 
  filter(name %in% vec) %>% 
  dcast(fdc_id ~ name) %>% 
  replace(is.na(.), 0) %>% # SE VA A ASUMIR QUE LOS NO IMPUTADOS SON 0
  merge(meats %>% dplyr::select(fdc_id, is_vegan, description), by = "fdc_id")

#Boxplots to see differences:

box_meat <- meat_nut3 %>% 
  melt(c("fdc_id","is_vegan","description")) %>% 
  mutate(is_vegan = case_when(
    is_vegan == TRUE ~ "Vegan",
    TRUE ~ "Meat Based"
    
  ))

fig <- plot_ly(box_meat,x = ~variable, y = ~value, color = ~is_vegan, type = "box", text = ~ description)%>%
  layout(boxmode = "group")


fig


#Analisis LDA

meat_nut5 <- meat_nut3 %>% 
  distinct(fdc_id, .keep_all = TRUE) %>% 
  dplyr::select(-description,-fdc_id) %>% 
  mutate(is_vegan = case_when(
    is_vegan == TRUE ~ "Vegan",
    TRUE  ~  "No_vegan")) %>% 
  dplyr::select(is_vegan, everything()) 

meats <- meat_nut5

maxs <- apply( meats[,2:15], 2, max )
mins <- apply( meats[,2:15], 2, min )
data <- as.data.frame( scale( meats[,2:15], center = mins, scale = maxs - mins ))


meats2 <- cbind( data, "is_vegan" = meat_nut5$is_vegan )

set.seed(1234)
index <- sample( 1:nrow( meats2 ), round( nrow( meats2 )*0.7 ), replace = FALSE )
X_train <- meats2[ index, ]

# Creo subconjunto de testing (30% de las observaciones)
test <- meats2[ -index, ]

# Creo nuevo dataset con las variables normalizadas y la etiqueta a predecir

set.seed(1234)
model <- lda( is_vegan ~ ., data = meats2 )

# Grafico las dos nuevas dimensiones creadas por el modelo LDA
projected_data <- as.matrix( meats2[, 1:14] ) %*% model$scaling 

plot( projected_data, col = meats2[15], pch = 15 )

corre <- as.data.frame(cor(wines2[, 1:13]))

X_train2 <- meats2 %>% 
  mutate(col = case_when(
    is_vegan == "Vegan" ~ "Vegan",
    TRUE ~ "Meat Based"))

data_plot <- as.data.frame(projected_data)

plot <- plot_ly(data = data_plot, y = ~ LD1, color =  X_train2$col, type = "scatter", title = "", text =  meat_nut5_aux$description)

plot

#Clusterizamos los cárnicos en general

meat_nut4 <- meat_nut3 %>% dplyr::select(-description) %>% 
  filter(fdc_id != 745130) %>%  #Son especias
  filter(fdc_id != 896456) %>%  #burger de salmon
  filter(!fdc_id %in% c(761332,771494,753488,577476,577900,577901)) %>%
  filter(fdc_id != 745130) %>%  
  filter(!fdc_id %in% c(761332,771494,507606,753538,484018)) %>%  #TRY
  mutate(is_vegan = case_when(fdc_id %in% c(584626,588606,388618,1042845,718720,1032377,690314,700884) ~ TRUE,
                              TRUE ~ is_vegan)) %>% 
  distinct(fdc_id, .keep_all = TRUE) %>% 
  mutate(row_n = paste(fdc_id,is_vegan)) %>% 
  dplyr::select(-fdc_id, -is_vegan ) %>% 
  dplyr::select(row_n, everything())

burg_nut4 <- meat_nut4

data_sca <- as.data.frame(scale(burg_nut4[2:15]))
desc <- burg_nut4[,1]

rownames(data_sca) <- desc

# Sumarizo las variables
summary(data_sca)



# Visualización del elbow method ---------------------------------

fviz_nbclust(x = data_sca, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(data_sca, method = "euclidean"), nstart = 50)

# Visualización del dendrograma ---------------------------------

hc_euclidea_completo <- hclust(d = dist(x = data_sca, method = "euclidean"),
                               method = "complete")

fviz_dend(x = hc_euclidea_completo, cex = 0.5, main = "Linkage completo",
          sub = "Distancia euclídea") +
  theme(plot.title =  element_text(hjust = 0.5, size = 15))


# Creacion de los clusters ---------------------------------
km_clusters <- kmeans(x = data_sca, centers = 5, nstart = 50)

fviz_cluster(object = km_clusters, data = data_sca, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, labelsize = 8) +
  labs(title = "Resultados clustering K-means") +
  theme_bw()


extract <- as.data.frame(km_clusters$cluster) %>% 
  rename("cluster" = "km_clusters$cluster") %>% 
  mutate(nam = lapply(km_clusters$cluster, attributes)) %>% 
  mutate(nam = row.names(.)) %>% 
  mutate_at(vars("nam"), ~str_replace_all(.,"[0-9]| ","")) %>% 
  mutate_at(vars("cluster"), as.character) %>% 
  group_by(cluster,nam) %>% 
  summarise(n = n())


prod_clus <-as.data.frame(km_clusters$cluster) %>% 
  rename("cluster" = "km_clusters$cluster") %>% 
  mutate(nam = lapply(km_clusters$cluster, attributes)) %>% 
  mutate(nam = row.names(.)) %>% 
  mutate(vegan = str_replace_all(nam,"[0-9]| ","")) %>% 
  mutate_at(vars("nam"), ~str_extract(.,"[0-9]{1,10}")) %>% 
  mutate_at(vars("cluster"), as.character) %>% 
  merge(meat_nut3 %>% distinct(fdc_id, .keep_all = TRUE) %>% dplyr::select(fdc_id,description), by.x = "nam" ,by.y = "fdc_id", all.x = TRUE)



nam <-  lapply(km_clusters$cluster, attributes)


#DBSCAN cluster:
library(dbscan)
library(fpc)

eps_plot = dbscan::kNNdistplot(data_sca, k=5)
eps_plot %>% abline(h = 3, lty = 2)


d <- fpc::dbscan(data_sca, eps = 3, MinPts =  15)

plot(d, data_sca, main = "DBSCAN", frame = FALSE)

fviz_cluster(d, data_sca, stand = FALSE, frame = FALSE, geom = "point")

fviz_cluster(object = d, data = data_sca, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, labelsize = 8) +
  labs(title = "Resultados clustering K-means") +
  theme_bw()

#Export to use in python:

meat_nut5 <- meat_nut3 %>% 
  distinct(fdc_id, .keep_all = TRUE) %>% 
  dplyr::select(fdc_id,description ,is_vegan, everything())

burg_nut4 <- burg_nut3 %>% 
  # dplyr::select(-description) %>% 
  filter(fdc_id != 745130) %>%  #Son especias
  filter(fdc_id != 896456) %>%  #burger de salmon 754720
  filter(!fdc_id %in% c(761332,771494,507606,753538,484018)) %>%  #salsas
  distinct(fdc_id, .keep_all = TRUE) %>% 
  dplyr::select(fdc_id,description ,is_vegan, everything()) %>% 
  mutate(is_vegan = case_when(fdc_id %in% c(584626) ~ TRUE,
         TRUE ~ is_vegan))
  # mutate(is_vegan = case_when(
  #   is_vegan == TRUE ~ "Vegan",
  #   TRUE ~ "Meat Based" )) %>% 
  # # mutate(row_n = paste(is_vegan)) %>% 
  # mutate(row_n = paste(fdc_id,is_vegan)) %>%
  # dplyr::select(-fdc_id, -is_vegan ) %>% 
  # dplyr::select(row_n, everything())


write.table(as.data.frame(meat_nut5), "C:\\Users\\LUCIA\\Downloads\\meat_nut.csv", row.names = FALSE,sep =";")
write.table(as.data.frame(burg_nut4), "C:\\Users\\LUCIA\\Downloads\\burg_nut.csv", row.names = FALSE,sep =";")


