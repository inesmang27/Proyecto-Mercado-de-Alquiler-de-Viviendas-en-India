data = openxlsx::read.xlsx("C:/Users/inesm/OneDrive/Desktop/trabajo inferencia definitivo/houserent.xlsx")
head(data)

nrow(data)
# Función para eliminar outliers usando IQR
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  return(x[x >= lower & x <= upper])
}

# Histograma con color bonito y títulos editados
       # Color del borde de las barras

hist(remove_outliers(data$Rent),
     col = "Orange",          # Color del histograma
     main = "Distribución del Alquiler sin outliers",  # Título del gráfico
     xlab = "Monto del Alquiler",         # Etiqueta del eje X
     ylab = "Frecuencia",                 # Etiqueta del eje Y
     border = "white")         # Color del borde de las barras


#######rent vs Furnishing status

furnished=data[data$Furnishing.Status=='Furnished',]$Rent
semi_furnished=data[data$Furnishing.Status=='Semi-Furnished',]$Rent
unfurnished=data[data$Furnishing.Status=='Unfurnished',]$Rent

#0. graficos
#boxplot
boxplot(Rent ~ Furnishing.Status,
        data = data,
        col = c("lightgreen", "lightblue", "lightpink"),
        main = "Alquiler por estado de amueblado",
        xlab = "Estado de amueblado",
        ylab = "Alquiler (INR)",
        border = "gray40")
#en el boxplot vemos que hay muchos valores atípicos. como el wilcox test trabaja con medianas, no se ve afectado por outliers


# Filtrar y limpiar cada grupo
furnished_sin <- remove_outliers(furnished)
semi_furnished_sin <- remove_outliers(semi_furnished)
unfurnished_sin <- remove_outliers(unfurnished)

furnished_df <- data.frame(Rent = furnished_sin, Furnishing.Status = "Furnished")
semi_df <- data.frame(Rent = semi_furnished_sin, Furnishing.Status = "Semi-Furnished")
unfurnished_df <- data.frame(Rent = unfurnished_sin, Furnishing.Status = "Unfurnished")
# Unir de nuevo los tres grupos limpios
data_limpia <- rbind(furnished_df, semi_df, unfurnished_df)

#nuevo boxplot
boxplot(Rent ~ Furnishing.Status,
        data = data_limpia,
        col = c("lightgreen", "lightblue", "lightpink"),
        main = "Boxplot de alquiler sin outliers (IQR)",
        xlab = "Estado de amueblado",
        ylab = "Alquiler (INR)",
        border = "gray40")
#Este gráfico refleja mejor la tendencia central y las comparaciones entre grupos, sin la influencia de extremos atípicos.

#histogramas por grupo
library(ggplot2)
ggplot(data_limpia, aes(x = Rent, fill = Furnishing.Status)) +
  geom_histogram(binwidth = 2000, position = "dodge") +
  facet_wrap(~Furnishing.Status, scales = "free_y") +
  labs(title = "Distribución de alquiler por tipo de amueblado", x = "Alquiler (INR)", y = "Frecuencia")

#grafico de densidad
library(ggplot2)
ggplot(data_limpia, aes(x = Rent, color = Furnishing.Status)) +
  geom_density() +
  labs(title = "Densidad de alquiler por tipo de amueblado", x = "Alquiler (INR)")




#1. distribución del precio de alquiler en viviendas amuebladas
hist(semi_furnished)
#distribución del precio de alquiler en viviendas amuebladas
hist(unfurnished)
#distribución del precio de alquiler en viviendas amuebladas
hist(furnished)
#observamos que todas tienen una distribución asimétrica positiva


#2. Prueba de normalidad (Shapiro-Wilk) para cada grupo
shapiro.test(furnished) #W = 0.58009, p-value < 2.2e-16
shapiro.test(semi_furnished) #W = 0.23752, p-value < 2.2e-16
shapiro.test(unfurnished) # W = 0.44224, p-value < 2.2e-16
#como cabía esperar se rechaza la normalidad. usaremos test wilcox



#3. Comparaciones 2 a 2

# a) Furnished vs Semi-Furnished. H0: mediana furnished <= mediana semi
wilcox.test(furnished, semi_furnished,alternative="greater") #W = 940140, p-value < 2.2e-16
#hay evidencia estadística de que las viviendas amuebladas (furnished) tienen un alquiler mayor que las semi-amuebladas.

# b) Furnished vs Unfurnished.  H0: mediana furnished <= mediana unfurnished
wilcox.test(furnished, unfurnished,alternative="greater") #W = 903139, p-value < 2.2e-16
#hay evidencia estadística de que las viviendas amuebladas (furnished) tienen un alquiler mayor que las no-amuebladas.

# c) Semi-Furnished vs Unfurnished  H0: mediana semi-furnished <= mediana unfurnished
wilcox.test(semi_furnished, unfurnished,alternative="greater") #W = 2619712, p-value < 2.2e-16
#hay evidencia estadística de que las viviendas semi-amuebladas tienen un alquiler mayor que las no-amuebladas.





###########size vs  Furnishing.Status

furnished_size=data[data$Furnishing.Status=='Furnished',]$Size
semi_size=data[data$Furnishing.Status=='Semi-Furnished',]$Size
unfurnished_size=data[data$Furnishing.Status=='Unfurnished',]$Size

hist(furnished_size)
hist(semi_size)
hist(unfurnished_size)

boxplot(Size ~ Furnishing.Status,
        data = data,
        col = c("lightgreen", "lightblue", "lightpink"),
        main = "Alquiler por estado de amueblado",
        xlab = "Estado de amueblado",
        ylab = "Alquiler (INR)",
        border = "gray40")



#normalidad, con muestras de menor tamaño
furn_size_s=sample(furnished_size,500)
semi_size_s=sample(semi_size,500)
unfurn_size_s=sample(unfurnished_size,500)

shapiro.test(furn_size_s) #W = 0.90411, p-value < 2.2e-16
shapiro.test(semi_size_s) #W = 0.8172, p-value < 2.2e-16
shapiro.test(unfurn_size_s) #W = 0.7258, p-value < 2.2e-16
#como cabía esperar se rechaza la normalidad. usaremos test wilcox. que no tendrá en cuenta los utliers observados en el boxplot

#contrastes 2 a 2
#Furnished vs Semi-Furnished
#H₀: El tamaño medio de Furnished ≤ Semi-Furnished
#H₁: El tamaño medio de Furnished > Semi-Furnished
wilcox.test(furnished_size, semi_size, alternative = "greater") #W = 742972, p-value = 0.8764
#no hay evidencia para rechazar H0
wilcox.test(furnished_size, semi_size, alternative = "less") #W = 742972, p-value = 0.1236
#No hay evidencia suficiente de que las viviendas Furnished sean más grandes que las semi-furnished

#Furnished vs Unfurnished
#H₀: El tamaño medio de Furnished ≤ unfurnished
#H₁: El tamaño medio de Furnished > unfurnished
wilcox.test(furnished_size, unfurnished_size, alternative = "greater") #W = 766994, p-value < 2.2e-16
#se rechaza H0. Las viviendas Furnished son significativamente más grandes que las Unfurnished.

#semi-Furnished vs Unfurnished
#H₀: El tamaño medio de semi- Furnished ≤ unfurnished
#H₁: El tamaño medio de semi- Furnished > unfurnished
wilcox.test(semi_size, unfurnished_size, alternative = "greater") #W = 2617772, p-value < 2.2e-16
#se rechaza H0. Las viviendas semi-Furnished son significativamente más grandes que las Unfurnished.


# Crear categorías de tamaño (pequeño, medio, grande)
# Eliminar outliers primero si es necesario, y luego crear categorías de tamaño
data$SizeCategory <- cut(data$Size,
                         breaks = c(-Inf, 800, 1600, Inf),
                         labels = c("Pequeño", "Mediano", "Grande"))


# Diagrama de barras combinado
library(ggplot2)
ggplot(data, aes(x = SizeCategory, fill = Furnishing.Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Tamaño de vivienda por estado de amueblado",
       x = "Tamaño de vivienda",
       y = "Frecuencia",
       fill = "Estado de amueblado") +
  scale_fill_manual(values = c("Furnished" = "lightgreen",
                               "Semi-Furnished" = "lightblue",
                               "Unfurnished" = "lightpink")) +
  theme_minimal()



# Crear data frame limpio
furnished_df <- data.frame(Rent = furnished, Estado = "Furnished")
semi_furnished_df <- data.frame(Rent = semi_furnished, Estado = "Semi-Furnished")
unfurnished_df <- data.frame(Rent = unfurnished, Estado = "Unfurnished")

data_clean <- rbind(furnished_df, semi_furnished_df, unfurnished_df)

# Boxplot sin outliers
boxplot(Rent ~ Estado,
        data = data_clean,
        col = c("lightgreen", "lightblue", "lightpink"),
        main = "Boxplot de alquiler sin outliers (IQR)",
        ylab = "Alquiler (INR)",
        xlab = "Estado de amueblado")

# Gráfico de barras apiladas con proporciones
ggplot(data, aes(x = SizeCategory, fill = Furnishing.Status)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución proporcional del estado de amueblado según el tamaño",
       x = "Tamaño de vivienda",
       y = "Proporción",
       fill = "Estado de amueblado") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Furnished" = "lightgreen",
                               "Semi-Furnished" = "lightblue",
                               "Unfurnished" = "lightpink")) +
  theme_minimal()
#indica segmentacion de mercado. casas grandes y amuebladas son de lujo.





########## size vs rent
hist(data$Size)

boxplot(Rent ~ SizeCategory,
        data = data,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Boxplot de Alquiler según Tamaño de Vivienda",
        ylab = "Alquiler (INR)",
        xlab = "Categoría de Tamaño")

# Histograma por categoría
hist(sample(data$Rent[data$SizeCategory == "Pequeño"],500), main="Pequeño", col="skyblue")
hist(sample(data$Rent[data$SizeCategory == "Mediano"],500), main="Mediano", col="lightgreen")
hist(sample(data$Rent[data$SizeCategory == "Grande"], 500),main="Grande", col="lightpink")

#normaliad. por los outliers lo comprobamos en un sample
shapiro.test(sample(data$Rent[data$SizeCategory == "Pequeño"],500)) 
shapiro.test(sample(data$Rent[data$SizeCategory == "Mediano"],500))
shapiro.test(sample(data$Rent[data$SizeCategory == "Grande"],500))
#se rechaza

# Comparación Pequeño vs Mediano
wilcox.test(data$Rent[data$SizeCategory == "Pequeño"],
            data$Rent[data$SizeCategory == "Mediano"],
            alternative = "less")  # H0: Pequeño ≥ Mediano
#se rechaza h0

# Mediano vs Grande
wilcox.test(data$Rent[data$SizeCategory == "Mediano"],
            data$Rent[data$SizeCategory == "Grande"],
            alternative = "less")
#se rechaza h0

# Pequeño vs Grande
wilcox.test(data$Rent[data$SizeCategory == "Pequeño"],
            data$Rent[data$SizeCategory == "Grande"],
            alternative = "less")
#se rechaza h0

# Calcular la media del alquiler por categoría de tamaño
medias_por_tamano <- aggregate(Rent ~ SizeCategory, data = data, FUN = median, na.rm = TRUE)

# Crear el gráfico
library(ggplot2)

ggplot(medias_por_tamano, aes(x = SizeCategory, y = Rent, fill = SizeCategory)) +
  geom_col() +
  labs(title = "Alquiler mediano por categoría de tamaño",
       x = "Tamaño de vivienda",
       y = "Alquiler medio (INR)") +
  theme_minimal()
#el salto del alquiler de viviendas grande es muy alto. viviendas grandes -> sector de lujo

#se puede observar que, dado que no hay evidencia de que el tamaño sea mas grande en viviendas furnished
#que en semi furnished pero estas son mas caras, el amueblado añade bastante valor. 

pequeño <- data$Rent[data$SizeCategory == "Pequeño"]
med <- data$Rent[data$SizeCategory == "Mediano"]
grande <- data$Rent[data$SizeCategory == "Grande"]


pequeño_df <- data.frame(Rent = remove_outliers(pequeño), Grupo = "Pequeño")
med_df <- data.frame(Rent = remove_outliers(med), Grupo = "Mediano")
grande_df <- data.frame(Rent = remove_outliers(grande), Grupo = "Grande")


# Unir todo en un solo data frame
size_data <- rbind(pequeño_df, med_df, grande_df)

# Dibujar histogramas comparables con facetado
ggplot(size_data, aes(x = Rent, fill = Grupo)) +
  geom_histogram(binwidth = 10000, color = "black", alpha = 0.7) +
  facet_wrap(~Grupo, scales = "free_y") +
  labs(title = "Distribución del alquiler por tipo de inquilino preferido (sin outliers)",
       x = "Alquiler (INR)",
       y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Pequeño" = "#8da0cb",
                               "Mediano" = "#fc8d62",
                               "Grande" = "#66c2a5")) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")


boxplot(Rent ~ Grupo,
        data = size_data,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Boxplot de Alquiler según Tamaño de Vivienda (sin outliers)",
        ylab = "Alquiler (INR)",
        xlab = "Categoría de Tamaño")


# Calcular las proporciones por grupo
tabla_S <- table(data$SizeCategory)
proporciones <- round(100 * tabla_S / sum(tabla_S), 1)

# Crear etiquetas con porcentaje
etiquetasS <- paste(names(tabla_S), " - ", proporciones, "%", sep = "")

# Gráfico de queso
pie(tabla_S,
    labels = etiquetasS,
    main = "Proporción de viviendas por categoría de tamaño",
    col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
    border = "white")





#INTERACCION SIZE FURNITURA
#¿afecta igual el amueblado al precio en todos los tamaños? 

#comparaciones
for (cat in levels(data$SizeCategory)) {
  cat_data <- subset(data, SizeCategory == cat)
  
  cat(paste("\n\n== Tamaño:", cat, "==\n"))
  
  f <- cat_data$Rent[cat_data$Furnishing.Status == "Furnished"]
  s <- cat_data$Rent[cat_data$Furnishing.Status == "Semi-Furnished"]
  u <- cat_data$Rent[cat_data$Furnishing.Status == "Unfurnished"]
  
  print(wilcox.test(f, s, alternative = "greater")) 
  print(wilcox.test(f, u, alternative = "greater"))
  print(wilcox.test(s, u, alternative = "greater"))
}
#pequeño: todas se rechazan muy significativamwnte. precio: f>s>u
#mediano: todas se rechazan muy significativamwnte. precio: f>s>u
#grande: las diferencias son menos significativas. encontramos que f>u y f>s pero entre u y s no hay diferencia significativa.
#luego en las casas grande la media del alquiler entre casas unfurnished y semi furnished no presenta diferencias significativas. Como inquilino es una buena oportunidad
#para alguilar semi-amueblado y para arrendatores es mejor ahorrarse el amueblado ya no que no aumenta el precio del alquiler de forma significativa.


# Calcular la mediana del alquiler por categoría
medianas_furn <- aggregate(Rent ~ SizeCategory + Furnishing.Status, data = data, FUN = median, na.rm = TRUE)

# Crear gráfico de barras agrupadas
ggplot(medianas_furn, aes(x = SizeCategory, y = Rent, fill = Furnishing.Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Alquiler mediano por Tamaño y Amueblado",
       x = "Tamaño de vivienda",
       y = "Alquiler medio (INR)") +
  theme_minimal()




###########Tenant Preferred vs Rent
#¿El tipo de inquilino preferido por el arrendador influye en el alquiler mensual?

boxplot(Rent ~ Tenant.Preferred,
        data = data,
        col = c("lightblue", "lightgreen", "lightpink", "lightgray"),
        main = "Alquiler según tipo de inquilino preferido",
        ylab = "Alquiler (INR)",
        xlab = "Inquilino preferido")

# Predefinir vectores para cada grupo de inquilino
bachelors <- data$Rent[data$Tenant.Preferred == "Bachelors"]
family <- data$Rent[data$Tenant.Preferred == "Family"]
both <- data$Rent[data$Tenant.Preferred == "Bachelors/Family"]

length(bachelors)
length(family)
length(both)


#normalidad
shapiro.test(sample(bachelors,500))
shapiro.test(family)
shapiro.test(sample(both,500))
#se rechaza. usamos wilcox

#comparaciones
wilcox.test(family, bachelors, alternative = "greater") #W = 224251, p-value = 6.773e-06
#el alquiler para familias es más caro.

wilcox.test(family, both, alternative = "greater") #W = 1080586, p-value < 2.2e-16
#el alquiler para familias es más caro.

wilcox.test(both, bachelors, alternative = "less") #W = 1167969, p-value < 2.2e-16
#el alquiler en both es significativamente más bajo

# Calcular la mediana de alquiler por tipo de inquilino
medianas_rent <- aggregate(Rent ~ Tenant.Preferred, data = data, FUN = median, na.rm = TRUE)
# Ordenar por mediana (opcional)
medianas_rent <- medianas_rent[order(medianas_rent$Rent, decreasing = TRUE), ]

# Gráfico de barras con mediana
ggplot(medianas_rent, aes(x = reorder(Tenant.Preferred, -Rent), y = Rent, fill = Tenant.Preferred)) +
  geom_col() +
  labs(title = "Alquiler mediano según inquilino preferido",
       x = "Tipo de inquilino",
       y = "Alquiler mediano (INR)") +
  theme_minimal() +
  theme(legend.position = "none")



#eliminamos outliers y hacemos histogramas
bachelors_sin <- remove_outliers(bachelors)
hist(bachelors_sin)

both_sin <-remove_outliers(both)
hist(both_sin)

family_sin <-remove_outliers(family)
hist(family_sin)


# Crear un data frame unificado con etiquetas
bachelors_df <- data.frame(Rent = bachelors_sin, Grupo = "Bachelors")
both_df <- data.frame(Rent = both_sin, Grupo = "Bachelors/Family")
family_df <- data.frame(Rent = family_sin, Grupo = "Family")

# Unir todo en un solo data frame
rent_data <- rbind(bachelors_df, both_df, family_df)

# Dibujar histogramas comparables con facetado
ggplot(rent_data, aes(x = Rent, fill = Grupo)) +
  geom_histogram(binwidth = 4000, color = "black", alpha = 0.7) +
  facet_wrap(~Grupo, scales = "free_y") +
  labs(title = "Distribución del alquiler por tipo de inquilino preferido (sin outliers)",
       x = "Alquiler (INR)",
       y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Bachelors" = "#8da0cb",
                               "Bachelors/Family" = "#fc8d62",
                               "Family" = "#66c2a5")) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")
#el grafico de family presenta una cola a la derecha mas alta


#las viviendas que aceptan family y bachelor son las mas frecuentes: puede interpretarse como una estrategia orientada a maximizar la ocupación en lugar de seleccionar un perfil específico.
# Calcular las proporciones por grupo
tabla <- table(data$Tenant.Preferred)
proporciones <- round(100 * tabla / sum(tabla), 1)

# Crear etiquetas con porcentaje
etiquetas <- paste(names(tabla), " - ", proporciones, "%", sep = "")

# Gráfico de queso
pie(tabla,
    labels = etiquetas,
    main = "Proporción de viviendas por tipo de inquilino preferido",
    col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
    border = "white")




#########Rent vs point of contact

rent_owner   <- data$Rent[data$Point.of.Contact == "Contact Owner"]
rent_agent   <- data$Rent[data$Point.of.Contact == "Contact Agent"]

rent_agent2 <- remove_outliers(rent_agent)
hist(rent_agent2)

rent_owner2 <- remove_outliers(rent_owner)
hist(rent_owner2)

shapiro.test(rent_agent)
shapiro.test(rent_owner)
#se rechaza

wilcox.test(rent_owner, rent_agent, alternative = "less")
#se rechaza H0. el alquiler de viviendas ofreciadas por propietarios es mas bajo

#boxplot sin outliers
df_rent_clean <- rbind(
  data.frame(Rent = rent_owner2, Contact = "Owner"),
  data.frame(Rent = rent_agent2, Contact = "Agent")
)
ggplot(df_rent_clean, aes(x = Contact, y = Rent, fill = Contact)) +
  geom_boxplot(outlier.shape = NA) +  # Oculta visualmente los outliers
  labs(title = "Distribución del alquiler por tipo de contacto (sin outliers)",
       x = "Punto de contacto", y = "Alquiler (INR)") +
  scale_fill_manual(values = c("Owner" = "#66c2a5", "Agent" = "#fc8d62")) +
  theme_minimal()

#diagrama de medianas
medianas_rent <- aggregate(Rent ~ Contact, data = df_rent_clean, FUN = median)

# Gráfico de barras
ggplot(medianas_rent, aes(x = Contact, y = Rent, fill = Contact)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Rent, 0)), vjust = -0.5, size = 5) +
  labs(title = "Alquiler mediano por tipo de contacto (sin outliers)",
       x = "Punto de contacto", y = "Alquiler mediano (INR)") +
  scale_fill_manual(values = c("Owner" = "#66c2a5", "Agent" = "#fc8d62")) +
  theme_minimal()





#Los agentes intermedian propiedades más caras, posiblemente porque:
#Representan propietarios más exigentes o zonas premium.
#Cobran comisiones e influyen en el precio final.

#TIPO DE CONTACTO VS TAMAÑO
size_owner <- data$Size[data$Point.of.Contact == "Contact Owner"]
size_agent <- data$Size[data$Point.of.Contact == "Contact Agent"]

shapiro.test(sample(size_agent,500))
shapiro.test(sample(size_owner,500))
#se rechaza
wilcox.test(size_agent, size_owner, alternative = "greater")
#Los agentes suelen gestionar viviendas más grandes, posiblemente para maximizar comisión.

contact <- subset(data, Point.of.Contact %in% c("Contact Owner", "Contact Agent"))

ggplot(contact, aes(x = Size, fill = Point.of.Contact)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución del tamaño de vivienda según tipo de contacto",
       x = "Tamaño (ft²)", y = "Densidad",
       fill = "Contacto") +
  scale_fill_manual(values = c("Contact Owner" = "#66c2a5", "Contact Agent" = "#fc8d62")) +
  theme_minimal(base_size = 14)


#TIPO DE CONTACTO VS INQUILINO
#¿Los distintos tipos de contacto (Owner, Agent) prefieren diferentes tipos de inquilinos (Bachelors, Family, Bachelors/Family)?
#¿Hay diferencias significativas en las proporciones de inquilinos preferidos (Tenant.Preferred) entre Owner y Agent? 
  
# Tabla de frecuencias
tabla <- table(contact$Point.of.Contact, contact$Tenant.Preferred)

# Totales por grupo
total_owner <- sum(tabla["Contact Owner", ])
total_agent <- sum(tabla["Contact Agent", ])
totales <- c(total_owner, total_agent)

# Bachelors: ¿mayor proporción entre Agents?
prop.test(c(tabla["Contact Owner", "Bachelors"], tabla["Contact Agent", "Bachelors"]),
          totales, alternative = "less", correct = FALSE) 
#se rechaza. prop1:0.1231343  prop2:0.2838457. Para bachelors mas ofrecidas por agents


# Bachelors/Family: ¿mayor proporción entre Owners?
prop.test(c(tabla["Contact Owner", "Bachelors/Family"], tabla["Contact Agent", "Bachelors/Family"]),
          totales, alternative = "greater", correct = FALSE)
#se rechaza. prop1:0.8065920  prop2:0.5552649.  "Bachelors/Family" mas ofrecidas por propietarios

# Family: ¿igual proporción?
prop.test(c(tabla["Contact Owner", "Family"], tabla["Contact Agent", "Family"]),
          totales, alternative = "less", correct = FALSE)
#se rechaza. prop1:0.07027363   prop2:0.16088947. family mas ofreciadas por agents

#las mas caras son ofrecidas por agentes

#diagrama de barras de proporciones:
# Crear tabla de frecuencias
tabla_df <- as.data.frame(table(contact$Point.of.Contact, contact$Tenant.Preferred))
colnames(tabla_df) <- c("Contacto", "Inquilino", "Frecuencia")

# Gráfico de proporciones
ggplot(tabla_df, aes(x = Contacto, y = Frecuencia, fill = Inquilino)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proporción de tipo de inquilino por tipo de contacto",
       x = "Punto de contacto",
       y = "Proporción (%)",
       fill = "Inquilino preferido") +
  scale_fill_manual(values = c("Bachelors" = "salmon",
                               "Bachelors/Family" = "skyblue",
                               "Family" = "palegreen")) +
  theme_minimal(base_size = 14)




#######Rent vs región

#definimos dos zonas: norte y sur y una nueva columna que sea region
sur<- data[data$City %in% c('Bangalore', 'Chennai', 'Hyderabad'), ]
norte <- data[data$City %in% c('Delhi', 'Kolkata', 'Mumbai'), ]
data$Region <- NA
data$Region[data$City %in% c('Bangalore', 'Chennai', 'Hyderabad')] <- "Sur"
data$Region[data$City %in% c('Delhi', 'Kolkata', 'Mumbai')] <- "Norte"

#variables
rentnorte <- norte$Rent
rentsur <- sur$Rent
rentnorte2 <- remove_outliers(rentnorte)
rentsur2 <- remove_outliers(rentsur)


library(ggplot2)
# Crear data frame con etiquetas de región
rent_norte_df <- data.frame(Rent = rentnorte2, Region = "Norte")
rent_sur_df   <- data.frame(Rent = rentsur2, Region = "Sur")
rent_region_data <- rbind(rent_norte_df, rent_sur_df)

# Crear histograma con facetado por región (doble)

ggplot(rent_region_data, aes(x = Rent, fill = Region)) +
  geom_histogram(binwidth = 4000, color = "black", alpha = 0.7) +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Distribución del alquiler en India por región (sin outliers)",
       x = "Alquiler (INR)",
       y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Norte" = "#fc8d62",
                               "Sur"   = "#66c2a5")) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "none")

#otros
hist(rentnorte2, freq = TRUE, col=terrain.colors(10), main = "Rental housing in North India") # Frecuencia absoluta
hist(rentsur2, freq = TRUE, col=terrain.colors(10), main = "Rental housing in Sur India") # Frecuencia absoluta
boxplot(rentnorte2, rentsur2,
        names = c("North", "South"),
        col = c("turquoise", "gold"),
        main = "Comparison between rent in North and South India",
        ylab = "Rent")

#Comprobamos normalidad de las poblaciones norte y sur 
shapiro.test(rentnorte2)
shapiro.test(rentsur2)
#como los p-valores son tan bajos no tenemos normalidad en ninguna de las dos poblaciones

#Hacemos contraste no paramétrico
#Ho: mediana de renta en ciudades del norte menor o igual que la del sur
wilcox.test(rentnorte2, rentsur2, alternative="greater")
#2.2e-16 rechazamos la hipotesis nula. Por tanto la mediana de norte es mayor que la de sur


#¿QUÉ FACTORES AFECTAN A QUE LAS CASAS DEL NORTE VALGAN MÁS?

#COMPARACION NIVEL DE AMUEBLADO NORTE Y SUR 
table(data$Furnishing.Status)

mueblesnorte <- norte$Furnishing.Status
mueblessur <- sur$Furnishing.Status
table(mueblesnorte)
table(mueblessur)

#diagramas de barras
tablamueblesnorte <- table(mueblesnorte)
barplot(tablamueblesnorte,
        main = "Furnished status in northern cities",
        col = c("salmon", "skyblue", "palegreen"))

tablamueblessur <- table(mueblessur)
barplot(tablamueblessur,
        main = "Furnished status in southern cities",
        col = c("salmon", "skyblue", "palegreen"))


#diagrama tipo pie
# Paso 1: Crear tabla de frecuencias
amueblado_region <- aggregate(x = list(Frecuencia = rep(1, nrow(data))),
                              by = list(Region = data$Region, Furnishing.Status = data$Furnishing.Status),
                              FUN = length)

# Paso 2: Calcular proporciones por región
amueblado_region$TotalRegion <- ave(amueblado_region$Frecuencia, amueblado_region$Region, FUN = sum)
amueblado_region$Prop <- amueblado_region$Frecuencia / amueblado_region$TotalRegion

# Paso 3: Gráfico circular (pie chart) con facetado por región
ggplot(amueblado_region, aes(x = "", y = Prop, fill = Furnishing.Status)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~Region) +
  labs(title = "Distribución del estado de amueblado por región",
       fill = "Amueblado") +
  theme_void() +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))
#observamos que en el norte hay mas viviendas furnished y unfurnished pero menos semifurnished que en el sur


#comprobaccion de que la proporcion de casas furnished en el norte es mayor que la del sur:
#Comparamos la proporción de Furnished en norte y sur
# Crear tabla con número de "Furnished" y "No-Furnished" por región
# Muebles Norte: 397 Furnished, resto no
# Muebles Sur: 283 Furnished, resto no

x <- c(397, 283)  # éxitos (Furnished)
n <- c(397 + 817 + 887, 283 + 1434 + 928)  # totales por región

# Comparar proporciones
prop.test(x = x, n = n, alternative = "greater", correct = FALSE)
#Ho: la proporción de casas amuebladas en el norte de india es menor que en el sur
#p-value = 5.965e-16
#prop 1    prop 2 
#0.1889576 0.1069943 
#rechazamos la hipótesis nula. Podemos decir que las casas del norte están más amuebladas


#COMPARACION TAMAÑO CASAS NORTE Y SUR 
sizenorte <- norte$Size
sizesur <- sur$Size

sizenorte2 <- remove_outliers(sizenorte)
sizesur2 <- remove_outliers(sizesur)

boxplot(sizenorte2, sizesur2,
        names = c("North", "South"),
        col = c("turquoise", "gold"),
        main = "Comparison between size in North and South India",
        ylab = "Size")
hist(sizenorte2, freq = TRUE, col=terrain.colors(10), main = "Housing size in North India") # Frecuencia absoluta
hist(sizesur2, freq = TRUE, col=terrain.colors(10), main = "Housing size in Sur India") # Frecuencia absoluta

# Crear categorías de tamaño
data$SizeCategory <- cut(data$Size,
                         breaks = c(-Inf, 800, 1600, Inf),
                         labels = c("Pequeño", "Mediano", "Grande"))

# Asignar región (si aún no está creado)
data$Region <- NA
data$Region[data$City %in% c("Delhi", "Kolkata", "Mumbai")] <- "Norte"
data$Region[data$City %in% c("Bangalore", "Chennai", "Hyderabad")] <- "Sur"

# Calcular frecuencias por región y tamaño
tabla_tamano <- table(data$Region, data$SizeCategory)
tabla_tamano_df <- as.data.frame(tabla_tamano)
colnames(tabla_tamano_df) <- c("Region", "Tamaño", "Frecuencia")

# Calcular proporciones dentro de cada región
tabla_tamano_df$TotalRegion <- ave(tabla_tamano_df$Frecuencia, tabla_tamano_df$Region, FUN = sum)
tabla_tamano_df$Prop <- tabla_tamano_df$Frecuencia / tabla_tamano_df$TotalRegion

# Pie chart con facetado por región
library(ggplot2)

ggplot(tabla_tamano_df, aes(x = "", y = Prop, fill = Tamaño)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~Region) +
  labs(title = "Distribución del tamaño de viviendas por región",
       fill = "Tamaño") +
  theme_void() +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))


#normalidad
shapiro.test(sizenorte2)
shapiro.test(sizesur2)
#descartamos normalidad 

#Hacemos contraste no paramétrico
#Ho: mediana del tamaño de las casas en ciudades del sur menor o igual que la del norte
wilcox.test(sizesur2, sizenorte2, alternative="greater")
#rechazamos Ho. Son más grandes en el norte



#COMPARACION TIPO DE INQUILINO PREFERIDO EN EL NORTE Y EN EL 
#Creemos que las casas del norte se alquilan más a familias porque hay más a turistas 
tenantnorte <-norte$Tenant.Preferred
tenantsur <-sur$Tenant.Preferred

# Tablas de frecuencia
tablatenantnorte <- table(tenantnorte)
tablatenantsur <- table(tenantsur)

# Barplots lado a lado para comparar
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

barplot(tablatenantnorte,
        main = "Tenant preferred type in the north",
        col = c("salmon", "skyblue", "palegreen"),
        ylim = c(0, max(tablatenantsur, tablatenantnorte) * 1.1))

barplot(tablatenantsur,
        main =  "Tenant preferred type in the south",
        col = c("salmon", "skyblue", "palegreen"),
        ylim = c(0, max(tablatenantsur, tablatenantnorte) * 1.1))

#test proporciones:
# Inquilinos Norte: 456 Bachelors, 1425 Bachelors/Family, 220 Family
# Inquilinos Sur: 374 Bachelors, 2019 Bachelors/Family, 252 Family

# Datos de frecuencia por grupo
#           Norte   Sur
bachelors        <- c(456,   374)
bachelors_family <- c(1425, 2019)
family           <- c(220,   252)

# Totales por región
totalinquilinos <- bachelors + bachelors_family + family

# Comparar proporciones
prop.test(bachelors, totalinquilinos, alternative = "greater", correct = FALSE) #prop 1:0.2170395   prop2: 0.1413989 
prop.test(bachelors_family, totalinquilinos, alternative = "less", correct = FALSE) #prop 1 0.6782485    prop 2 0.7633270
prop.test(family, totalinquilinos, alternative = "two.sided", correct = FALSE) #prop1: 0.1047120   prop2:  0.0952741 
#donde se ofrecen más casas para bachelors es en el norte, 
#donde se ofrecen más casas para bachelors/family es en el sur
#se ofrecen en igual proporcion para family. no hay diferencia significativa

#En el norte, se prefieren más los bachelors (solteros), posiblemente por zonas urbanas o universitarias.
#En el sur, predominan las viviendas más “flexibles” para bachelors/family, lo cual puede indicar mayor adaptabilidad del mercado.


#diagrama barras apiladas
tenant_norte_df <- data.frame(Region = "Norte", Grupo = tenantnorte)
tenant_sur_df   <- data.frame(Region = "Sur",   Grupo = tenantsur)
tenant_regiones <- rbind(tenant_norte_df, tenant_sur_df)

ggplot(tenant_regiones, aes(x = Region, fill = Grupo)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proporción de tipos de inquilino preferido por región",
       x = "Región",
       y = "Proporción (%)",
       fill = "Inquilino preferido") +
  scale_fill_manual(values = c("Bachelors" = "salmon",
                               "Bachelors/Family" = "skyblue",
                               "Family" = "palegreen")) +
  theme_minimal(base_size = 14)
#el grupo family que en general es el mas caro no es el predominante en el norte, depende el precio de la región?



#OTROS FACTORES
#Depende entonces que el alquiler sea más alto de las características de la casa, o es un factor del norte

#Para las totalmente AMUEBLADAS
rentnortefurnished <- subset(data, City %in% c("Delhi", "Kolkata", "Mumbai") & Furnishing.Status == "Furnished")$Rent
rentsurfurnished <- subset(data, City %in% c("Bangalore", "Chennai", "Hyderabad") & Furnishing.Status == "Furnished")$Rent

rentnortefurnished2 <- remove_outliers(rentnortefurnished)
rentsurfurnished2 <- remove_outliers(rentsurfurnished)
shapiro.test(rentnortefurnished2)
shapiro.test(rentsurfurnished2)
#Rechazamos normalidad
wilcox.test(rentsurfurnished2, rentnortefurnished2, alternative="less")
#siguen valiendo más las del norte

#Para las SEMI-AMUEBLADAS
rentnortesemifurnished <- subset(data, City %in% c("Delhi", "Kolkata", "Mumbai") & Furnishing.Status == "Semi-Furnished")$Rent
rentsursemifurnished <- subset(data, City %in% c("Bangalore", "Chennai", "Hyderabad") & Furnishing.Status == "Semi-Furnished")$Rent

rentnortesemifurnished2 <- remove_outliers(rentnortesemifurnished)
rentsursemifurnished2 <- remove_outliers(rentsursemifurnished)
shapiro.test(rentnortesemifurnished2)
shapiro.test(rentsursemifurnished2)
#Rechazamos normalidad
wilcox.test(rentsursemifurnished, rentnortesemifurnished, alternative="two.sided")
#siguen valiendo más las del norte

#Para las no amuebladas
rentnorteunfurnished <- subset(data, City %in% c("Delhi", "Kolkata", "Mumbai") & Furnishing.Status == "Unfurnished")$Rent
rentsurunfurnished <- subset(data, City %in% c("Bangalore", "Chennai", "Hyderabad") & Furnishing.Status == "Unfurnished")$Rent

rentnorteunfurnished2 <- remove_outliers(rentnorteunfurnished)
rentsurunfurnished2 <- remove_outliers(rentsurunfurnished)
shapiro.test(rentnorteunfurnished2)
shapiro.test(rentsurunfurnished2)
#Rechazamos normalidad
wilcox.test(rentsurunfurnished, rentnorteunfurnished, alternative="less")
#siguen valiendo más las del norte



#TAMAÑOS
# Eliminar outliers primero si es necesario, y luego crear categorías de tamaño
data$SizeCategory <- cut(data$Size,
                         breaks = c(-Inf, 800, 1600, Inf),
                         labels = c("Pequeño", "Mediano", "Grande"))
#Para las casas pequeñas 
rentnortepeque <- subset(data, City %in% c("Delhi", "Kolkata", "Mumbai") & SizeCategory == 'Pequeño')$Rent
rentsurpeque <- subset(data, City %in% c("Bangalore", "Chennai", "Hyderabad") & SizeCategory == 'Pequeño')$Rent

rentnortepeque2 <- remove_outliers(rentnortepeque)
rentsurpeque2 <- remove_outliers(rentsurpeque)
hist(rentnortepeque2, freq = TRUE, col=terrain.colors(10), main= "Renting small houses in North India") # Frecuencia absoluta
hist(rentsurpeque2, freq = TRUE, col=terrain.colors(10), main= "Renting small houses in South India") # Frecuencia absoluta

shapiro.test(rentnortepeque2)
shapiro.test(rentsurpeque2)
#rechazamos normalidad
wilcox.test(rentsurpeque, rentnortepeque, alternative="less")
#las casas pequeñas de india son más caras en el norte 


#Para las casas medianas
rentnortemed <- subset(data, City %in% c("Delhi", "Kolkata", "Mumbai") & SizeCategory == 'Mediano')$Rent
rentsurmed <- subset(data, City %in% c("Bangalore", "Chennai", "Hyderabad") & SizeCategory == 'Mediano')$Rent

rentnortemed2 <- remove_outliers(rentnortemed)
rentsurmed2 <- remove_outliers(rentsurmed)
hist(rentnortemed2, freq = TRUE, col=terrain.colors(10), main= "Renting medium houses in North India") # Frecuencia absoluta
hist(rentsurmed2, freq = TRUE, col=terrain.colors(10), main= "Renting medium houses in South India") # Frecuencia absoluta

shapiro.test(rentnortemed2)
shapiro.test(rentsurmed2)
#rechazamos normalidad
wilcox.test(rentsurmed, rentnortemed, alternative="less")
#las casas medianas de india son más caras en el sur 



#Para las casas grandes
rentnortegrande <- subset(data, City %in% c("Delhi", "Kolkata", "Mumbai") & SizeCategory == 'Grande')$Rent
rentsurgrande <- subset(data, City %in% c("Bangalore", "Chennai", "Hyderabad") & SizeCategory == 'Grande')$Rent

rentnortegrande2 <- remove_outliers(rentnortegrandes)
rentsurgrande2 <- remove_outliers(rentsurgrande)
hist(rentnortegrande2, freq = TRUE, col=terrain.colors(10), main= "Renting large houses in North India") # Frecuencia absoluta
hist(rentsurgrande2, freq = TRUE, col=terrain.colors(10), main= "Renting large houses in South India") # Frecuencia absoluta

shapiro.test(rentnortegrande2)
shapiro.test(rentsurgrande2)
#rechazamos normalidad
wilcox.test(rentsurgrande, rentnortegrande, alternative="less")
#las casas grandes de india son más caras en el sur 





#segmentos
data$Segmento <- with(data, ifelse(Furnishing.Status == "Furnished" & 
                                     SizeCategory == "Grande" & 
                                     Tenant.Preferred == "Family", "Premium",
                                   ifelse(Furnishing.Status == "Unfurnished" & 
                                            SizeCategory == "Pequeño" & 
                                            Tenant.Preferred == "Bachelors/Family", "Económico",
                                          "Medio")))


# Tabla cruzada
table(data$Region, data$Segmento)

# Proporciones por región
prop.table(table(data$Region, data$Segmento), margin = 1) * 100


library(ggplot2)

ggplot(data, aes(x = Region, fill = Segmento)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución proporcional de segmentos por región",
       x = "Región",
       y = "Proporción",
       fill = "Segmento de mercado") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()