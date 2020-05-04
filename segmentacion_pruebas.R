#Leer los datos
datos.bruto <- read_excel("Online Retail.xlsx", sheet = "Online Retail", na = "NA")
#Creamos datos de respaldo
data <- datos.bruto
str(data)
#Se convierte en factor las variables necesarias
data$InvoiceNo <- as.factor(as.character(data$InvoiceNo))
data$StockCode <- as.factor(as.character(data$StockCode))
data$Description <- as.factor(as.character(data$Description))
data$CustomerID <- as.factor(as.character(data$CustomerID))
data$Country <- as.factor(as.character(data$Country))
#cuantas personas están en el dataset
length(unique(data$CustomerID))
#Existe algunos vacios?
sum(is.na(data$CustomerID))
#Se hace de nuevo otro respaldo
dataold1<- data
#Se quitan los NA´s del dataset
data <- subset(data, !is.na(data$CustomerID))
#Con cuantos datos se va quedar el análisis
nrow(data)
#convertir el campo fecha, en formato fecha
data$InvoiceDateFF<-as.Date(substr(as.character(data$InvoiceDate),1,10))
#cual es el rango de fechas?
range(data$InvoiceDateFF)
#Se elimina la columna que no se necesita
data$InvoiceDate <-NULL
#Se hace otro respaldo
dataold2<- data
#se hace el filtro de todos los registros que sean mayores a la fecha 2010-12-09
data <- data[which(data$InvoiceDateFF >= as.Date("2010-12-09","%Y-%m-%d")),]
# cuantos registros quedan?
nrow(data)
#Se  respalda la data, antes de reducirla mas
dataold3 <- data
# se saca un segmentos de los cuales ya previamente se conoce
data <- subset(data, Country == "United Kingdom")
#cuantos registros quedan?
nrow(data)
#preparar las medidas o indicadores
#se crea el dataset customer que se irá alimentando
customers <- as.data.frame(unique(data$CustomerID))
#Se cambia el nombre de la columna
names(customers) <- "CustomerID"
# Se calcula la cantidad de días desde la fecha de la transacción y el día de hoy
data$recency <- as.Date("2011-12-10") - data$InvoiceDateFF
#Se toma las transacciones tipo c, y los guardo en una sola parte
temp <- subset(data, purchase.invoice == 1)
# se crea una tabla aparte y se calcula el tiempo mínimo de compra por ID. 
recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)
#se remueve el objeto temp
remove(temp)
# se relaciona el ID de los clientes con el tiempo más próoxima de la última compra
customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE)