library(igraph)
library(dplyr)

datos <- read.csv("C:/Users/aleja/PycharmProjects/Metro/M4_Tramos.csv", quote="")
datos$TIEMPOTRAMOANTERIOR <- datos$LONGITUDTRAMOANTERIOR / datos$VELOCIDADTRAMOANTERIOR
Nodos <- unique(datos$CODIGOESTACION)
Estaciones <- data.frame(Nodos)
Estaciones$Nombre <- sapply(Estaciones$Nodos, function(x) {
  unique(datos$DENOMINACION[datos$CODIGOESTACION == x])
})
remplazo <- list(
  "6"  = c("6-1", "6-2"),
  "9"  = c("9A", "9B"),
  "12" = c("12-1", "12-2"),
  "7"  = c("7a", "7b"),
  "10" = c("10a", "10b")
)

Estaciones$Linea <- sapply(Estaciones$Nodos, function(x) {
  a <- unique(datos$NUMEROLINEAUSUARIO[datos$CODIGOESTACION == x])
  
  clave <- names(remplazo)[
    sapply(remplazo, function(v) setequal(a, v))
  ]
  
  if (length(clave) == 1) clave else a
})

cabezaTermino <- data.frame(LINEA=unique(datos$NUMEROLINEAUSUARIO))
cabezaTermino$ORIGEN <- sapply(cabezaTermino$LINEA, function(x) {
  c(datos$DENOMINACION[datos$NUMEROLINEAUSUARIO == x & datos$TIPOPARADA=='C'],
    datos$DENOMINACION[datos$NUMEROLINEAUSUARIO == x & datos$TIPOPARADA=='T'])[1]
})
cabezaTermino$DESTINO <- sapply(cabezaTermino$LINEA, function(x) {
  datos$DENOMINACION[datos$NUMEROLINEAUSUARIO == x & datos$TIPOPARADA=='T']
})
cabezaTermino$CODORIGEN <- sapply(cabezaTermino$LINEA, function(x) {
  c(datos$CODIGOESTACION[datos$NUMEROLINEAUSUARIO == x & datos$TIPOPARADA=='C'],
    datos$CODIGOESTACION[datos$NUMEROLINEAUSUARIO == x & datos$TIPOPARADA=='T'])[1]
})
cabezaTermino$CODDESTINO <- sapply(cabezaTermino$LINEA, function(x) {
  datos$CODIGOESTACION[datos$NUMEROLINEAUSUARIO == x & datos$TIPOPARADA=='T']
})

colores <- c(  # CAMBIO: usa un vector nombrado en lugar de lista
  "1"   ="#38A8E2",
  "2"   ="#DB0B15",
  "3"   ="#FACB4B",
  "4"   ="#A46645",
  "5"   ="#80B91F",
  "6-1" ="#A29C99",
  "7a"  ="#F6910A",
  "7b"  ="#F6910A",
  "8"   ="#E96198",
  "9A"  ="#932C7C",
  "9B"  ="#932C7C",
  "10a" ="#0A6EB7",
  "10b" ="#0A6EB7",
  "11"  ="#135C2B",
  "12-1"="#AEA51D",
  "R"   ="#156CB2"
)

lineas <- list()
for (x in cabezaTermino$LINEA) {
  nombre_linea <- as.character(x)
  if (!(nombre_linea %in% c("12-2","6-2"))){
    lineas[[nombre_linea]] <- datos %>%
      filter(NUMEROLINEAUSUARIO == nombre_linea, SENTIDO == "1") %>%
      select(LONGITUDTRAMOANTERIOR, NUMEROORDEN, CODIGOESTACION, DENOMINACION) %>% 
      add_row(
        LONGITUDTRAMOANTERIOR = 0, 
        NUMEROORDEN = 1, 
        DENOMINACION = cabezaTermino[cabezaTermino$LINEA == x, "ORIGEN"][[1]],
        CODIGOESTACION = cabezaTermino[cabezaTermino$LINEA == x, "CODORIGEN"][[1]]
      ) %>%
      arrange(NUMEROORDEN)
  }
}

grafo <- graph.empty(directed = FALSE)

# CAMBIO: asigna el color de forma más directa
grafo <- add_vertices(grafo, n = nrow(Estaciones), 
                      name = Estaciones$Nodos,
                      label = Estaciones$Nombre, 
                      linea = I(Estaciones$Linea))

# Asigna colores después, de forma más simple
V(grafo)$color <- sapply(Estaciones$Linea, function(x) {
  col <- colores[as.character(x)]
  if (is.na(col)) "gray" else col
})

for (num_linea in names(lineas)) {
  linea <- lineas[[num_linea]]
  cat("Procesando línea", linea$DENOMINACION[1], "con", nrow(linea), "estaciones\n")
  for (i in 1:(nrow(linea) - 1)) {
    origen <- linea$CODIGOESTACION[i]
    destino <- linea$CODIGOESTACION[i + 1]
    peso <- linea$LONGITUDTRAMOANTERIOR[i + 1]
    grafo <- add_edges(grafo, as.character(c(origen, destino)), 
                       weight = peso, 
                       tipo = "trayecto", 
                       linea = num_linea, 
                       color = colores[num_linea])
  }
}

comp <- components(grafo)
cat("Número de componentes:", comp$no, "\n")
print(comp$csize)

for (i in 1:comp$no) {
  nodos_componente <- V(grafo)[comp$membership == i]
  subgrafo <- induced_subgraph(grafo, nodos_componente)
  plot(subgrafo, 
       vertex.label = V(subgrafo)$label, 
       vertex.size = 7,
       vertex.label.cex = 0.7,
       main = "Visualización de la Componente Conexa")
}

for (nombre in unique(Estaciones$Nombre)) {
  nodos_mismo_nombre <- V(grafo)[V(grafo)$label == nombre]
  if (length(nodos_mismo_nombre) > 1) {
    for (i in 1:(length(nodos_mismo_nombre) - 1)) {
      for (j in (i + 1):length(nodos_mismo_nombre)) {
        grafo <- add_edges(grafo, 
                           c(as.character(nodos_mismo_nombre[i]$name), 
                             as.character(nodos_mismo_nombre[j]$name)), 
                           weight = 0.001, 
                           tipo = "transbordo", 
                           color = "black")
      }
    }
  }
}

pesos_layout <- ifelse(E(grafo)$tipo == "transbordo", 
                       1,  # transbordo: distancia mínima
                       E(grafo)$weight / 100)  # distancia real escalada

layout_kk <- layout_with_kk(grafo, weights = pesos_layout)

tkplot(grafo, 
       vertex.label = V(grafo)$label, 
       edge.color = E(grafo)$color,
       edge.width = 2,
       vertex.size = 10,
       vertex.label.cex = 0.8,
       layout = layout_kk)
# Unimos entre sí todos los nodos con mismo misma label con una arista especial de la categoría transbordo con un peso de 0
for (nombre in unique(Estaciones$Nombre)) {
  nodos_mismo_nombre <- V(grafo)[V(grafo)$label == nombre]
  if (length(nodos_mismo_nombre) > 1) {
    for (i in 1:(length(nodos_mismo_nombre) - 1)) {
      for (j in (i + 1):length(nodos_mismo_nombre)) {
        grafo <- add_edges(grafo, c(as.character(nodos_mismo_nombre[i]$name), as.character(nodos_mismo_nombre[j]$name)), 
                           weight = 0.001, 
                           tipo = "transbordo", 
                           color = "black")
      }
    }
  }
}

tkplot(grafo, 
     vertex.label = V(grafo)$label, 
     edge.color = E(grafo)$color,
     edge.width = 5,
     main = "Grafo del Metro de Madrid con Líneas de Colores")


ruta <- shortest_paths(
  grafo, 
  from = as.character("305"), 
  to = as.character("46"),
  weights = E(grafo)$weight,
  output = "both" # Nos devuelve tanto los nodos como las aristas
)

# Ver los nombres de las estaciones del camino
nombres_ruta <- V(grafo)[ruta$vpath[[1]]]$name
datos$DENOMINACION[match(nombres_ruta, datos$CODIGOESTACION)]


# Diámetro considerando la longitud de los tramos (en km o metros)
distancia_maxima <- diameter(grafo, directed = FALSE, weights = E(grafo)$weight)

# Diámetro considerando solo el número de estaciones (saltos)
saltos_maximos <- diameter(grafo, directed = FALSE, weights = NA)

cat("El trayecto más largo de la red mide:", distancia_maxima, "unidades de peso.\n")
cat("El trayecto con más estaciones intermedias tiene:", saltos_maximos, "paradas.\n")


# Obtenemos los nodos que forman esa ruta crítica
nodos_diametro <- get_diameter(grafo, directed = FALSE, weights = E(grafo)$weight)

# Ver los nombres de las estaciones (extremos y paradas intermedias) ordenados
nombres_diametro <- V(grafo)[nodos_diametro]$name
nom<-datos$DENOMINACION[match(nombres_diametro, datos$CODIGOESTACION)]
nom
length(unique(nom))

# Ahora hacemos un nuevo grafo en el que los nodos unidos por aristas de transbordo, sean un único nodo
grafo_simplificado <- contract.vertices(
  grafo, 
  mapping = as.numeric(factor(V(grafo)$label)), 
  vertex.attr.comb = list(
    name = function(x) x[1],
    label = function(x) x[1],
    linea = function(x) paste(unique(unlist(x)), collapse = ",")
  )
)
# Eliminamos las aristas múltiples resultantes del proceso de contracción
grafo_simplificado <- simplify(grafo_simplificado, 
                             remove.multiple = TRUE, 
                             edge.attr.comb = list(
                               weight = "min",
                               tipo = function(x) paste(unique(unlist(x)), collapse = ","),
                               linea = function(x) paste(unique(unlist(x)), collapse = ",")
                             ))

# Calculamos la ruta más corta en el grafo simplificado
ruta_simplificada <- shortest_paths(
  grafo_simplificado, 
  from = as.character("111"), 
  to = as.character("66"),
  weights = E(grafo_simplificado)$weight,
  output = "both" # Nos devuelve tanto los nodos como las aristas
)
# Ver los nombres de las estaciones del camino
nombres_ruta_simplificada <- V(grafo_simplificado)[ruta_simplificada$vpath[[1]]]$name
datos$DENOMINACION[match(nombres_ruta_simplificada, datos$CODIGOESTACION)]

# Hacemos lo del diametro pero en el grafo simplificado
distancia_maxima_simplificada <- diameter(grafo_simplificado, directed = FALSE, weights = E(grafo_simplificado)$weight)
saltos_maximos_simplificada <- diameter(grafo_simplificado, directed = FALSE, weights = NA)
cat("En el grafo simplificado, el trayecto más largo de la red mide:", distancia_maxima_simplificada, "unidades de peso.\n")
cat("En el grafo simplificado, el trayecto con más estaciones intermedias tiene:", saltos_maximos_simplificada, "paradas.\n")   
nodos_diametro_simplificado <- get_diameter(grafo_simplificado, directed = FALSE, weights = E(grafo_simplificado)$weight)
nombres_diametro_simplificado <- V(grafo_simplificado)[nodos_diametro_simplificado]$name
nom_simplificado<-datos$DENOMINACION[match(nombres_diametro_simplificado, datos$CODIGOESTACION)]
length(unique(nom_simplificado))  



# ==========================================================
# GRAFO DE TRANSBORDOS CON ESTACIÓN DE CAMBIO EN LAS ARISTAS
# ==========================================================

# 0. Guardamos en cada arista de transbordo la estación donde ocurre
E(grafo)$estacion_transbordo <- NA

idx_trans <- which(E(grafo)$tipo == "transbordo")

E(grafo)$estacion_transbordo[idx_trans] <-
  V(grafo)[ends(grafo, E(grafo)[idx_trans])[,1]]$label

# 1. Eliminamos las aristas de transbordo para detectar bloques sin cambio
grafo_sin_transbordos <- delete_edges(
  grafo,
  E(grafo)[E(grafo)$tipo == "transbordo"]
)

# 2. Componentes conexas (bloques sin transbordo)
comp <- components(grafo_sin_transbordos)

# 3. Contraemos los vértices según esos bloques
grafo_transbordo <- contract.vertices(
  grafo,
  mapping = comp$membership,
  vertex.attr.comb = list(
    name  = function(x) x[1],
    label = function(x) x[1],
    linea = function(x) paste(unique(unlist(x)), collapse = ",")
  )
)

# 4. Simplificamos el grafo, conservando la información del transbordo
grafo_transbordo <- simplify(
  grafo_transbordo,
  remove.multiple = TRUE,
  remove.loops = TRUE,
  edge.attr.comb = list(
    weight = "min",
    tipo   = function(x) paste(unique(unlist(x)), collapse = ","),
    linea  = function(x) paste(unique(unlist(x)), collapse = ","),
    estacion_transbordo = function(x)
      paste(unique(na.omit(unlist(x))), collapse = ",")
  )
)

plot(grafo_transbordo,
     edge.label = E(grafo_transbordo)$estacion_transbordo,
     vertex.size = 7,
     edge.label.cex = 0.7,
     vertex.label.cex = 0.7,
     main = "Grafo de Transbordos del Metro de Madrid")

# 5. Diámetro en términos de transbordos (número de cambios)
diametro_transbordos <- diameter(
  grafo_transbordo,
  directed = FALSE,
  weights = NA
)

cat("Máximo número de transbordos en la red:", diametro_transbordos, "\n")

# Aristas del diámetro en términos de transbordos
aristas_diametro_transbordos <- get_diameter(
  grafo_transbordo,
  directed = FALSE,
  weights = NA
)
nombres_diametro_transbordos <- V(grafo_transbordo)[aristas_diametro_transbordos]$name
nom_transbordos <- datos$DENOMINACION[match(nombres_diametro_transbordos, datos$CODIGOESTACION)]

#Mostramos el subgrafo del camino
subgrafo_diametro_transbordos <- induced_subgraph(
  grafo_transbordo,
  aristas_diametro_transbordos
)
plot(subgrafo_diametro_transbordos,
     edge.label = E(subgrafo_diametro_transbordos)$estacion_transbordo,
     vertex.size = 7,
     edge.label.cex = 0.7,
     vertex.label.cex = 0.7,
     main = "Subgrafo del Camino de Diámetro en Términos de Transbordos")



     A <- matrix(c(
  0, 8, 3, 0, 0, 13, 0, 0, 0,
  0, 0, 2, 1, 0, 0, 0, 0, 0,
  0, 3, 0, 9, 2, 0, 0, 0, 0,
  0, 0, 0, 0, 4, 0, 2, 0, 6,
  5, 0, 0, 6, 0, 5, 0, 4, 0,
  0, 0, 0, 0, 0, 0, 0, 7, 1,
  0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 5,
  0, 0, 0, 0, 3, 0, 4, 0, 0
), nrow = 9, byrow = TRUE)

N <- nrow(A)
# Creamos el objeto de grafo para funciones automáticas
g <- graph_from_adjacency_matrix(A, mode = "directed", weighted = TRUE)

# Grado de entrada y salida (Weighted = Strength)
k_in <- strength(g, mode = "in")
k_out <- strength(g, mode = "out")

# 1. Grado simple (total)
c_deg <- k_in + k_out

# 2. Grado normalizado
c_deg_norm <- c_deg / (N - 1)

# 3. Media entre arcos adyacentes
c_deg_avg <- (k_in + k_out) / 2

################################################################################

# Matriz de distancias (usando pesos)
# Nota: igraph interpreta pesos como 'coste', a mayor peso más distancia.
dist_matrix <- distances(g, mode = "out")

# 1. Centralidad de cercanía clásica (Clos)
# c_clos = 1 / suma de distancias
c_clos <- 1 / rowSums(dist_matrix)

# 2. Centralidad de cercanía armónica (Clos2)
# Útil para grafos no conexos (evita el problema de distancias infinitas)
c_clos2 <- (1 / (N - 1)) * rowSums(1 / dist_matrix)
c_clos2[is.infinite(c_clos2)] <- 0 # Limpieza de valores

################################################################################

# Centralidad de intermediación normalizada
# La fórmula: sum(sigma_sd(i)/sigma_sd) / ((N-1)*(N-2))
c_betw <- betweenness(g, directed = TRUE, normalized = TRUE)

################################################################################

# 1. Preparación de matrices
A_bin <- (A > 0) * 1
I <- diag(N)
ones <- rep(1, N)

# 2. Cálculo del Autovalor Máximo (Fix para el error de 'complex')
# Extraemos la parte real de los autovalores antes de usar max()
autovalores <- eigen(A_bin)$values
lambda_max <- max(Re(autovalores)) 

# 3. Centralidad de Katz
# Aseguramos que alpha < 1/lambda_max
alpha_katz <- 1 / (lambda_max + 1)
c_katz <- (solve(I - alpha_katz * t(A_bin)) - I) %*% ones

# 4. Centralidad Espectral (Autovector)
ev <- eigen(t(A))
# Buscamos el índice del autovalor cuya parte real es el máximo
idx <- which.max(Re(ev$values))
c_eig <- abs(Re(ev$vectors[, idx]))
# 3. PageRank
# Usando la fórmula estocástica: alpha * P^T * C + (1-alpha)/N
alpha_pr <- 0.85
# Matriz de transición P = D^-1 * A
D_inv <- diag(1 / ifelse(k_out == 0, 1, k_out))
P <- D_inv %*% A
# Resolución directa: C = (1-alpha)/N * (I - alpha*P^T)^-1 * 1
c_pagerank <- ((1 - alpha_pr) / N) * solve(I - alpha_pr * t(P)) %*% ones

################################################################################

resultados <- data.frame(
  Nodo = 1:9,
  Grado_Avg = round(c_deg_avg, 2),
  Cercania = round(c_clos, 3),
  Intermediacion = round(c_betw, 3),
  Autovector = round(as.vector(c_eig), 3),
  Katz = round(as.vector(c_katz), 3),
  PageRank = round(as.vector(c_pagerank), 3)
)

print(resultados)


g <- make_ring(10)
g$name <- "Ring"
V(g)$name <- c("a","a","c","d","e","a","b","c","d","e")
E(g)$weight <- runif(ecount(g))

g2 <- contract(g, factor(c("a","a","c","d","e","a","b","c","d","e")),
               vertex.attr.comb=function(x) if (length(unique(x))!=1) stop("Error contrayendo el grafo. Dos valores para un mismo nodo: ",x)  else x[1])

## graph and edge attributes are kept, vertex attributes are
## combined using the 'toString' function.
print(g2, g=TRUE, v=TRUE, e=TRUE)
plot(g2, vertex.label=V(g2)$name, edge.label=round(E(g2)$weight,2))
plot(simplify(g2), vertex.label=V(g2)$name, edge.label=round(E(g2)$weight,2))

