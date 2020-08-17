#função para calcular a quilometragem dentro da area escolhida da viagem

distancia_viagem_centro <- function(origem, destino, area){
  require(osrm)
  require(rgdal)
  require(raster)
  
  rota <- osrmRoute(src = origem,
                    dst = destino,
                    overview = 'full',
                    returnclass = "sp" )
  
  rota2 <- intersect(rota, area)
  
  if(is.null(rota2)==T){return(c(SpatialLinesLengths(rota), 0))}
  
  return(c(SpatialLinesLengths(rota), SpatialLinesLengths(rota2)))

  
}