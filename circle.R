is.circle = function(circle_x, circle_y, r, x, y){

  
  if ( r != 1 )
  {stop ("El radio por defecto debe ser 1.")}
  if (circle_x != 0)
  {stop("El centro debe ser el origen de las cordenadas")}
  if (circle_y != 0)
  {stop("El centro debe ser el origen de las cordenadas")}


  
  d = ((circle_x - x)^2 + (circle_y - y)^2)
  if (d < r^2){
    "True"
  }
  
  else{
    "False"
  }

}