#True = dentro de la circunsferencia
#False = Fuera de la circunsferencia
#center_x = coordenanda en X
#center_y = coordenada en y
# r por defecto 1

is.circle = function(center_x, center_y, x, y, r) {

  if ( r != 1 )
  {stop ("El radio por defecto debe ser 1.")}

  d = r^2 - (center_x-x)^2 + (center_y-y)^2
  if (d < 0){
    "True"
  }
  else{
    "False"
  }
}

