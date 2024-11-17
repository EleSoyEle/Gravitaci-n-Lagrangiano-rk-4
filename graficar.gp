set terminal pngcairo size 800,600 enhanced font 'Verdana,12'
set polar

set output "imagen.png"
set title "Orbita"
plot 'data.txt' using 1:2 with lines

unset polar

set output "diagrama_de_fase_r.png"
set title "Diagrama de fase de r"
plot 'data.txt' using 1:3 with lines
unset polar

set output "diagrama_de_fase_theta.png"
set title "Diagrama de fase de theta"
plot 'data.txt' using 2:4 with lines