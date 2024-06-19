#Renta de un apartamento considerando los siguientes atributos
# 1.- La renta sube un 0.7% cada mes
# 2.- El salario sube un 3.5% anual

calculo <- function(salarioMensual, rentaBisemanal) {
  
  salarioInicial <- salarioMensual
  rentaInicial <- rentaBisemanal
  
  rentaAnual <- 0
  
  for (i in 1:12) {
    salarioMensual <- salarioMensual * (1 + 3.5/100)
    rentaBisemanal <- rentaBisemanal * (1 + 0.7/100)
    rentaAnual <- rentaAnual + rentaBisemanal * 26
  }
  return(list(rentaAnual = rentaAnual,
              salarioInicial = salarioInicial,
              rentaInicial = rentaInicial,
              rentaBisemanal = rentaBisemanal,
              salarioMensual = salarioMensual))
}

salarioMensual <- 16500
rentaBisemanal <-4500

resultados <- calculo(salarioMensual, rentaBisemanal)

gasto_renta <- resultados$rentaAnual
salarioInicial <- resultados$salarioInicial
rentaInicial <- resultados$rentaInicial
rentaBisemanal <- resultados$rentaBisemanal
salarioMensual <- resultados$salarioMensual

cat("Al final del año, gastaste ", format(gasto_renta, big.mark = ","), " en renta\n")
cat("Al inicio del año, tu renta era de ", rentaInicial, " mensuales\n")
cat("A su vez, al inicio de año tu salario fue de ", salarioInicial, "\n")
cat("En comparacion, tu salario al final del año fue de ", format(salarioMensual, big.mark = ","), "\n")
cat("Mientras que tu renta al final de año fue de ", format(rentaBisemanal * 2, big.mark = ","), " mensuales\n")