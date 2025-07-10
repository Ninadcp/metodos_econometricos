/*******************************************************************************
                          Universidad de San Andrés	         
             Métodos Econométricos y Organización Industrial Aplicada
							  Trabajo Práctico 2
								 Julio 2024
		   Luca Bentivenga, Nina Di Costanzo Pereira y María Luján Puchot
********************************************************************************/

clear all
set more off

import excel using "TP_2_data.xlsx", firstrow  clear

label variable precio "Precio"
label variable descuento "Descuento"

*Dado el elevado número de parámetros a estimar en algunas especificaciones, recomendamos tener instalado reghdfe y su versión IV, ivreghdfe, para acelerar este proceso.

/*Install ftools (remove program if it existed previously)
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")

*Install reghdfe
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")

*Install ivreg2, the core package
cap ado uninstall ivreg2
ssc install ivreg2

*Finally, install this package
cap ado uninstall ivreghdfe
net install ivreghdfe, from(https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/)*/

********************************************************************************
*  LOGIT
********************************************************************************

*Contruimos los market share:

order semana tienda marca
sort semana tienda marca

*Total de ventas por semana:

bysort semana: egen suma_ventas = total(ventas)

/*Sin embargo, estas ventas representan solo el 64% del mercado total, ya que el resto corresponderían al outside good. Luego, calculamos las ventas totales por semana a lo largo de todas las tiendas.*/

gen ventas_totales = suma_ventas / 0.64

*Calculamos el share a nivel de semana-tienda-marca:

gen share_marca = ventas / ventas_totales
gen lshare_marca = log(share_marca)
gen lshare_outside_good = log(0.36)

label variable share_marca "Share de la marca a nivel semana-tienda"
label variable lshare_marca "Logaritmo del share de la marca a nivel semana-tienda"
label variable lshare_outside_good "Logaritmo del share del outside good"

/*Para este último cálculo estamos suponiendo implícitamente que el share de mercado del outside good se mantiene constante a lo largo de las distintas semanas y tiendas.*/

*Por último, calculamos delta_jt = log(sjt) - log(s0)

gen delta_jt = lshare_marca - lshare_outside_good
label variable delta_jt "Utilidad media"

drop suma_venta ventas_totales

*Estimaciones
*==============================================================================*

*ssc install reghdfe

eststo clear

*1.
eststo: qui reghdfe delta_jt precio descuento, vce(robust)
estadd local marcafe "No"
estadd local marcaxtiendafe "No"
estadd local inst "--"
estadd local hausmanins "--"

scalar alpha_1 = _b[precio]

*2.
eststo: qui reghdfe delta_jt precio descuento, absorb(marca) vce(robust)
estadd local marcafe "Sí"
estadd local marcaxtiendafe "No"
estadd local inst "--"
estadd local hausmanins "--"

scalar alpha_2 = _b[precio]

*3.
eststo: qui reghdfe delta_jt precio descuento, absorb(i.marca#i.tienda) vce(robust)
estadd local marcafe "No"
estadd local marcaxtiendafe "Sí"
estadd local inst "--"
estadd local hausmanins "--"

scalar alpha_3 = _b[precio]

*4.
eststo: qui ivreghdfe delta_jt (precio = costo) descuento, vce(robust)
estadd local marcafe "No"
estadd local marcaxtiendafe "No"
estadd local inst "Sí"
estadd local hausmanins "No"

eststo: qui ivreghdfe delta_jt (precio = costo) descuento, vce(robust) absorb(marca)
estadd local marcafe "Sí"
estadd local marcaxtiendafe "No"
estadd local inst "Sí"
estadd local hausmanins "No"

eststo: qui ivreghdfe delta_jt (precio = costo) descuento, vce(robust) ///
absorb(i.marca#i.tienda)
estadd local marcafe "No"
estadd local marcaxtiendafe "Sí"
estadd local inst "Sí"
estadd local hausmanins "No"

*5.

*Primero necesitamos crear el instrumento de Hausman (precio promedio de la marcas en otras tiendas en una misma semana)

*Variable auxiliar
gen precio_original = precio

*Calcular promedio de precio por marca-semana
egen precio_marca_semana_promedio = mean(precio), by(marca semana)

*Calcular cantidad de observaciones por marca-semana
egen n_obs = count(precio), by(marca semana)

*Calcular Hausman IV excluyendo tienda actual:
gen hausman_iv = (precio_marca_semana_promedio * n_obs - precio_original) / (n_obs - 1)

drop precio_original precio_marca_semana_promedio n_obs

*Luego, hacemos las estimaciones

eststo: qui ivreghdfe delta_jt (precio = hausman_iv) descuento, vce(robust)
estadd local marcafe "No"
estadd local marcaxtiendafe "No"
estadd local inst "No"
estadd local hausmanins "Sí"

eststo: qui ivreghdfe delta_jt (precio = hausman_iv) descuento, vce(robust) ///
absorb(marca)
estadd local marcafe "Sí"
estadd local marcaxtiendafe "No"
estadd local inst "No"
estadd local hausmanins "Sí"

eststo: qui ivregress ivreghdfe delta_jt (precio = hausman_iv) descuento, ///
vce(robust) absorb(i.marca#i.tienda)
estadd local marcafe "No"
estadd local marcaxtiendafe "Sí"
estadd local inst "No"
estadd local hausmanins "Sí"

esttab using "$output/EJ_3.tex", se replace label collabels("") ///
keep(precio descuento, relax) cells(b(fmt(4) star) se(par fmt(4))) ///
s(marcafe marcaxtiendafe inst hausmanins N, fmt(0 0 0 0 0) ///
label("Dummies por marca" "Dummies por marca-tienda" "Costo" "Hausman")) ///
star(* 0.1 ** 0.05 *** 0.01)

*6.

*Elasticidades
*==============================================================================*

gen elastic1 = alpha_1 * precio * (1 - share_marca)
gen elastic2 = alpha_2 * precio * (1 - share_marca)
gen elastic3 = alpha_3 * precio * (1 - share_marca)

preserve

collapse (mean) elastic1 elastic2 elastic3, by(marca)

esttab using "elasticidades_marca.tex", booktabs nonumber nostar ///
    cells("elastic1(fmt(3)) elastic2(fmt(3)) elastic3(fmt(3))") ///
    varlabels(marca "Marca") ///
    collabels("Modelo 1" "Modelo 2" "Modelo 3") ///
    replace

restore
