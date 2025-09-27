program oscilador  
       
 implicit none 

!declaramos variables enteras para las iteraciones y reales para las cantidades físicas
!podemos editar estos valores dependiendo de las necesidades de la simulación

 integer :: i, n  

 real :: m, b, k ,a, x, v, h, t, Edis, v_prev 

 m = 0.5  
 k = 4.0  
 b = 0.5 
 h = 0.05  
 x = 1.0  
 v = 0.0  
 n = 301  
 Edis = 0.0  
 v_prev = v

!guardamos el valor de la energía previa para la cuadratura
!guardamos los valores obtenidos en un archivo de texto

 open(200,file="resultados.dat") 

!inicializamos ciclo de cálculo
    do i = 1, n  
       a = - (b/m) * v - (k/m) * x  
       x = x + v * h + 0.5 * a * h**2  
       v = v + a * h  
       t = real(i)*h 

!regla trapezoidal para la inegral  
       if (i > 1) then 
         Edis = Edis + 0.5 * b * (v_prev**2 + v**2) * h 
       end if 

!actualizamos el valor de la velocidad

    v_prev = v 
 
!escribimos los reultados en el archivo de texto
       write(200,*) t, x, v, Edis 
                          
 
     end do 

 close(200) 

end program oscilador 

                         
