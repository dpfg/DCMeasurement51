Duty cycle measurement on Intel8051-based microcontroller
=====================================================
This is my cours project in the university on the "Digital and Microprocessor Devices".
Subject of the course project is to design a measurement duty cycle signal. 
I used an Intel8051-based microcontroller. 
Task has been done in the ISIS Proteus. Project files is attached.

How it work
-----------------------------------------------------
Timer0 is countering duration of the signal(t). Timer1 — cycle(T). 
Duty cycle S = T/t


Metrological characteristics
-----------------------------------------------------
T_max=1/(1∙〖10〗^6 )∙2^16=65535∙〖10〗^(-6) s

t_min= 1∙〖10〗^(-6)  s

f_min=1/(65535∙〖10〗^(-6) )=1,5 Hz

f_max=1/(2∙1∙〖10〗^(-6) )=0.5 MHz

δ_T=± (δ_0+T_0/T_x ),

δ_t=± (δ_0+δ_ф+T_0/t_x )

δ_s=√(〖δ_T〗^2+〖δ_t〗^2 )

