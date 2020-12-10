(* ::Package:: *)

"Resolveremos la ED. Lineal no homogenea de 2\[Degree] orden por el método de variación de parametros
Forma: y''+f(x)y'+g(x)y=r(x). Donde f(x) y g(x) son constantes.\n"

Clear[y1, y2, yg, yh, yp, yp1, yp2, u, v, m1, m2, roots]

"Ecuación Diferencial Homógenea -- INICIO\n"

"Ecuación Diferencial: y''-4y'+3y=0"
"Resolver internamente la equación en su estado a*m^2+b*m+c=0"
roots=Solve[m^2-4m+3==0,m]
{m1, m2} = roots // Values // Flatten
"\nRaíces de la ED en su estado a*m^2+b*m+c=0"
"Raíz 1"
m1 
"Raíz 2"
m2

"\nR(X)"
R[x]=x*Exp[x]

"\nValores de Yn"
y1[x_]=Exp[m1*x];
"y1 <-" y1[x]
y2[x_]=Exp[m2*x];
"y2 <-" y2[x]

"\nED Homógenea"
yh[x]=c[1]y1[x]+c[2]y2[x]

"\nEcuación Diferencial Homógenea -- FIN\n"

"Búsqueda de los valores de U y V -- INICIO\n"

"Wronskiano [W(x,y)]"
wronskian=Det[{{y1[x],y2[x]},{y1'[x], y2'[x]}}]

"\nValores de U"
"U'"
u = -{y2[x]*R[x]}/wronskian
"U:"
u=Integrate[u,x]

"\nValores de V"
"V'"
v={y1[x]*R[x]}/wronskian
"V"
v=Integrate[v,x]

"\nBúsqueda de los valores de U y V -- FIN\n"

"Solución particular"
yp=Simplify[u * y1[x]]+Simplify[v * y2[x]]

"Solución general"
yg=yh[x]+yp


