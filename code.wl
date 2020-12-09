(* ::Package:: *)

"Resolveremos la ED. Lineal no homogenea de 2\[Degree] orden por el método de variación de parametros
Forma: y''+f(x)y'+g(x)y=r(x). Donde f(x) y g(x) son constantes.
Ecuación ejemplo de este script: y''-4y'+3y=x*exp(x)
"

"\n Declarando funciones y soluciones"
Clear[y1, y2, yg, yh, yp, yp1, yp2, u, v]

"Ecuación Diferencial Homógenea -- inicio
"
"Ecuación Diferencial: y''-4y+3y=0 resolución
"
"Raíces de la ED en su estado a*m^2+b*m+c=0 -> " {{m->-1}{m->-3}}
"Resolver internamente la equación ->" Solve[m^2-4m+3==0]

"\n r(x)="
f[x]=x*Exp[x]

"\n valores de yn"
y1[x_]=Exp[x];
y2[x_]=Exp[3x];

"ED Homogenea"

yh[x]=c[1]y1[x]+c[2]y2[x]

"Ecuación Diferencial Homógenea -- fin"

"Wronskiano"
"W(y1,y2)-> "
wronskian=Det[{{y1[x],y2[x]},{y1'[x], y2'[x]}}]

"u"
u = -{y2[x]*f[x]}/wronskian
u=Integrate[u,x]

"v"
v={y1[x]*f[x]}/wronskian
v=Integrate[v,x]

"Solución particular"
yp1=Simplify[u * y1[x]]
yp2=Simplify[v * y2[x]]
yp=yp1+yp2

"Solución general"
yg=yh[x]+yp
