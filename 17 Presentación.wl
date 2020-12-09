(* ::Package:: *)

"Resolveremos la ED. Lineal no homogenea de 2\[Degree] orden por el m\[EAcute]todo de variaci\[OAcute]n de parametros
Forma: y''+f(x)y'+g(x)y=r(x). Donde f(x) y g(x) son constantes.
Ecuaci\[OAcute]n ejemplo de este script: y''-4y'+3y=x*exp(x)
"

"\n Declarando funciones y soluciones"
Clear[y1, y2, yg, yh, yp, yp1, yp2, u, v]

"Ecuaci\[OAcute]n Diferencial Hom\[OAcute]genea -- inicio
"
"Ecuaci\[OAcute]n Diferencial: y''-4y+3y=0 resoluci\[OAcute]n
"
"Ra\[IAcute]ces de la ED en su estado a*m^2+b*m+c=0 -> " {{m->-1}{m->-3}}
"Resolver internamente la equaci\[OAcute]n ->" Solve[m^2-4m+3==0]

"\n r(x)="
f[x]=x*Exp[x]

"\n valores de yn"
y1[x_]=Exp[x];
y2[x_]=Exp[3x];

"ED Homogenea"

yh[x]=c[1]y1[x]+c[2]y2[x]

"Ecuaci\[OAcute]n Diferencial Hom\[OAcute]genea -- fin"

"Wronskiano"
"W(y1,y2)="
wronskian=Det[{{y1[x],y2[x]},{y1'[x], y2'[x]}}]

"u"
u = -{y2[x]*f[x]}/wronskian
u=Integrate[u,x]

"v"
v={y1[x]*f[x]}/wronskian
v=Integrate[v,x]

"Soluci\[OAcute]n particular"
yp1=u[x]y1[x]
yp1=FullSimplify[yp1]
yp2=v[x]y2[x]
yp2=FullSimplify[yp1]
yp[x]=yp1[x]+yp2[x]


"Soluci\[OAcute]n general"
yg[x]=yh[x]+yp[x]






