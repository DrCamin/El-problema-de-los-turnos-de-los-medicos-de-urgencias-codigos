$TITLE TFM
SETS I Día /1*28/
J Turno /1*3/
N Médicos disponibles /1*30/
;

ALIAS (i ii);

TABLE D(j,i) demanda de médicos por día y turno
        1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28
1       9   9   9   9   9   3   3   9   9   9   9   9   3   3   9   9   9   9   9   3   3   9   9   9   9   9   3   3
2       5   5   5   5   5   3   3   5   5   5   5   5   3   3   5   5   5   5   5   3   3   5   5   5   5   5   3   3
3       3   3   3   3   2   2   2   3   3   3   3   2   2   2   3   3   3   3   2   2   2   3   3   3   3   2   2   2
;

TABLE P(j,i) Penalizaciones asociadas a cubrir cada turno
        1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28
1       1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
2       1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
3       1.5 1.5 1.5 1.5 2   2   2   1.5 1.5 1.5 1.5 2   2   2   1.5 1.5 1.5 1.5 2   2   2   1.5 1.5 1.5 1.5 2   2   2
;

VARIABLES
X(i,j,n) Binaria trabajo de cada dia en cada turno de cada medico
MMin
MMax
TMin
TMax
NMin
NMax
DFMin
DFMax
NFMin
NFMax
P_opt funcion objetivo

BINARY VARIABLES X;
POSITIVE VARIABLES MMin, MMax, TMin, TMax, NMin, NMax, DFMin, DFMax, NFMin, NFMax;

EQUATIONS
fobj función objetivo
MMini
MMaxi
TMini
TMaxi
NMini
NMaxi
DFMini
DFMaxi
NFMini
NFMaxi
DemInf cota inferior de médicos trabajando
DemSup cota superior de médicos trabajando
RT1 restriccion de turnos 1
RT2 restriccion de turnos 2
RT3 restriccion de turnos 3
RT4 restriccion de turnos 4
RT5_1 restriccion de turnos 5.1
RT5_2 restriccion de turnos 5.2
RT5_3 restriccion de turnos 5.3
RT5_4 restriccion de turnos 5.4
RT5_5 restriccion de turnos 5.5
RT6 restriccion de turnos 6
RT7 restriccion de turnos 7
RT8_1 restriccion de turnos 8
RT8_2
RT9
;
fobj .. P_opt =E= (MMax-MMin)+ 1.2*(TMax-TMin) + 1.5*(NMax-NMin) + 2*(DFMax-DFMin) + 2*(NFMax-NFMin);
MMaxi(n) .. MMax =G= SUM(i$(MOD(ORD(i)-1,7) le 4), X(i,'1',n));
MMini(n) .. MMin =L= SUM(i$(MOD(ORD(i)-1,7) le 4), X(i,'1',n));
TMaxi(n) .. TMax =G= SUM(i$(MOD(ORD(i)-1,7) le 4), X(i,'2',n));
TMini(n) .. TMin =L= SUM(i$(MOD(ORD(i)-1,7) le 4), X(i,'2',n));
NMaxi(n) .. NMax =G= SUM(i$(MOD(ORD(i)-1,7) le 3), X(i,'3',n));
NMini(n) .. NMin =L= SUM(i$(MOD(ORD(i)-1,7) le 3), X(i,'3',n));
DFMaxi(n) .. DFMax =G= SUM(i$(MOD(ORD(i)-1,7) ge 5), X(i,'1',n));
DFMini(n) .. DFMin =L= SUM(i$(MOD(ORD(i)-1,7) ge 5), X(i,'1',n));
NFMaxi(n) .. NFMax =G= SUM(i$(MOD(ORD(i)-1,7) ge 4), X(i,'3',n));
NFMini(n) .. NFMin =L= SUM(i$(MOD(ORD(i)-1,7) ge 4), X(i,'3',n));
DemInf(i,j) .. D(j,i) =L= SUM(n, X(i,j,n));
*                             ARREGLAR DEMANDAS
DemSup(i,j) .. D(j,i) + 1 =G= SUM(n, X(i,j,n));
RT1(i,n)$(mod(ORD(i)-1,7) le 4) .. SUM(j, X(i,j,n)) =L= 1;
RT2(i,n) .. SUM(j, X(i+1,j,n)) =L= 3-3*X(i,'3',n);
RT3(i,n)$(mod(ORD(i)-1,7) ge 5) .. X(i,'1',n) =E= X(i,'2',n);
RT4(i,n)$(mod(ORD(i),7) eq 6) .. X(i-1,'3',n) + X(i,'1',n) + X(i,'3',n) + X(i+1,'1',n) + X(i+1,'3',n) =L= 1;
RT5_1(i,n)$(mod(ORD(i),7) eq 5) .. SUM(j, X(i+5,j,n)) + X(i+4,'3',n) =L= 1-X(i,'3',n);
RT5_2(i,n)$(mod(ORD(i),7) eq 6) .. SUM(j, X(i+3,j,n)) + X(i+2,'3',n) =L= 1-X(i,'1',n);
RT5_3(i,n)$(mod(ORD(i),7) eq 6) .. SUM(j, X(i+2,j,n)) + X(i+1,'3',n) =L= 1-X(i,'3',n);
RT5_4(i,n)$(mod(ORD(i),7) eq 0) .. X(i+1,'1',n)+SUM(j, X(i+3,j,n))+X(i+2,'3',n) =L= 2-2*X(i,'1',n);
RT5_5(i,n)$(mod(ORD(i),7) eq 0) .. SUM(j, X(i+2,j,n)) =L= 1-X(i,'3',n);
RT6(i,n)$(mod(ORD(i)+1,7) gt 1) .. X(i+1,'1',n) =L= 1-X(i,'2',n);
RT7(i,n)$(mod(ORD(i), 7) eq 1) .. X(i,'2',n) + X(i+1,'2',n) + X(i+2,'2',n) + X(i+3,'2',n) + X(i+4,'2',n) =L= 3;
RT8_1(i,n)$(mod(ORD(i),7) eq 6) .. 5*(1-X(i,'1',n)-X(i,'3',n)) =G= X(i-1,'3',n) + X(i-2,'3',n) + X(i-3,'3',n) + X(i-4,'3',n) + X(i-5,'3',n);
RT8_2(i,n)$(mod(ORD(i),7) eq 0) .. 5*(1-X(i,'1',n)-X(i,'3',n)) =G= X(i-2,'3',n) + X(i-3,'3',n) + X(i-4,'3',n) + X(i-5,'3',n) + X(i-6,'3',n);
RT9(i,n)$(mod(ORD(i),7) eq 5) .. 5 - SUM(ii$(ORD(ii) ge ORD(i)-4 and ORD(ii) le ORD(i)), SUM(j$(ORD(j) ne 3), X(ii,j,n))) =G= X(i,'3',n) + X(i+1,'1',n) + X(i+1,'3',n) + X(i+2,'1',n) + X(i+2,'3',n);

model firstapproach /all/;
solve firstapproach minimizing P_opt using MIP;