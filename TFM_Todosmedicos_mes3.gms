$TITLE TFM
SETS I Día /1*28/
J Turno /1*3/
N Médicos disponibles /1*31/
S Semana /1*4/;

ALIAS (i, ii);

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
* Esta tabla no se usa en este modelo
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
RT9 restriccion de turnos 9

DOS1
DOS2
DOS3
DOS4_1
DOS4_2

TRES1_1
TRES1_2
TRES2
TRES3
TRES4_1
TRES4_2
TRES5

CUATRO1
CUATRO2
CUATRO3
CUATRO4
CUATRO5

CINCO1
CINCO2
CINCO3
CINCO4

SEIS1
SEIS2
SEIS3

SIETE1
SIETE2
SIETE3

OCHO3

NUEVE1
NUEVE2
NUEVE3
NUEVE4
NUEVE5

MESANT1
MESANT2
MESANT3
MESANT4
MESANT5
;

fobj .. P_opt =E= (MMax-MMin)+ 1.2*(TMax-TMin) + 1.5*(NMax-NMin) + 2*(DFMax-DFMin) + 2*(NFMax-NFMin);
MMaxi(n)$(ORD(n) le 20) .. MMax =G= SUM(i$(MOD(ORD(i)-1,7) le 4), X(i,'1',n));
MMini(n)$(ORD(n) le 20) .. MMin =L= SUM(i$(MOD(ORD(i)-1,7) le 4), X(i,'1',n));
TMaxi(n)$(ORD(n) le 20) .. TMax =G= SUM(i$(MOD(ORD(i)-1,7) le 4), X(i,'2',n));
TMini(n)$(ORD(n) le 20) .. TMin =L= SUM(i$(MOD(ORD(i)-1,7) le 4), X(i,'2',n));
NMaxi(n)$(ORD(n) le 20) .. NMax =G= SUM(i$(MOD(ORD(i)-1,7) le 3), X(i,'3',n));
NMini(n)$(ORD(n) le 20) .. NMin =L= SUM(i$(MOD(ORD(i)-1,7) le 3), X(i,'3',n));
DFMaxi(n)$(ORD(n) le 20) .. DFMax =G= SUM(i$(MOD(ORD(i)-1,7) ge 5), X(i,'1',n));
DFMini(n)$(ORD(n) le 20) .. DFMin =L= SUM(i$(MOD(ORD(i)-1,7) ge 5), X(i,'1',n));
NFMaxi(n)$(ORD(n) le 20) .. NFMax =G= SUM(i$(MOD(ORD(i)-1,7) ge 4), X(i,'3',n));
NFMini(n)$(ORD(n) le 20) .. NFMin =L= SUM(i$(MOD(ORD(i)-1,7) ge 4), X(i,'3',n));
DemInf(i,j) .. D(j,i) =L= SUM(n, X(i,j,n));
*                           ARREGLAR DEMANDAS
DemSup(i,j) .. D(j,i) + 1 =G= SUM(n, X(i,j,n));
RT1(i,n)$(((ORD(n) le 20) or (ORD(n) eq 29) or (ORD(n) eq 30)) and (mod(ORD(i)-1,7) le 4)) .. SUM(j, X(i,j,n)) =L= 1;
RT2(i,n)$(ORD(n) le 20 or ORD(n) eq 22) .. SUM(j, X(i+1,j,n)) =L= 3-3*X(i,'3',n);
RT3(i,n)$((ORD(n) le 26 or ORD(n) eq 31) and (mod(ORD(i)-1,7)) ge 5) .. X(i,'1',n) =E= X(i,'2',n);
RT4(i,n)$((ORD(n) le 22) and (mod(ORD(i),7) eq 6)) .. X(i-1,'3',n) + X(i,'1',n) + X(i,'3',n) + X(i+1,'1',n) + X(i+1,'3',n) =L= 1;
RT5_1(i,n)$(((ORD(n) le 20) or (ORD(n) eq 22)) and (mod(ORD(i),7) eq 5)) .. SUM(j, X(i+5,j,n)) + X(i+4,'3',n) =L= 1-X(i,'3',n);
RT5_2(i,n)$((ORD(n) le 25) and (mod(ORD(i),7) eq 6)) .. SUM(j, X(i+3,j,n)) + X(i+2,'3',n) =L= 1-X(i,'1',n);
RT5_3(i,n)$((ORD(n) le 20 or ORD(n) eq 22) and (mod(ORD(i),7) eq 6)) .. SUM(j, X(i+2,j,n)) + X(i+1,'3',n) =L= 1-X(i,'3',n);
RT5_4(i,n)$((ORD(n) le 25) and (mod(ORD(i),7) eq 0)) .. X(i+1,'1',n)+SUM(j, X(i+3,j,n))+X(i+2,'3',n) =L= 2-2*X(i,'1',n);
RT5_5(i,n)$(((ORD(n) le 20) or (ORD(n) eq 22)) and (mod(ORD(i),7) eq 0)) .. SUM(j, X(i+2,j,n)) =L= 1-X(i,'3',n);
RT6(i,n)$((ORD(n) le 20) and (mod(ORD(i)+1,7) gt 1)) .. X(i+1,'1',n) =L= 1-X(i,'2',n);
RT7(i,n)$((ORD(n) le 20) and (mod(ORD(i), 7) eq 1)) .. X(i,'2',n) + X(i+1,'2',n) + X(i+2,'2',n) + X(i+3,'2',n) + X(i+4,'2',n) =L= 3;
RT8_1(i,n)$((ORD(n) le 20 or ORD(n) eq 22) and (mod(ORD(i),7) eq 6)) .. 5*(1-X(i,'1',n)-X(i,'3',n)) =G= X(i-1,'3',n) + X(i-2,'3',n) + X(i-3,'3',n) + X(i-4,'3',n) + X(i-5,'3',n);
RT8_2(i,n)$((ORD(n) le 20 or ORD(n) eq 22) and (mod(ORD(i),7) eq 0)) .. 5*(1-X(i,'1',n)-X(i,'3',n)) =G= X(i-2,'3',n) + X(i-3,'3',n) + X(i-4,'3',n) + X(i-5,'3',n) + X(i-6,'3',n);
RT9(i,n)$((ORD(n) le 20) and (mod(ORD(i),7) eq 5)) .. 5 - SUM(ii$(ORD(ii) ge ORD(i)-4 and ORD(ii) le ORD(i)), SUM(j$(ORD(j) ne 3), X(ii,j,n))) =G= X(i,'3',n) + X(i+1,'1',n) + X(i+1,'3',n) + X(i+2,'1',n) + X(i+2,'3',n);

DOS1(i,n)$(ORD(n) eq 21 and (mod(ORD(i)-1,7) le 4)) .. X(i,'1',n) =E= 0;
DOS2(i,n)$(ORD(n) eq 21) .. X(i,'3',n) =E= 0;
DOS3(n)$(ORD(n) eq 21) .. SUM(i$(mod(ORD(i)-1,7) le 4),X(i,'2',n)) =E= 1;
DOS4_1(n)$(ORD(n) eq 21) .. SUM(i$(mod(ORD(i)-1,7) ge 5),X(i,'1',n)) =G= 1;
DOS4_2(n)$(ORD(n) eq 21) .. SUM(i$(mod(ORD(i)-1,7) ge 5),X(i,'1',n)) =L= 2;

TRES1_1(n)$(ORD(n) eq 22) .. SUM(i$(mod(ORD(i)-1,7) le 3), X(i,'3',n)) =G= 1;
TRES1_2(n)$(ORD(n) eq 22) .. SUM(i$(mod(ORD(i)-1,7) le 3), X(i,'3',n)) =L= 2;
TRES2(i,n)$((ORD(n) eq 22) and (mod(ORD(i)-1,7) le 3)) .. X(i,'2',n) =L= X(i,'3',n);
TRES3(i,n)$((ORD(n) eq 22) and (mod(ORD(i)-1,7) le 4)) .. X(i,'1',n) =E= 0;
TRES4_1(n)$(ORD(n) eq 22) .. SUM(i$(mod(ORD(i),7) eq 6), X(i-1,'3',n) + X(i,'1',n) + X(i,'3',n) + X(i+1,'1',n) + X(i+1,'3',n)) =G= 1;
TRES4_2(n)$(ORD(n) eq 22) .. SUM(i$(mod(ORD(i),7) eq 6), X(i-1,'3',n) + X(i,'1',n) + X(i,'3',n) + X(i+1,'1',n) + X(i+1,'3',n)) =L= 2;
TRES5(i,n)$(ORD(n) eq 22 and (mod(ord(i),7) eq 5)) .. X(i,'2',n) =E= 0;

CUATRO1(i,n)$(((ORD(n) ge 23) and (ORD(n) le 25)) and (mod(ORD(i)-1,7) le 4)) .. X(i, '1', n) =E= 0;
CUATRO2(i,n)$(((ORD(n) ge 23) and (ORD(n) le 25)) and (mod(ORD(i)+3,7) le 1)) .. X(i, '2', n) =E= 0;
CUATRO3(s,n)$((ORD(n) ge 23) and (ORD(n) le 25)) .. SUM(i$(ORD(i) ge 7*(ord(s)-1)+1 and ORD(i) le 7*(ORD(s)-1)+3), X(i,'2',n)) =E= 1;
CUATRO4(i,n)$((ORD(n) ge 23) and (ORD(n) le 25)) .. X(i,'3',n) =E= 0;
CUATRO5(n)$((ORD(n) ge 23) and (ORD(n) le 25)) .. SUM(i$(mod(ORD(i)-1,7) ge 5),X(i,'1',n)) =E= 1;

CINCO1(i,s,n)$((ORD(n) eq 26) and (ord(i) eq 7*(ord(s)-1)+5)) .. X(i, '3', n) =E= X(i+2, '1',n);
CINCO2(n)$(ORD(n) eq 26) .. SUM(s, SUM(i$(ord(i) eq 7*(ord(s)-1)+5), X(i, '3', n))) =E= 1;
CINCO3(i,j,n)$((ORD(n) eq 26) and (MOD(ORD(i),7) ne 0) and (ORD(j) ne 3)) .. X(i,j,n) =E= 0;
CINCO4(i,n)$((ORD(n) eq 26) and (MOD(ORD(i),7) ne 5)) .. X(i,'3',n) =E= 0;

SEIS1(i,n)$(ORD(n) eq 27 or ORD(n) eq 28) .. X(i,'2',n) =E= 0;
SEIS2(i,n)$(ORD(n) eq 27 or ORD(n) eq 28) .. X(i,'3',n) =E= 0;
SEIS3(s,n)$(ORD(n) eq 27 or ORD(n) eq 28) .. SUM(i$(ORD(i) ge 7*(ord(s)-1)+1 and ORD(i) le 7*ord(s)), X(i,'1', n)) =E= 5;

SIETE1(s,n)$(ORD(n) eq 29) .. SUM(i$(ORD(i) ge 7*(ord(s)-1)+1 and ORD(i) le 7*(ord(s)-1)+5), X(i,'1', n) + X(i, '2', n)) =E= 5;
SIETE2(i,n)$((ORD(n) eq 29) or (ord(n) eq 30)) .. X(i,'3',n) =E= 0;
SIETE3(i,j,n)$(((ORD(n) eq 29) or (ord(n) eq 30)) and (mod(ORD(i)-1,7) ge 5) and (ORD(j) ne 3)) .. X(i,j,n) =E= 0;

OCHO3(i,j,n)$(ord(n) eq 30 and (((ord(i) eq 18) and ord(j) ne 1) or (ord(i) eq 15 or ord(i) eq 19 or ORD(i) eq 22))) .. X(i,j,n) =E= 0;

NUEVE1(n)$(ORD(n) eq 31) .. SUM(i$(MOD(ord(i),7) eq 4), X(i,'2',n)) =E= 2;
NUEVE2(n)$(ORD(n) eq 31) .. SUM(i$(MOD(ORD(i)-1,7) ge 4), SUM(j, X(i,j,n))) =E= 3;
NUEVE3(i,n)$((ORD(n) eq 31) and (mod(ORD(i)-1,7) le 4)) .. X(i,'1',n) =E= 0;
NUEVE4(i,n)$((ORD(n) eq 31) and (mod(ORD(i)-1,7) le 2 or mod(ORD(i)-1,7) eq 4)) .. X(i,'2',n) =E= 0;
NUEVE5(i,n)$((ORD(n) eq 31) and (mod(ORD(i)-1,7) le 3)) .. X(i,'3',n) =E= 0;

MESANT1(i,j,n)$((ORD(n) eq 11 or ORD(n) eq 12 or ORD(n) eq 19) and (ord(i) eq 3 or (ord(i) eq 2 and ord(j) eq 3))) .. X(i,j,n) =E= 0;
MESANT2(i,j,n)$((ORD(n) eq 7 or ORD(n) eq 16 or ORD(n) eq 20) and (ord(i) eq 2 or (ord(i) eq 1 and ord(j) eq 3))) .. X(i,j,n) =E= 0;
MESANT3(i,j,n)$((ORD(n) eq 10 or ORD(n) eq 17) and ord(i) eq 1) .. X(i,j,n) =E= 0;
MESANT4(i,j,n)$((ORD(n) eq 13 or ORD(n) eq 14 or ORD(n) eq 15 or ORD(n) eq 18) and (ord(i) eq 3 or (ord(i) eq 2 and ord(j) eq 3) or (ord(i) eq 1 and ord(j) eq 1))) .. X(i,j,n) =E= 0;
MESANT5(i,j,n)$((ORD(n) eq 4 or ORD(n) eq 31) and (ord(i) eq 1 or ord(i) eq 2 )) .. X(i,j,n) =E= 0;

model firstapproach /all/;
solve firstapproach minimizing P_opt using MIP;