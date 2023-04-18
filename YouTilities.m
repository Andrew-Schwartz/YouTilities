(* ::Package:: *)

UnitForm[units_][quantity_]:=UnitConvert[quantity,units]
UnitForm::usage="like UnitConvert but better";


Subscript[s_String, q]:=Quantity[s]


sideEffect[f_][x_]:=(f@x;x)


print[f_][x_]:=sideEffect[Print@*f][x]


loadConstants[]:=(
\[HBar]=Subscript["ReducedPlanckConsant", q];
c=Subscript["SpeedOfLight", q];
Subscript[q, e]=Subscript["ElectronCharge", q];
Subscript[m, e]=Subscript["ElectronMass", q];
Subscript[\[Epsilon], 0]=Subscript["VacuumPermittivity", q];
)
