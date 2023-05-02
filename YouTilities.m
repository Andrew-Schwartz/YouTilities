(* ::Package:: *)

UnitForm[units_][quantity_]:=UnitConvert[quantity,units]
UnitForm::usage="like UnitConvert but better";


Subscript[s_String, q]:=Quantity[s]


also[f_][x_]:=(f@x;x)


alsoPrint[f_][x_]:=also[Print@*f][x]


loadConstants[]:=(
\[HBar]=Subscript["ReducedPlanckConsant", q];
c=Subscript["SpeedOfLight", q];
Subscript[q, e]=Subscript["ElectronCharge", q];
Subscript[m, e]=Subscript["ElectronMass", q];
Subscript[\[Epsilon], 0]=Subscript["VacuumPermittivity", q];
)


CreateDirectory[FileNameJoin@{NotebookDirectory[],"figures"}];
save[name_String,thing_,format_String:"pdf",dpi_Integer:500]:=Export[FileNameJoin@{NotebookDirectory[],"figures",name<>"."<>format},thing,ImageResolution->dpi]
save[name_String,thing_,formats_List,dpi_Integer:500]:=Table[
	save[name,thing,format,dpi],
	{format,formats}
]
alsoSave[name_String,format_String:"pdf",dpi_Integer:500][thing_]:=also[save[name,thing,format,dpi]]
