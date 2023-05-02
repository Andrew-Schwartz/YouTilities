(* ::Package:: *)

UnitForm[units_][quantity_]:=UnitConvert[quantity,units]
UnitForm::usage="like UnitConvert but for postfix";


Subscript[s_String, q]:=Quantity[s]


also[f_][x_]:=(f@x;x)


alsoPrint[f_:Identity]:=also[Print@*f]


loadConstants[]:=(
\[HBar]=Subscript["ReducedPlanckConstant", q];
c=Subscript["SpeedOfLight", q];
Subscript[q, e]=Subscript["ElectronCharge", q];
Subscript[m, e]=Subscript["ElectronMass", q];
Subscript[\[Epsilon], 0]=Subscript["VacuumPermittivity", q];
)


saveDirectory[]:=Module[
	{dir=FileNameJoin@{NotebookDirectory[],"figures"}},
	If[
		!DirectoryQ@dir,
		CreateDirectory[FileNameJoin@{NotebookDirectory[],"figures"}],
		dir
	]
]
save[fig_,name_String,format_String:"pdf",dpi_Integer:500]:=Export[FileNameJoin@{saveDirectory[],name<>"."<>format},thing,ImageResolution->dpi]
save[fig_,name_String,formats_List,dpi_Integer:500]:=save[fig,name,#,dpi]&/@formats
alsoSave[name_String,format_String:"pdf",dpi_Integer:500]:=also[save[#,name,format,dpi]&]
alsoSave[name_String,formats_List,dpi_Integer:500]:=also[save[#,name,formats,dpi]&]
