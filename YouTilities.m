(* ::Package:: *)

UnitForm[units_][quantity_]:=UnitConvert[quantity,units]
UnitForm::usage="like UnitConvert but for postfix";


Subscript[s_String, q]:=Quantity[s]


also[f_][x_]:=(f@x;x)


alsoPrint[f_:Identity]:=also[Print@*f]


printThen[f_][x_]:=Module[]


loadQuantites[\[HBar]]:=\[HBar]=Quantity["ReducedPlanckConstant"];
loadQuantites[c]:=c=Quantity["SpeedOfLight"];
loadQuantites[Subscript[q, e]]:=Subscript[q, e]=Quantity["ElectronCharge"];
loadQuantites[Subscript[m, e]]:=Subscript[m, e]=Quantity["ElectronMass"];
loadQuantites[Subscript[\[Epsilon], 0]]:=Subscript[\[Epsilon], 0]=Quantity["VacuumPermittivity"];
loadQuantites[Subscript[\[Mu], 0]]:=Subscript[\[Mu], 0]=Quantity["VacuumPermeability"];
SetAttributes[loadQuantites,Listable]
(*loadQuantites[symbs__]:=(loadQuantites@#)&/@{symbs}*)
loadQuantites[symbs__]:=loadQuantites[{symbs}]


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


defaultColorData[]:=ColorData[97]
