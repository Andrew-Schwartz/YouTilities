(* ::Package:: *)

UnitForm[units_][quantity_]:=UnitConvert[quantity,units]
UnitForm::usage="like UnitConvert but for postfix";


Subscript[s_String, q]:=Quantity[s]


also[f_][x_]:=(f@x;x)


alsoPrint[f_:Identity]:=also[Print@*f]


printThen[f_][x_]:=Module[]


ClearAll@loadQuantites;
(* Makes loadQuantities idempotent, so loadQuantities[loadQuantities[\[HBar]]]==loadQuantities[\[HBar]] *)
loadQuantites[q_Quantity]:=q;
(* Define constants that can be loaded *)
loadQuantites[HoldPattern@\[HBar]]:=\[HBar]=Quantity["ReducedPlanckConstant"];
loadQuantites[HoldPattern@c]:=c=Quantity["SpeedOfLight"];
loadQuantites[HoldPattern@Subscript[q, e]]:=Subscript[q, e]=Quantity["ElectronCharge"];
loadQuantites[HoldPattern@Subscript[m, e]]:=Subscript[m, e]=Quantity["ElectronMass"];
loadQuantites[HoldPattern@Subscript[\[Epsilon], 0]]:=Subscript[\[Epsilon], 0]=Quantity["VacuumPermittivity"];
loadQuantites[HoldPattern@Subscript[\[Mu], 0]]:=Subscript[\[Mu], 0]=Quantity["VacuumPermeability"];
(* Allow list of quantities to be loaded  *)
SetAttributes[loadQuantites,Listable]
(* Handle other symbols not defined above, and return 0 *)
loadQuantites::unknownSymb="Unknown symbol `1`";
loadQuantites[symb_]:=(Message[loadQuantites::unknownSymb,symb];0);
(* Load product, sum, etc of constants at once *)
loadQuantites[HoldPattern[f_[symbs__]]]:=f@@loadQuantites[{symbs}]
(* Allow list of quantities to be loaded without surrounding in brackets *)
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
