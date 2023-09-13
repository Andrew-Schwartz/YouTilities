(* ::Package:: *)

UnitForm[units_][quantity_]:=UnitConvert[quantity,units]
UnitForm::usage="like UnitConvert but for postfix";


Subscript[s_String, q]:=Quantity[s]


also[f_][x_]:=(f@x;x)


alsoPrint[f_:Identity]:=also[Print@*f]


printThen[f_][x_]:=Module[]


ClearAll@loadQuantities;
(* Makes loadQuantities idempotent, so loadQuantities[loadQuantities[\[HBar]]]==loadQuantities[\[HBar]] *)
loadQuantities[q_Quantity]:=q;
(* Define constants that can be loaded *)
loadQuantities[HoldPattern@\[HBar]]:=\[HBar]=Quantity["ReducedPlanckConstant"];
loadQuantities[HoldPattern@c]:=c=Quantity["SpeedOfLight"];
loadQuantities[HoldPattern@Subscript[q, e]]:=Subscript[q, e]=Quantity["ElectronCharge"];
loadQuantities[HoldPattern@Subscript[m, e]]:=Subscript[m, e]=Quantity["ElectronMass"];
loadQuantities[HoldPattern@Subscript[\[Epsilon], 0]]:=Subscript[\[Epsilon], 0]=Quantity["VacuumPermittivity"];
loadQuantities[HoldPattern@Subscript[\[Mu], 0]]:=Subscript[\[Mu], 0]=Quantity["VacuumPermeability"];
(* Allow list of quantities to be loaded  *)
SetAttributes[loadQuantities,Listable]
(* Handle other symbols not defined above, and return 0 *)
loadQuantities::unknownSymb="Unknown symbol `1`";
loadQuantities[symb_]:=(Message[loadQuantities::unknownSymb,symb];0);
(* Load product, sum, etc of constants at once *)
loadQuantities[HoldPattern[f_[symbs__]]]:=f@@loadQuantities[{symbs}]
(* Allow list of quantities to be loaded without surrounding in brackets *)
loadQuantities[symbs__]:=loadQuantities[{symbs}]
(* With no arguments, load all quantities *)
loadQuantities[]:=(Keys[DownValues[loadQuantities]]/.loadQuantities->List)[[;;-5,1]]//Flatten//ReleaseHold//loadQuantities


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
