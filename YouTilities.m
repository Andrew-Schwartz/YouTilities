(* ::Package:: *)

UnitForm[units_][quantity_]:=UnitConvert[quantity,units]
UnitForm::usage="like UnitConvert but for postfix";


Subscript[s_String, q]:=Quantity[s]


also[f_][x_]:=(f@x;x)


alsoPrint[f_:Identity]:=also[Print@*f]


ClearAll@loadQuantities;
quantityAssoc[HoldPattern@symb]:=<|\[HBar]->Quantity["ReducedPlanckConstant"]|>[symb]
(* Makes loadQuantities idempotent, so loadQuantities[loadQuantities[\[HBar]]]==loadQuantities[\[HBar]] *)
loadQuantities[q_Quantity]:=q;
(* Define constants that can be loaded *)
loadQuantities[HoldPattern@\[HBar]]:=\[HBar]=Quantity["ReducedPlanckConstant"];
loadQuantities[HoldPattern[h]]:=h=Quantity["PlanckConstant"];
loadQuantities[HoldPattern@c]:=c=Quantity["SpeedOfLight"];
loadQuantities[HoldPattern@Subscript[q, e]]:=Subscript[q, e]=Quantity["ElectronCharge"];
loadQuantities[HoldPattern@Subscript[m, e]]:=Subscript[m, e]=Quantity["ElectronMass"];
loadQuantities[HoldPattern@Subscript[m, p]]:=Subscript[m, p]=Quantity["ProtonMass"];
loadQuantities[HoldPattern@Subscript[m, n]]:=Subscript[m, n]=Quantity["NeutronMass"];
loadQuantities[HoldPattern@Subscript[m, \[Mu]]]:=Subscript[m, \[Mu]]=Quantity["MuonMass"];
loadQuantities[HoldPattern@Subscript[m, \[Tau]]]:=Subscript[m, \[Tau]]=Quantity["TauMass"];
loadQuantities[HoldPattern@Subscript[\[Epsilon], 0]]:=Subscript[\[Epsilon], 0]=Quantity["VacuumPermittivity"];
loadQuantities[HoldPattern@Subscript[\[Mu], 0]]:=Subscript[\[Mu], 0]=Quantity["VacuumPermeability"];
loadQuantities[HoldPattern@Subscript[a, 0]]:=Subscript[a, 0]=Quantity["BohrRadius"];
loadQuantities[HoldPattern@G]:=G=Quantity["GravitationalConstant"];
loadQuantities[HoldPattern@Subscript[N, A]]:=Subscript[N, A]=Quantity["AvogadroConstant"];
loadQuantities[HoldPattern@Subscript[k, B]]:=Subscript[k, B]=Quantity["BoltzmannConstant"];
(* Allow list of quantities to be loaded  *)
SetAttributes[loadQuantities,Listable]
(*(* Handle other symbols not defined above, and return 0 *)
loadQuantities::unknownSymb="Unknown symbol `1`";
loadQuantities[symb_]:=(Message[loadQuantities::unknownSymb,symb];symb);*)
(* Return unknown symbols unchanged *)
loadQuantities[symb_]:=symb;
(* Load product, sum, etc of constants at once *)
loadQuantities[HoldPattern[f_[symbs__]]]:=f@@loadQuantities[{symbs}]
(* Allow list of quantities to be loaded without surrounding in brackets *)
loadQuantities[symbs__]:=loadQuantities[{symbs}]
(* With no arguments, load all quantities *)
(* todo figure out how to not have to hardcode the -5 (# of non-definition defs) *)
loadQuantities[]:=(Keys[DownValues[loadQuantities]]/.loadQuantities->List)[[;;-5,1]]//Flatten//ReleaseHold//loadQuantities


saveDirectory[]:=Module[
	{dir=FileNameJoin@{NotebookDirectory[],"mathematica_figures"}},
	If[
		!DirectoryQ@dir,
		CreateDirectory[FileNameJoin@{NotebookDirectory[],"mathematica_figures"}],
		dir
	]
]
save[fig_,name_String,format_String:"pdf",dpi_Integer:500]:=Export[FileNameJoin@{saveDirectory[],name<>"."<>format},fig,ImageResolution->dpi]
save[fig_,name_String,formats_List,dpi_Integer:500]:=save[fig,name,#,dpi]&/@formats
alsoSave[name_String,format_String:"pdf",dpi_Integer:500]:=also[save[#,name,format,dpi]&]
alsoSave[name_String,formats_List,dpi_Integer:500]:=also[save[#,name,formats,dpi]&]


defaultColorData[]:=ColorData[97]
defaultColorData[n_Integer]:=defaultColorData[][n]


ClearAll@eigenStuff
eigenStuff::zeroEigenvectors="Some zero-eigenvectors";
eigenStuff::unequal="#vals!=#vecs";
eigenStuff[matrix_,opts:OptionsPattern[{ai\[Lambda]->False,rref->False}]]:=Module[
	{
		ai\[Lambda]=OptionValue@ai\[Lambda],
		rref=OptionValue@rref,
		len=Length@matrix,
		vecs,vals
	},
	{vals,vecs}=Eigensystem[matrix];
	If[ContainsAny[vecs,{Table[0,len]}],
		Throw[Message[eigenStuff::zeroEigenvectors]]
	];
	AI\[Lambda]=matrix-# IdentityMatrix@len&/@vals;
	If[Length/@{vals,vecs}/.List->Equal,
		Grid[
			Join[
				{{"\[Lambda]",If[ai\[Lambda],"A-I\[Lambda]",Nothing],If[rref,"RREF",Nothing],"Eigenvector"}},
				{
					vals,
					If[ai\[Lambda],MatrixForm/@AI\[Lambda],Nothing],
					If[rref,MatrixForm@*RowReduce/@AI\[Lambda],Nothing],
					MatrixForm/@vecs
				}\[Transpose]
			],
			Frame->All
		],
		Message[eigenStuff::unequal]
	]
]


ClearAll[enumerate]
enumerate=MapIndexed[{#2[[1]],#1}&];


(* FullSimplifty + ComplexExpand (to get rid of "Conjugate"s) *)
(* From melt *)
Clear[cf];
(*cf[expr_]:=FullSimplify@ComplexExpand@expr;*)
cf[expr_]:=FullSimplify[ComplexExpand@Normal@expr,Thread[DeleteDuplicates@Cases[Normal@expr,_Symbol,\[Infinity]]>0]]

cf[expr_,assumps_]:=FullSimplify[ComplexExpand@Normal@expr,assumps]
cf::usage = "FullSimplify assuming that all symbols are real and positive.";


(* Pride Flags! *)
(* How to add gradients: https://mathematica.stackexchange.com/questions/57885/is-it-possible-to-insert-new-colour-schemes-into-colordata/57893#57893 *)
(* Flag rgb colors: https://www.flagcolorcodes.com/flags/pride *)

ColorData[1];
(* Export this function so users can add gradients as needed *)
insertGradient[name_,colors_]:=(
	AppendTo[
		DataPaclets`ColorDataDump`colorSchemes,
		{{name,"",{}},{"Gradients"},1,{0,1},colors,""}
	];
	AppendTo[
		DataPaclets`ColorDataDump`colorSchemeNames,
		name
	];
)
Module[{data,names,colors},
	data=Import[NotebookDirectory[]<>"flags.tsv","TSV","Numeric"->False];
	names=#<>"Flag"&/@data[[;;,1]];
	colors=Map[RGBColor["#"<>#]&,data[[;;,2;;]],{2}];
	
	insertGradient@@#&/@Transpose@{names,colors};
]
