(* ::Package:: *)

BeginPackage["YouTilities`"];


Begin["`Private`"];


UnitForm[units_][quantity_]:=UnitConvert[quantity,units]


Subscript[s_String, q]:=Quantity[s]


sideEffect[f_][x_]:=(f@x;x)


End[];


EndPackage[];
