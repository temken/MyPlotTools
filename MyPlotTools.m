(* ::Package:: *)

(* ::Title:: *)
(*My Plot Tools (19/11/2019)*)


BeginPackage["MyPlotTools`"]

CreateColorScheme::usage="CreateColorScheme[name,colorlist] generates a continuous color scheme out of the color list, and adds it to the list of schemes under the given name";
fticksR::usage="fticksR[min_,max_,AMAX_: 1] a list of fancy logarithmic frame ticks."
fticksRodd::usage="fticksR[min_,max_,AMAX_: 1] a list of fancy logarithmic frame ticks."
fticksReven::usage="fticksR[min_,max_,AMAX_: 1] a list of fancy logarithmic frame ticks."
fticksRNoLabels::usage="fticksR[min_,max_,AMAX_: 1] a list of fancy logarithmic frame ticks."
myText::usage="myText[text_,size_:14,color_:Black] creates a string in my text style."
myFrameTicksStyle::usage="Use in a framed plot via FrameTicksStyle->myFrameTicksStyle."


(* ::Chapter:: *)
(*Create a color scheme*)


CreateColorScheme[name_,colorlist_]:=Module[{new},
(*force autoloading*)
ColorData[];
DataPaclets`ColorDataDump`colorSchemes;
DataPaclets`ColorDataDump`colorSchemeNames;

Unprotect[ColorData];
new={{name,"",{}},{"Gradients"},1,{0,1},colorlist,""};
AppendTo[DataPaclets`ColorDataDump`colorSchemes,new];
AppendTo[DataPaclets`ColorDataDump`colorSchemeNames,new[[1,1]]];
]


(* ::Chapter:: *)
(*Fancy frame ticks for log plots*)


Clear[PrettyExp]
PrettyExp[x_,MAX_: 0]:=If[Abs[x]>=MAX,Superscript[10,x],If[x>=0,10^x,10.^x]]
Clear[fticksR]
fticksR[min_,max_,AMAX_: 1]:=Flatten[Table[Prepend[Flatten[Table[{i*10.^x,"",{.005,0}},{i,2,9}],{1}],{10.^x,PrettyExp[x,AMAX],{.015,0}}],{x,Floor[Log[10,min]],Ceiling[Log[10,max]],1}],1]
fticksRodd[min_,max_,AMAX_: 1]:=Flatten[Table[Prepend[Flatten[Table[{i*10.^x,"",{.005,0}},{i,2,9}],{1}],If[OddQ[x],{10.^x,PrettyExp[x,AMAX],{.015,0}},{10.^x,"",{.015,0}}]],{x,Floor[Log[10,min]],Ceiling[Log[10,max]],1}],1]
fticksReven[min_,max_,AMAX_: 1]:=Flatten[Table[Prepend[Flatten[Table[{i*10.^x,"",{.005,0}},{i,2,9}],{1}],If[EvenQ[x],{10.^x,PrettyExp[x,AMAX],{.015,0}},{10.^x,"",{.015,0}}]],{x,Floor[Log[10,min]],Ceiling[Log[10,max]],1}],1]
Clear[fticksRNoLabels]
fticksRNoLabels[min_,max_,AMAX_: 1]:=Flatten[Table[Prepend[Flatten[Table[{i*10.^x,"",{.005,0}},{i,2,9}],{1}],{10.^x,"",{.015,0}}],{x,Floor[Log[10,min]],Ceiling[Log[10,max]],1}],1]



(* ::Chapter:: *)
(*My text style*)


myText[text_,size_:14,color_:Black]:=Text[Style[text,color,FontFamily->"Carlito",size]]
myFrameTicksStyle=Directive[Black,12,FontFamily->"Carlito"];


(* ::Chapter:: *)
(*My Plot Theme*)


Themes`AddThemeRules["myPlotTheme",
  Frame->True,
  FrameTicksStyle->myFrameTicksStyle,
  Joined->True,
  AspectRatio->1/GoldenRatio,
  ImageSize->400
];


Print[
"MyPlotTools by Timon Emken (2019)."]
EndPackage[]
