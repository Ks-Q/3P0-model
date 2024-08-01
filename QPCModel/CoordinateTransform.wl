(* ::Package:: *)

(* ::Package:: *)
(* : Mathematica Version : 14.0 *)


(* : Package Version : 1.0 *)

(* : Title : Coordinate transform *)

(* : Author : Kaisa Qiao*)

(* : History :
2024.07.30 

*)

(* : Warning : *)

(* : Reference : *)

(* : Limitation : *)


CartesianToSpherical::usage="CartesianToSpherical[{x,y,z}] transform a set of Cartesian coordinate {x,y,z} to Spherical coordinate {\[Rho],\[Theta],\[Phi]}. \
x,y,z can't equal to zero simutaneously"
NewSphericalVector::usage="NewSphericalVector[{a,\[Theta]1,\[Phi]1},{b,\[Theta]2,\[Phi]2}]sums up vectors in spherical system. \
The sum of two vectors can't equal to zero beacause of the characters of the function CartesianToSpherical"


pA\[Rho]::usage==""
pA\[Lambda]::usage==""
pB\[Rho]::usage==""
pB\[Lambda]::usage==""
pC::usage==""


Begin["`Private`"]


CartesianToSpherical[{x_,y_,z_}]:={Sqrt[x^2+y^2+z^2],ArcCos[z/Sqrt[x^2+y^2+z^2]],If[x!=0,ArcTan[x,y],Sign[y]*\[Pi]/2]}


NewSphericalVector[{a_,\[Theta]1_,\[Phi]1_},{b_,\[Theta]2_,\[Phi]2_}]:=Block[
{ca ={a Cos[\[Phi]1] Sin[\[Theta]1],a Sin[\[Theta]1] Sin[\[Phi]1],a Cos[\[Theta]1]},
cb ={b Cos[\[Phi]2] Sin[\[Theta]2],b Sin[\[Theta]2] Sin[\[Phi]2],b Cos[\[Theta]2]}
},
CartesianToSpherical[ca+cb]]


(* ::Text:: *)
(*Jacobi coordinate*)


(* ::Text:: *)
(*\:8fd9\:91cc\:6211\:4eec\:5c061, 3 quark  \:8bbe\:4e3a  A, B\:7684\[Rho]  mode*)


(*pA\[Rho] = 1/Sqrt[2](2p1+p4-P)*)
pA\[Rho][{P_,aP_,bP_},{p1_,a1_,b1_},{p4_,a4_,b4_}]:=

Block[{p,pp},
p = NewSphericalVector[{p4,a4,b4},{-P,aP,bP}];
pp = NewSphericalVector[{2p1,a1,b1},p];
{1/Sqrt[2] pp[[1]],pp[[2]],pp[[3]]}];
(*pA\[Lambda] = Sqrt[3/2](P-p4)*)
pA\[Lambda][{P_,aP_,bP_},{p1_,a1_,b1_},{p4_,a4_,b4_}]:=

Block[{pp},
pp = NewSphericalVector[{P,aP,bP},{-p4,a4,b4}];
{Sqrt[3/2]pp[[1]],pp[[2]],pp[[3]]}];
(*pB\[Rho] = 1/Sqrt[2](2p1+p4-P)*)
pB\[Rho][{P_,aP_,bP_},{p1_,a1_,b1_},{p4_,a4_,b4_}]:=
Block[{p,pp},
p = NewSphericalVector[{p4,a4,b4},{-P,aP,bP}];
pp = NewSphericalVector[{2p1,a1,b1},p];
{1/Sqrt[2] pp[[1]],pp[[2]],pp[[3]]}];
(*pB\[Lambda] = 1/Sqrt[6](P-3p4)*)
pB\[Lambda][{P_,aP_,bP_},{p1_,a1_,b1_},{p4_,a4_,b4_}]:=
Block[{pp},
pp = NewSphericalVector[{P,aP,bP},{-3p4,a4,b4}];
{1/Sqrt[6] pp[[1]],pp[[2]],pp[[3]]}];
(*pC = p4-Subscript[m, q]/(Subscript[m, q]+Subscript[m, Q])P*)
pC[{P_,aP_,bP_},{p1_,a1_,b1_},{p4_,a4_,b4_},mq_,mQ_]:=
Block[{p},
p = NewSphericalVector[{p4,a4,b4},{-(mq/(mq+mQ))P,aP,bP}]];


End[]
