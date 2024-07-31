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


Begin["`Private`"]


CartesianToSpherical[{x_,y_,z_}]:={Sqrt[x^2+y^2+z^2],ArcCos[z/Sqrt[x^2+y^2+z^2]],If[x!=0,ArcTan[x,y],Sign[y]*\[Pi]/2]}


NewSphericalVector[{a_,\[Theta]1_,\[Phi]1_},{b_,\[Theta]2_,\[Phi]2_}]:=Block[
{ca ={a Cos[\[Phi]1] Sin[\[Theta]1],a Sin[\[Theta]1] Sin[\[Phi]1],a Cos[\[Theta]1]},
cb ={b Cos[\[Phi]2] Sin[\[Theta]2],b Sin[\[Theta]2] Sin[\[Phi]2],b Cos[\[Theta]2]}
},
CartesianToSpherical[ca+cb]]


End[]
