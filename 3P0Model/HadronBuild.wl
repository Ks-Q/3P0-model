(* ::Package:: *)

(* ::Package:: *)

(* : Mathematica Version : 14.0 *)

(* : Package Version : 1.0 *)

(* : Title : HadronBuild *)

(* : Author : Kaisa Qiao*)

(* : History :
2024.07.30 

*)

(* : Warning : *)

(* : Reference : *)

(* : Limitation : *)



RSHOp::usage="RSHOp[n,l,p,\[Beta]] denotes the radiatial wave function with SHO form.\
p means in momentum space. \
\[Beta] relates to oscillator frequence and mass."
\[Psi]SHOp::usage="\[Psi]SHOp[{n,l,ml},{p,\[Theta],\[Phi]},\[Beta]] denotes the spatial wave function, \
n is the radial excitation number, \
l is the angular momentum, \
ml is the angular momentum projection on the z-axis."


Begin["`Private`"]
RSHOp[n_,l_,p_,beta_]:=((-1)^k (-I)^l)/beta^(3/2) Sqrt[(2n!)/Gamma[n+l+3/2]](p/beta)^l Exp[-p^2/(2beta^2)]LaguerreL[n,l+1/2,p^2/beta^2]

\[Psi]SHOp[{n_,l_,ml_},{p_,\[Theta]_,\[Phi]_},beta_]:=RSHOp[n,l,p,beta]*SphericalHarmonicY[l,ml,\[Theta],\[Phi]]
End[]
