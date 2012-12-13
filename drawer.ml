(* drawer.ml
   Draws stuff!

*)

open Util;;
open Drawing;;

class drawer screen =
object (self)
  
  (* We need to be able to draw different types of atoms in different
     colors.
     Pout.

  *)
  method drawSpring (spring : Spring.spring) =
    let atomsize = 2 in
    let p1, p2 = spring#getPoints in
    let logx1 = x2screen p1#getX !Util.logscreenx
    and logy1 = y2screen p1#getY !Util.logscreeny
    and logx2 = x2screen p2#getX !Util.logscreenx
    and logy2 = y2screen p2#getY !Util.logscreeny in

    let showP1 = (logx1 > atomsize) && (logx1 < (!screenx - atomsize)) &&
	(logy1 > atomsize) && (logy1 < (!screeny - atomsize))
    and showP2 = (logx2 > atomsize) && (logx2 < (!screenx - atomsize)) &&
	(logy2 > atomsize) && (logy2 < (!screeny - atomsize)) in

      if showP1 then
	  drawCircle screen (255,0,0) logx1 logy1 atomsize;
      if showP2 then
	  drawCircle screen (255,0,0) logx2 logy2 atomsize;
      if showP1 && showP2 then
	drawLine screen (0,255,0) logx1 logx2 logy1 logy2;


  method drawSprings lst =
    List.iter self#drawSpring lst
(*
  method drawForce (atom1 : Atom.atom) (atom2 : Atom.atom) =
    let logx1 = x2screen atom1#getX !Util.logscreenx
    and logx2 = x2screen atom2#getX !Util.logscreenx
    and logy1 = y2screen atom1#getY !Util.logscreeny
    and logy2 = y2screen atom2#getY !Util.logscreeny in
    let dx = abs (logx1 - logx2)
    and dy = abs (logy1 - logy2) in
    let dsquared = (dx * dx) + (dy * dy)
    and rsquared = int_of_float (atom1#getAttRange *. atom1#getAttRange) in
      if dsquared < rsquared then
	drawLine screen (0,255,0) logx1 logx2 logy1 logy2
      else
	();
      


  method drawForces atoms =
    let rec loop current rest =
      if rest = [] then
	()
      else (
	List.iter (fun (x : Atom.atom) -> self#drawForce x current) rest;
	loop (List.hd rest) (List.tl rest)
      )
    in
      loop (List.hd atoms) (List.tl atoms)

(*
    List.fold_left (fun a1 a2 -> self#drawForce a1 a2; a2)
      (List.hd atoms) (List.tl atoms)
*)

  method drawAtoms atoms =
    List.iter self#drawAtom atoms
*)
  method clearBackground =
    drawFilledRect screen 0 0 !screenx !screeny (0,0,0)
      
end;;
