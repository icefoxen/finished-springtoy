(* spring.ml
   A spring connects two points.
   It has no mass.
   It has a spring constant, an optional friction factor, and a
   relaxed length.

   To do: Dampers!
   Someday, try out making drag proportional to speed.  So there's
   more drag if you go fast.

   Simon Heath
   March 22, 2007
*)


open Point;;

class spring springconst friction relaxedlength end1 end2 =
object (self)
  val k = springconst
  val f = friction
  val len = relaxedlength

  val end1 : point = end1
  val end2 : point = end2

  method getCurrentLength =
    end1#getPosVector#sub end2#getPosVector
      

  (* Mjolnir says the len is needed to make the force proportional
     to the rest length of the spring, not the absolute length.
     A long spring with the same k can be stretched further more easily
     than a short spring.
     strain = delta_l / natural_l
  *)
  method getForceMagnitude =
    let deltal = self#getCurrentLength#getMagnitude -. len in
      (deltal /. len) *. k *. f

  method getForceVector =
    let m = self#getForceMagnitude in
    let v = self#getCurrentLength#getUnitVector in
      v#mul m


  method calc dt = 
    let fvec = self#getForceVector#mul dt  in
      end1#applyForce fvec#invert;
      end2#applyForce fvec;
      end1#calc dt;
      end2#calc dt;
(*      Printf.printf "1: %f, %f   2: %f,%f\n" end1#getX end1#getY end2#getX end2#getY;  *)

  method getPoints =
    (end1, end2)

end;;
