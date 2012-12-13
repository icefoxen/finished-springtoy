(* point.ml
   A point is just that, a point.  It has a position and a mass, and
   has springs connected to it.
   I will eventually need subclasses of it that are some sort of prime mover, 
   generating movement on its own by oscillating in some pattern.
*)

open Vector;;

class point x' y' m =
object (self)
  val mass = m

  val mutable velvector = new vector 0. 0.

  val mutable posvector = new vector x' y'

  val mutable accelvector = new vector 0. 0.
  val mutable prevaccelvector = new vector 0. 0.


  method getM  = m

  method getV = velvector
  method setV v = velvector <- v

  method getX = posvector#getX
  method getY = posvector#getY
  method setX x' = posvector#setX x'
  method setY y' = posvector#setY y'

  method getPosVector = posvector

  method setXY x' y' = 
    posvector#setXY x' y';

  method getKE =
    0.5 *. m *. (velvector#getMagnitude *. velvector#getMagnitude)



  (* Optimization: Sum up all forces before dividing by mass *)
  method applyForce (fvec : vector) =
    accelvector <- accelvector#add fvec; 



  (* Apparently this is called "Very basic Euler integration"
     p = pold + v*t, v = vold + accel.

     More accurate to do:
     p = pold + v*t + 0.5*accel*t^2.

     This is still inaccurate; it falls down badly when the change
     in acceleration is very high... as in during fast collisions.

     Then there's Beeman's Algorithm, which is what we're using here:
     p = pold + v*t + (2/3)a*t^2 - (1/6)lasta*t^2 
  *)
  method calc dt =
    accelvector <- accelvector#div mass;
    velvector <- velvector#add accelvector;

    let tsquared = dt *. dt in
    let term1 = (accelvector#mul ((2. /. 3.) *. tsquared))
    and term2 = (prevaccelvector#mul ((1. /. 6.) *. tsquared)) in
      posvector <- ((posvector#add (velvector#mul dt))#add term1)#sub term2;

      (*
	let ke = (accelvector#mul 0.5)#mul (dt *. dt) in
	posvector <- (posvector#add (velvector#mul dt))#add ke;
      *)

      prevaccelvector#setXY accelvector#getX accelvector#getY;
      accelvector#setXY 0. 0.;



end;;



class fixedpoint x' y' m =
object (self)
  inherit point x' y' m

  method applyForce (fvec : vector) =
    ()

  method calc dt =
    ()

end
