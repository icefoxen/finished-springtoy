(* driver.ml
   This is the class that actually does the mainloop stuff.
   It is completely seperate from rooms and such.
*)


open Util;;
open Sdlkey;;

open Point;;
open Spring;;
open Vector;;

(*

let makeRandomAtom () =
  let xpos = Random.float 800.
  and ypos = Random.float 600.
  and xvel = (Random.float 1.) -. 0.5
  and yvel = (Random.float 1.) -. 0.5 in
  let atom = makeAtom5 xpos ypos in
    atom#applyForce (new vector xvel yvel);
    atom
;;

let makeRandomAtom2 () =
  let xpos = Random.float 800.
  and ypos = Random.float 600.
  and xvel = (Random.float 1.) -. 0.5
  and yvel = (Random.float 1.) -. 0.5 in
  let atom = makeAtom3 xpos ypos in
    atom#applyForce (new vector xvel yvel);
    atom
;;

*)

let makePoints () =
  let p0 = new fixedpoint 350. 300. 10.0
  and p1 = new point 400. 300. 10.0
  and p2 = new point 420. 300. 10.0
  and p3 = new point 440. 300. 10.0
  and p4 = new point 460. 300. 10.0
  and p5 = new point 480. 300. 10.0 in

    p5#applyForce (new vector (20.0) 0.0);
  let s0 = new spring 20.0 1.0 20.0 p0 p1
  and s1 = new spring 20.0 1.0 20.0 p1 p2
  and s2 = new spring 20.0 1.0 20.0 p2 p3
  and s3 = new spring 20.0 1.0 20.0 p3 p4
  and s4 = new spring 20.0 1.0 20.0 p4 p5 in
    [s0; s1; s2; s3; s4]
;;

let makePointGrid () =
  let springs = ref [] in
  let addSpring x = springs := x :: !springs in
  let currentPoint = ref (new fixedpoint 0. 0. 10.0) in
    for x = 5 to 20 do
      for y = 5 to 20 do
	let np = new point ((float_of_int y) *. 20.) 
	  ((float_of_int x) *. 20.) 10.0 in
	  addSpring (new spring 200.0 1.0 50.0 !currentPoint np);
	  currentPoint := np;
      done
      done;
      let np = new fixedpoint 420. 420. 10. in
	addSpring (new spring 200.0 1.0 50.0 !currentPoint np);
      !springs
;;

let makePointLine () =
  let springs = ref [] in
  let addSpring x = springs := x :: !springs in
  let currentPoint = ref (new fixedpoint 80. 300. 10.0) in
    for x = 5 to 20 do
      let np = new point ((float_of_int x) *. 30.) 300.0 10.0 in
	addSpring (new spring 20.0 1.0 30.0 !currentPoint np);
	currentPoint := np;	
    done;
    !currentPoint#setXY (!currentPoint#getX) (!currentPoint#getY +. 1.5);
    let np = new fixedpoint 630. 300. 10. in
      addSpring (new spring 200.0 1.0 20.0 !currentPoint np);

      !springs
;;

	


class driver scr =
object (self)
  val mutable objects : spring list = []

  val mutable framecount = 0
  val mutable lastframe = 0
  val mutable framestarttime = 0
  val mutable dt = 0
  val mutable maxdt = 0
  val mutable energysum = 0.

  val mutable temp = 0.0

  val mutable continue = true

  val drawer = new Drawer.drawer scr




  initializer
    objects <- makePointLine ();


  method drawObjects : unit =
    drawer#drawSprings objects;


  method draw =
    drawer#clearBackground;
    self#drawObjects;

  method doInput =
    ignore (Sdlevent.poll());
    if (is_key_pressed KEY_q) then
      self#stopGame;
    
(*
  method collideObjects =
    let rec loop current rest =
      if rest = [] then
	()
      else (
	List.iter (fun (x : atom) -> current#influenceAtom x dt) rest;
	loop (List.hd rest) (List.tl rest)
      )
    in
      loop (List.hd objects) (List.tl objects)
*)

  method updateTimers =
    lastframe <- framestarttime;
    framestarttime <- Sdltimer.get_ticks ();
    framecount <- framecount + 1;

    (* Max timestep is 75ms.  Any longer and things can fall through
       things. *)
    dt <- min 75 (framestarttime - lastframe);
    maxdt <- max dt maxdt;


  (* Fixed timestep is actually a bonus for simulations... *)
  method calculate =
    List.iter (fun (x : spring) -> x#calc 1.) objects;



  method mainLoop =
    while continue do

      self#updateTimers;
      self#doInput;
      self#calculate;
      self#draw;
(*      Sdltimer.delay 300; *)

	flush stdout;
	Sdlvideo.flip scr;
    done;



  method addObject x =
    objects <- x :: objects

  method addObjects x =
    objects <- x @ objects

  method removeObject x =
    objects <- List.filter (fun itm -> itm <> x) objects


  (* This should eventually lead to a main menu or such, but for now
     just quits *)
  method stopGame =
    continue <- false;
    Printf.printf "Max dt: %d\n" dt;

end;;
