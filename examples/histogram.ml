module Gp = Gnuplot

let normal =
  let pi = 4.*.atan 1. in
  let saved = ref None in
  fun st ->
  match !saved with
  | Some (r, t) -> saved := None; r *. sin t
  | None ->
     let u1 = Random.State.float st 1. in
     let u2 = Random.State.float st 1. in
     let r = sqrt (-2. *. log u1) in
     let t = 2. *. pi *. u2 in
     saved := Some (r, t) ;
     r *. cos t

let () =
  let generate_noise () =
    let st = Random.State.make_self_init () in
    (* Box-Muller transform *)
    Base.List.init 1000 ~f:(fun _ -> normal st)
  in
  let gp = Gp.create () in
  (* Histogram of Gaussian samples *)
  Gp.set
    gp
    ~title:"Histogram of gaussian distribution";
  let boxes data = Gp.Series.boxes data ~fill:`Solid ~color:`Red ~bins:30 in
  let histeps data = Gp.Series.histeps data ~color:`Blue ~bins:30 in
  Gp.plot_many gp ~range:(Gp.X (-10., 10.))
    [ boxes (generate_noise ());
      histeps (generate_noise ())] ;
  Unix.sleep 10;
  Gp.close gp
