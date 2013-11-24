
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()
 
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst
 
(* main *)
let main () =
  begin
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    sdl_init ();
    let img = Sdlloader.load_image Sys.argv.(1) in
    let (w,h) = Img.get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    let newSurface = Sdlvideo.create_RGB_surface_format img [] w h in
    let m = Matrix.from_img img in
    let ds_mat = Img.binarize m (Img.seuil m) in
    let ds = Matrix.to_img ( ds_mat ) in
    show img display;
    wait_key ();
    Img.image2grey img newSurface;
    show newSurface display;
    wait_key ();
    show ds display;
    wait_key ();
    (* Filtre passe bas *)
    let img_convolved = Img.sharpen img in
    show img_convolved display;
    wait_key();

	    let ds = Matrix.to_img ( Img.reverse m ) in
	    show ds display;
	    wait_key ();
            let ds = Matrix.to_img ( Img.rotate m 10. ) in
            show ds display;
            wait_key ();
    (* on quitte *)
    exit 0
  end
 
let _ = main ()
