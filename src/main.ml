
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
    Img.binarize m m (Img.seuil m);
    print_string "binarized\n";
    flush_all ();
    let ds = Matrix.to_img ( m ) in
    show img display;
    wait_key ();
    Img.image2grey img newSurface;
    print_string "image2greyed\n";
    flush_all ();
    show newSurface display;
    wait_key ();
    (* Filtre passe bas *)
    Img.sharpen m m;
    print_string "sharpened\n";
    flush_all ();
    show (Matrix.to_img m) display;
    wait_key();

            let rev = Img.reverse m in
            print_string "reversed\n";
            flush_all();
            let ds = Matrix.to_img ( rev ) in
            show ds display;
            wait_key ();
            let angle = Img.hough m in
            print_float angle;
            let rot = Img.rotate m angle in
            print_string "rotated\n";
            flush_all();
            let ds = Matrix.to_img ( rot ) in
            show ds display;
            wait_key ();
    (* on quitte *)
    exit 0
  end

let _ = main ()
