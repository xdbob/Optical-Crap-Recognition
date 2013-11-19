
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
(* On crée la surface d'affichage en doublebuffering -> image normale *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    show img display;
    wait_key ();
    (* Gris-nairisation *)
    let newSurface = Sdlvideo.create_RGB_surface_format img [] w h in
            Img.image2grey img newSurface;
            let m = Matrix.from_img img in
            show newSurface display;
            wait_key ();
            (* Binairisation  *)
            let ds_mat = Img.binarize m (Img.seuil m) in
            let ds = Matrix.to_img ( ds_mat ) in
            show ds display;
            wait_key ();
            (* Filtre passe bas -> 'image relief' )
            let kern = [|[|1;2;1|];[|2;4;2|];[|1;2;1|]|] in
            for i = 0 to w do
                for j = 0 to h do  
                        let mp = Matrix.submatrix ds_mat i j 3 in
                        let result = Matrix.produit mp kern in
                        Matrix.insert ds_mat result i j 3;
                done     
            done;
            show (Matrix.to_img ds_mat) display;
            wait_key();
	    ( reverse img *) 
	    let ds = Matrix.to_img ( Img.reverse m ) in
	    show ds display;
	    wait_key ();
 	    (* rotate *)
            let ds = Matrix.to_img ( Img.rotate m (Img.hough m)) in
            show ds display;
            wait_key ();
    (* on quitte *)
    exit 0
  end
 
let _ = main ()
