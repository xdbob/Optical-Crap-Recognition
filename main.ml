(* Intensité luminseuse d'un pixel *)
let level (r,g,b) = ( 0.3 *. float_of_int r +. 0.59 *. float_of_int g +. 0.11 *. float_of_int b ) /. 255.0

(* Passage d'un pixel en niveau de gris *)
let color2grey x = let z = int_of_float ( ( level x ) *. 255.0 ) in (z,z,z)

(* Dimentions d'une image *)
let get_dims img =
        ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* Passage d'une image en niveau de gris (avec parcours) *)
let image2grey input output = let (w,h) = get_dims input in
        begin
                for i = 0 to w do
                        for j = 0 to h do
                                let c = Sdlvideo.get_pixel_color input i j in
                                let n = color2grey c in
                                Sdlvideo.put_pixel_color output i j n
                        done
                done
        end
(*Calcule le seuil d'une image*)
let seuil img = 
	let (w,h) = get_dims img in
	let t = ref 0 in
	for i = 0 to w do
		for j = 0 to h do
			let (g,_,_) = Sdlvideo.get_pixel_color img i j in
			t := !t+g
		done
	done;
	!t / (w*h)
(*Binarisation d'une image*)
let binarize img dst s = 
	let (w,h) = get_dims img in
    for i = 0 to w do
        for j = 0 to h do
            let (g,_,_) = Sdlvideo.get_pixel_color img i j in
			let c = if g >= s then 255 else 0 in
			Sdlvideo.put_pixel_color dst i j (c, c, c)
        done
    done

 
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
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
        (* on affiche l'image *)
        show img display;
        (* on attend une touche *)
        wait_key ();
        let newSurface = Sdlvideo.create_RGB_surface_format img [] w h in
	
                image2grey img newSurface;
                show newSurface display;
                wait_key ();
	let bin = Sdlvideo.create_RGB_surface_format img [] w h in
        let s = seuil img in
	binarize img bin s;
	show bin display;
	wait_key ();

        (* on quitte *)
        exit 0
  end
 
let _ = main ()
