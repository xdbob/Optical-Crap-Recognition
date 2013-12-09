let _ = GMain.init ()

(*----------- RANDOM AUTHORS ------------- *)
let randomliste =    
      let zero = "Fabien Gregoire" in
      let un = "Valentin Baudry" in
      let deux = "Maxime Orefice" in
      let trois = "Etienne Brouzes" in
      Random.self_init();
      match (Random.int 4) with
      |0 -> [zero; deux; un; trois]
      |1 -> [un; zero; trois ; deux]
      |2 -> [deux; trois; zero; un]
      |_ -> [trois; un; deux ; zero]

(* ---------- ABOUT BUTTON -------------- *)
let show () =
  let dialog = 
    GWindow.about_dialog 
      ~name:"OCRaml" 
      ~version:"v2.0"
      ~authors: randomliste
      ~copyright:"Copyright : IllégiTeam"
      ~license:("OCRaml est un logiciel OCR gratuit,"
    		^" réalisé par quatre membres de l'EPITA")
      ~icon_name:"logo.png"
      ~website:"http://OCRaml.zxq.net"
      ~website_label:"OCRaml.zxq.net"
      ~position:`CENTER_ON_PARENT
      ~destroy_with_parent:true () in
    dialog#set_logo(GdkPixbuf.from_file "logo.png");
    ignore (dialog#connect#response 
              ~callback:(fun _ -> dialog#show ()));
    ignore(dialog#run ());
    dialog#misc#hide ()

let img_path = ref "init"

let step1_clicked = ref false 

(* string textbox *)
let texte = ref "" 


(*--------------- GRAPHIC FONCTIONS ------------------ *)

let main() =
Sdl.init [`EVERYTHING];
Sdlevent.enable_events Sdlevent.all_events_mask;
(* Creation de la fenetre *)
let window =  GWindow.window 
~title:"Optical Crap Recognition"
	  ~position:`CENTER
	  ~resizable:false
	  ~width:1200
	  ~height:650
	  ~border_width:10 () in 
  	ignore(window#connect#destroy ~callback:GMain.Main.quit);
	
(*------------------ INTERFACE ----------------------*)

let big_box = GPack.hbox
    ~border_width:0
    ~packing:window#add() in

let frame = GBin.frame
    ~label:"Image"
    ~width:650
    ~border_width:5
    ~packing:big_box#add() in

(* ------------------- IMAGE -----------------------*)

let picture_area  = GMisc.image    
    ~file:"init.png"
    ~packing:frame#add () in

let right_box = GPack.vbox
    ~width:600
    ~border_width:5
    ~packing:big_box#add() in


let box_2D = GPack.vbox
   ~height:130
   ~packing:right_box#add() in

 
(* ----------------------  MENU  ----------------------- *)
 
let frame_menu = GBin.frame
  ~label:"Menu"
  ~height:50
  ~packing:box_2D#add() in

let hbox = GPack.hbox
  ~border_width:5
  ~packing:frame_menu#add() in

let menu_box = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~spacing:10
  ~packing:(hbox#pack ~expand:false) () in

let copy_ref path = 
	img_path := path in

(* OPEN FILE FILTER *)
let filter () = GFile.filter
	~name:"Pictures"
	~patterns:[ "*.bmp" ; "*.jpg" ; "*.png"] () in

(* -------------------- OCR FONCTIONS ---------------------- *)
let sdl_init () =
begin
    		Sdl.init [`EVERYTHING];
    		Sdlevent.enable_events Sdlevent.all_events_mask;
	end 
in

let imageapercu () = 
  let pixbufvide = GdkPixbuf.create
                     ~width:600
                     ~height:600
                      () in
  let pixbuf = GdkPixbuf.from_file("TMP/image_tmp.bmp") in
  GdkPixbuf.scale 
           ~dest: pixbufvide
           ~scale_x: (600. /. (float (GdkPixbuf.get_width pixbuf)))
           ~scale_y: (600. /. (float (GdkPixbuf.get_height pixbuf)))
            pixbuf;
  GdkPixbuf.save 
            ~filename:("TMP/image_apercu.bmp") 
            ~typ:"bmp"
            pixbufvide;
in

let imagecompression () = 
      sdl_init ();
      let pixbuf = GdkPixbuf.from_file(!img_path) in
      let w = GdkPixbuf.get_width pixbuf in
      let h = GdkPixbuf.get_height pixbuf in
      let maximus = max w h in
      let diviseur = ref 1 in
if (maximus>2000) then
	if (maximus<4000) then
        diviseur := 2;        
	if (maximus>4000) && (maximus<6000) then
        diviseur := 3;
        if (maximus>6000) then
        diviseur := 4;

let pixbufvide = GdkPixbuf.create
                     ~width:(w/(!diviseur))
                     ~height:(h/(!diviseur))
                      () in      
GdkPixbuf.scale 
           ~dest: pixbufvide
            ~width:(w/(!diviseur))
            ~height:(h/(!diviseur))
            pixbuf;  

GdkPixbuf.save 
            ~filename:("TMP/image_cp.bmp") 
            ~typ:"bmp"
            pixbufvide;

in     

let greylevel () =
    begin
      sdl_init ();
      let img1 = Sdlloader.load_image (!img_path) in
      let img2 = Img.new_img img1 in
      Img.image2grey img1 img2;
      Sdlvideo.save_BMP img2 "TMP/image_tmp.bmp";
    end;
  imageapercu (); 
in

let noiseelimination () =
    begin
      sdl_init ();
      let img1 = Matrix.from_img (Sdlloader.load_image (!img_path)) in
      Img.sharpen img1 img1;
      Sdlvideo.save_BMP (Matrix.to_img img1) "TMP/image_tmp.bmp";
    end;
  imageapercu ();
in

let binarisation () =
    begin
      sdl_init ();
      let m = Matrix.from_img (Sdlloader.load_image (!img_path)) in
      Img.binarize m m (Img.seuil m);
      Sdlvideo.save_BMP (Matrix.to_img m) "TMP/image_tmp.bmp";
    end;
  imageapercu ();
 in

let linedetect () =
    begin
      sdl_init ();
      let img1 = Sdlloader.load_image (!img_path) in
      let img2 = Sdlloader.load_image (!img_path) in
      let img3 = Matrix.emptymatrix2image img2 in
      let mat1 = Matrix.image2emptymatrix (img2) in
      let mat2 = Matrix.image2matrix mat1 img1 in 
      Segmentation.search_line (mat2);
      Segmentation.call_column (mat2);
      Sdlvideo.save_BMP (Matrix.matrix2image mat2 img3) "TMP/image_tmp.bmp";
    end;
  imageapercu ();
  picture_area#set_file "TMP/image_apercu.bmp";
  (*copy_ref "TMP/image_tmp.bmp" () *)
  in


let rotationcpr () = 
(* Calcul de l'angle sur l'image compressée *)
   begin
      sdl_init ();
      imagecompression();
      copy_ref "TMP/image_cp.bmp";
      let mat = Matrix.from_img (Sdlloader.load_image (!img_path)) in
      let angle = Img.hough mat in
(* On applique la rotation sur l'image *)
      copy_ref "TMP/image_tmp.bmp";
      let rot = Img.rotate mat angle in
      Sdlvideo.save_BMP (Matrix.to_img rot) "TMP/image_tmp.bmp";
   end; 
  imageapercu ();
  picture_area#set_file "TMP/image_apercu.bmp";
  copy_ref "TMP/image_tmp.bmp" in

  
(* -------------------  PRETREATMENT  ---------------------- *)

let pre_processing_frame = GBin.frame
  ~label:"Prétraitement"
  ~height:50
  ~packing:box_2D#add() in

let hbox = GPack.hbox
  ~border_width:0
  ~packing:pre_processing_frame#add() in

let box1 = GPack.vbox
  ~border_width:0
  ~packing:hbox#add() in

let box2 = GPack.vbox
  ~border_width:0
  ~packing:hbox#add() in

let bbox1 = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~height: 50
  ~spacing:10
  ~packing:(box1#pack ~expand:false) () in

let greylevel_button = GButton.button 
  ~label:"Niveau de gris"
  ~packing:bbox1#add () in
   ignore (greylevel_button#connect#clicked ~callback:(greylevel););

let noiseel_button = GButton.button 
  ~label:"Elimination du bruit"
  ~packing:bbox1#add () in
   ignore (noiseel_button#connect#clicked ~callback:(noiseelimination););

let bbox2 = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~height: 50
  ~spacing:10
  ~packing:(box2#pack ~expand:false) () in

let binarisation_button = GButton.button
  ~label:"Binarisation" 
  ~packing:bbox2#add () in
   ignore(binarisation_button#connect#clicked ~callback:(binarisation););

let rotate_button = GButton.button
  ~label:"Rotation" 
  ~packing:bbox2#add () in
   ignore(rotate_button#connect#clicked ~callback:(rotationcpr););

(* ---------------------- TEXT RECONISATION ---------------------- *)
 
let box_3D = GPack.vbox
   ~height:125
   ~packing:right_box#add() in

let reconisation_frame = GBin.frame
  ~label:"Reconnaissance du texte"
  ~height:75
  ~packing:box_3D#add() in

let hbox2 = GPack.hbox
  ~border_width:5
  ~packing:reconisation_frame#add() in

let box1 = GPack.vbox
  ~border_width:0
  ~packing:hbox2#add() in

let bbox1 = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~height: 50
  ~spacing:10
  ~packing:(box1#pack ~expand:false) () in

let linedetection_button = GButton.button 
  ~label:"Détection des caractères"
  ~packing:bbox1#add () in
   ignore (linedetection_button#connect#clicked ~callback:(linedetect););

(*---------------TEXT TREATMENT--------------- *)

let frame_textetraitment = GBin.frame
  ~width:50
  ~label:"Traitement du texte"
  ~packing:box_3D#add() in

let hboxtraitement = GPack.hbox
  ~border_width:5
  ~packing:frame_textetraitment#add() in

let boxtraitement = GPack.vbox
  ~border_width:0
  ~packing:hboxtraitement#add() in

let boxtreat = GPack.button_box `HORIZONTAL
  ~layout:`SPREAD
  ~height: 50
  ~spacing:10
  ~packing:(boxtraitement#pack ~expand:false) () in

(* ---------------------- TEXT BOX ----------------------- *)

let box_3D2 = GPack.hbox
  ~border_width:0
  ~height:300
  ~packing:right_box#add() in

let frame_texte = GBin.frame
  ~width:300
  ~label:"Texte"
  ~packing:box_3D2#add() in

let textbox = GText.view
  ~wrap_mode:`WORD 
  ~packing:frame_texte#add () in  
textbox#misc#modify_font_by_name "Monospace 10";
let stringlol = 
"     .d88888b.   .d8888b.  8888888b.                         888 
    d88P   Y88b d88P  Y88b 888   Y88b                        888 
    888     888 888    888 888    888                        888 
    888     888 888        888   d88P  8888b.  88888b.d88b.  888 
    888     888 888        8888888P        88b 888  888  88b 888 
    888     888 888    888 888 T88b   .d888888 888  888  888 888 
    Y88b. .d88P Y88b  d88P 888  T88b  888  888 888  888  888 888 
      Y88888P     Y8888P   888   T88b  Y888888 888  888  888 888


               888     888       .d8888b.       .d8888b.  
               888     888      d88P  Y88b     d88P  Y88b 
               888     888             888     888    888 
               Y88b   d88P           .d88P     888    888 
                Y88b d88P        .od888P       888    888 
                 Y88o88P        d88P           888    888 
                  Y888P         888        d8b Y88b  d88P 
                   Y8P          888888888  Y8P   Y8888P      "

 in textbox#buffer#set_text(stringlol);

(*------------------EXTRACTION BUTTON ------------------- *)
let extraction () =
texte := "";

let lst = ref [] in

let to_couple_x liste =
  let rec couple_x = function
    | [] -> []
    | e1::e2::l -> (e1,e2) :: (couple_x l)
    | _ -> []
  in couple_x (List.rev liste) in


let moy_char liste =
  let rec medium_space liste_x = match liste_x with
    |[] -> 0
    |(e1,e2)::l -> (e2 - e1) + medium_space l
  in ((medium_space (liste)) / (List.length liste)) in


let calcul_dist_char liste =
  let rec calcul_dist liste accu = match liste with
    |[] -> failwith "Liste paire!"
    |_::[] -> (List.rev (accu))
    |(e1,e2)::(e3,e4)::l -> calcul_dist ((e3,e4)::l) ((e3-e2)::accu)
  in calcul_dist liste [] in


let is_space listeX =
let new_listX = to_couple_x listeX in
  let moyenne_char = moy_char new_listX in
  let distance_char = calcul_dist_char new_listX in
  let vect = Array.make (List.length distance_char) false in
    for i = 0 to (List.length (distance_char) - 1) do
      if (((List.nth distance_char i) > (moyenne_char/2)) || ((List.nth distance_char i) < 0)) then
        vect.(i) <- true
    done;
    vect in
	
let vect = is_space (!lst) in
  for i = 0 to Array.length vect - 1 do
    texte := (!texte)^"x";
    if vect.(i) then
    texte := (!texte)^" ";
  done;
    texte:= (!texte)^"x";

textbox#buffer#set_text(!texte) in

let extraction_button = GButton.button 
  ~label:"Extraction"
  ~packing:bbox1#add () in
   ignore (extraction_button#connect#clicked ~callback:(extraction););

(* -----------------CORRECTION BUTTON ------------------- *)

let correction () = 
if (GtkSpell.is_attached(textbox)) then 
    GtkSpell.detach(textbox)
else GtkSpell.attach(textbox) in

let correction_button = GButton.button 
  ~label:"Correction"
  ~packing:boxtreat#add () in ignore (correction_button#connect#clicked
   ~callback:(correction));
  ignore(GMisc.image ~stock:`SPELL_CHECK
   ~packing:correction_button#set_image ());


(* -------------------- SAVE BUTTON ---------------------- *)

let savetext () = 
  let och = open_out "texte.txt" in
  output_string och (textbox#buffer#get_text ());
  close_out och  in

let save_button = GButton.button 
  ~label:"Enregistrer"
  ~stock: `SAVE
  ~packing:boxtreat#add () in ignore (save_button#connect#clicked
   ~callback:(savetext));
  ignore(GMisc.image ~stock:`SAVE ~packing:save_button#set_image ());

(* -------------------- OPEN BUTTON ---------------------- *)
let touch_picture btn () =
  Gaux.may copy_ref btn#filename;
  let image1 = Sdlloader.load_image (!img_path) in
  Sdlvideo.save_BMP image1 "TMP/image_tmp.bmp";
  imageapercu(); 
  print_string "toto\n";
flush_all(); 
  picture_area#set_file ("TMP/image_apercu.bmp");
  step1_clicked := false; 
in 

let open_b = GFile.chooser_button
		~action: `OPEN
		~width:100
		~height:25
		~packing:(menu_box#add) () in
  open_b#set_filter (filter ());
  ignore(open_b#connect#selection_changed (touch_picture open_b););
let reset () = 
	picture_area#set_file "init.png";
        copy_ref "init.png";
	step1_clicked:= false;
	img_path := "" in

let reset_button = GButton.button 
  ~label:"Reset" 
  ~stock:`CLEAR
  ~packing:menu_box#add () in ignore (reset_button#connect#clicked 
  ~callback:(reset));
  ignore(GMisc.image ~stock:`CLEAR ~packing:reset_button#set_image ());


let about_button = GButton.button 
  ~label:"About" 
  ~stock: `ABOUT
  ~packing:menu_box#add ()in ignore (about_button#connect#clicked
  ~callback:(show));
  ignore(GMisc.image ~stock:`ABOUT ~packing:about_button#set_image ());


let close_button = GButton.button 
  ~label:"Fermer" 
  ~stock: `CLOSE
  ~packing:menu_box#add () in ignore (close_button#connect#clicked
  ~callback:(GMain.quit));
  ignore(GMisc.image ~stock:`CLOSE ~packing:close_button#set_image ());


window#show();
GMain.Main.main ()


let _ = Printexc.print main ();
