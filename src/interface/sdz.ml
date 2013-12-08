let _ = GMain.init ()

let window = GWindow.window 
  (*~height:entrezunnombre pour la hauteur mais au pire, osef, c'est redimenstionnable*)
  ~focus_on_map:false
  ~decorated:true
  ~width:600 
  ~position: `CENTER
  ~resizable:true
  ~title:"Optical Crap Recognition's interface" ()

let vbox = GPack.vbox 
  ~spacing:10
  ~border_width:10
  ~packing:window#add ()

let table = GPack.table
  ~row_spacings:5
  ~col_spacings:5
  ~homogeneous:true
  ~packing:scroll#add_with_viewport ()

let bbox = GPack.button_box `HORIZONTAL
  ~layout:`EDGE
  ~packing:(vbox#pack ~expand:false) ()

let help_message () = print_endline "Cliquez sur \"Quitter\" pour quitter"

let help = 
  let button = GButton.button 
    ~stock:`HELP
    ~packing:bbox#add () in
  button#connect#clicked ~callback:help_message;
  button

let quit = 
  let button = GButton.button
    ~stock:`QUIT
    ~packing:bbox#add () in
  button#connect#clicked ~callback:GMain.quit;
  button

let symbols =
  Array.concat [
    (* Lettres grecques minuscules. *)
    Array.init  25 (fun i -> Glib.Utf8.from_unichar (i +  945));
    (* Divers symboles mathématiques. *)
    Array.init 256 (fun i -> Glib.Utf8.from_unichar (i + 8704));
  ]

(* Le conteneur GPack.table (GtkTable) est rempli : chaque case reçoit un bouton
 * contenant un symbole du tableau < symbols > défini ci-dessus. Le symbole est
 * inséré dans une étiquette (GtkLabel) pour pouvoir utiliser les balises Pango
 * (notamment <big> et </big> qui augmentent la taille du texte). *)
let init_table () =
  Array.iteri (fun i sym ->
    let button = GButton.button 
      ~relief:`NONE 
      ~packing:(table#attach ~left:(i mod 10) ~top:(i / 10)) ()
    and markup = Printf.sprintf "<big>%s</big>" sym in
    ignore (GMisc.label ~markup ~packing:button#add ())
  ) symbols

let window =  GWindow.window
~title:"OCRaml v2.0"
          ~position:`CENTER
          ~resizable:false
          ~width:1200
          ~height:650
          ~border_width:10 () in
        ignore(window#connect#destroy ~callback:GMain.Main.quit);

let _ =
  init_table ();
  window#connect#destroy ~callback:GMain.quit;
  window#show ();
  GMain.main ()
