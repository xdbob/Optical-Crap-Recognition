let clicked msg () =
  print_endline msg;
  flush stdout

let delete_event ev =
 GMain.Main.quit ();
 false

let main () =
  let window = 
	GWindow.window ~title:"Best interface graph EU" ~border_width:150 () in
  window#event#connect#delete ~callback:delete_event;
  let box1 = GPack.hbox ~packing:window#add () in

  let button = GButton.button ~label:"Button 1 qui affiche 'lol' dans la console" ~packing:box1#pack () in
  button#connect#clicked ~callback:(clicked "lol");
  
  let button = GButton.button ~label:"Button 2 qui affiche 'lul' dans la console" ~packing:box1#pack () in
  button#connect#clicked ~callback:(wait_key ());

  let button = GButton.button ~label:"Quit" ~packing:box1#pack () in
  button#connect#clicked ~callback:window#destroy;
  
  window#show ();
  GMain.Main.main ()

let _ = main ()
