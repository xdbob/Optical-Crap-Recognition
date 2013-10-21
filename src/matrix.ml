(* fabrique une matrice de largeur w et de hauteur h 
 * en applicant la fonction f x y *)

let init w h f =
  let f2 y x = f x y in
  let fct x = Array.init w ( f2 x ) in
  Array.init h fct

(* retourne la largeur d'une matrice *)
let width m = 
  if Array.length m = 0 then
    0
  else
    Array.length m.(0)

(* retourne la hauteur d'une matrice (Simple binding de Array.length) *)
let height m =
  Array.length m

let get_dims m =
  (width m, height m)

(* Met l'élément v à l'emplacement x y ) *)
let set m x y v =
  m.(y).(x) <- v

let get m x y =
  m.(y).(x)

let iter f m =
  let parcours m2 = Array.iter f m2 in
  Array.iter parcours m

let iteri f m =
  let f2 y x = f x y in
  let parcours y m2 = Array.iteri (f2 y) m2 in
  Array.iteri parcours m

(* Transforme une image noir et blanc en une matrice *)
let from_img img =
  let f x y = let (c,_,_) =
      Sdlvideo.get_pixel_color img x y in
      c in
  let (w,h) = 
    ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h) in
  init w h f

(* Transforme une matrice en image noir et blanc *)
let to_img m =
  let img = Sdlvideo.create_RGB_surface [] (width m) (height m) 24
  Int32.zero Int32.zero Int32.zero Int32.zero in
  let copy x y c =
    Sdlvideo.put_pixel_color img x y (c,c,c) in
  iteri copy m;
  img


(* Produit de convalescence de la matrice m1 et du noyau kern *)
let produit m1 kern =
    let w = width m1 in
    let f x y = 0 in
    let nmatrix = init w w f in
    for i=0 to w-1 do
        for j=0 to w-1 do
            for k=0 to w-1 do
                set nmatrix i j ((get m1 i k)*(get kern k j)+(get nmatrix i j));
           done
       done
    done;
    nmatrix

(* Générer une matrice 3*3 ou 5*5 ou n*n à partir du centre x y d'une matrice mat *) 
let submatrix mat x y size = 
    let blank x y = 255 in
    let new_matrix = init size size blank in
    for i = x-(size/2) to x+(size/2) do
        for j = y-(size/2) to y+(size/2) do
            match (i,j) with
                | (n,_) when (n < 0 || n > (height mat)-1) -> ()
                | (_,n) when (n < 0 || n > (width mat)-1) -> ()
                | (_,_) -> set new_matrix ((i-x)+size/2) ((j-y)+size/2) (get mat i j ); 
        done;
    done;
    new_matrix

let insert big small x y size =
    for i = x-(size/2) to x+(size/2) do
        for j = y-(size/2) to y+(size/2) do
            match (i,j) with
                | (n,_) when (n < 0 || n > (height big)-1) -> ()
                | (_,n) when (n < 0 || n > (width big)-1) -> ()
                | (_,_) -> set big i j (get small ((i-x)+size/2) ((j-y)+size/2))
        done;
    done;
    big


