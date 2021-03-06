(* fabrique une matrice de largeur w et de hauteur h 
 * en applicant la fonction f x y *)

let init w h f =
  let tab = Bigarray.Array2.create Bigarray.int Bigarray.c_layout w h in
  for i=0 to w -1 do
    for j=0 to h - 1 do
      tab.{i,j} <- f i j
    done
  done;
  tab

let empty w h =
    let f x y = 0 in
    init w h f

(* retourne la largeur d'une matrice *)
let width m = 
  Bigarray.Array2.dim1 m

(* retourne la hauteur d'une matrice (Simple binding de Array.length) *)
let height m =
  Bigarray.Array2.dim2 m

let get_dims m =
  (width m, height m)

(* Même principe que init mais sur une matrice existante *)
let modify m f =
  let (w,h) = get_dims m in
  for i=0 to w-1 do
    for j=0 to h-1 do
      m.{i,j} <- f i j
    done
  done

(* Met l'élément v à l'emplacement x y ) *)
let set m x y v =
  m.{x,y} <- v

let get m x y =
  m.{x,y}

(* Extrait une ligne d'une matrice *)
let extract m line =
  let f x = get m x line in
  Array.init (width m) f

let iter f m =
  let (w,h) = get_dims m in
  for i=0 to w - 1 do
    for j=0 to h - 1 do
      f m.{i,j}
    done
  done

let iteri f m =
  let (w,h) = get_dims m in
  for i=0 to w - 1 do
    for j=0 to h - 1 do
      f i j m.{i,j}
    done
  done

(* Transforme une image noir et blanc en une matrice *)
let from_img img =
  let f x y = let (c,_,_) =
      Sdlvideo.get_pixel_color img x y in
      c 
  in
  let (w,h) = 
    ((Sdlvideo.surface_info img).Sdlvideo.w,
    (Sdlvideo.surface_info img).Sdlvideo.h) in
  init w h f

(* Transforme une matrice en image noir et blanc *)
let to_img m =
  let img = Sdlvideo.create_RGB_surface [] (width m) (height m) 24
  Int32.zero Int32.zero Int32.zero Int32.zero in
  let copy x y c =
    Sdlvideo.put_pixel_color img x y (c,c,c) in
  iteri copy m;
  img
(*
(* Produit matriciel entre x et y *)
let mult x y =
  let n = height x in
  let f i j =
    let r = ref 0. in
    for k=0 to n-1 do
      r := !r +. ((get x i k) *. (get y k j))
    done;
    !r in
  init (width x) (height y) f

(* Addition avec une constante *)
let plus m1 m2 =
  let w = max (width m1) (width m2) in
  let h = max (height m1) (height m2) in
  let g m x y = if x < 0 || y < 0 || x >= width m || y >= height m then
    0.
  else
    get m x y in
  let f x y = (g m1 x y) +. (g m2 x y) in
  init w h f
  *)  

(* Transforme un tableau en matrice (sur une colonne) *)
let to_column t =
  let f _ y =
    t.(y) in
  init 1 (Array.length t) f

(* Transforme un tableau en matrice (sur une ligne) *)
let to_line t =
  let f x _ =
    t.(x) in
  init (Array.length t) 1 f

(* Renvoi la x-ième ligne de la matrice *)
let get_line m y =
  Bigarray.Array2.slice_left m y

(* Renvoi la x-ième colonne de la matrice *)
let get_column m x =
  Bigarray.Array2.slice_right m x

(* Transforme les colonnes en ligne et inversement *)
let transpose m =
  let f x y = get m y x in
  let (w,h) = get_dims m in
  init h w f

(* Produit de convalescence de la matrice m1 et du noyau kern *)
let produit m1 kern =
    let w = width m1 in
    let f x y = 0 in
    let nmatrix = init w w f in
    for i=0 to w-1 do
        for j=0 to w-1 do
            for k=0 to w-1 do
                set nmatrix i j (((get m1 i k)*(get kern k j)+(get nmatrix i j)));
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
    ()

let get_bigarray img x y = 
	let _, arr,_,_ = img in
	Bigarray.Array2.get arr x y

let matrix2image matrix img =
  for i = 0 to width matrix -1 do
    for j = 0 to height matrix-1 do
        if (Bigarray.Array2.get matrix i j = 255) then
            Sdlvideo.put_pixel_color img i j (255,255,255)
        else
            Sdlvideo.put_pixel_color img i j (0,0,0)
    done;
  done;
    img

let image2emptymatrix pictureval =
            let width= (Sdlvideo.surface_info pictureval).Sdlvideo.w and
                height = (Sdlvideo.surface_info pictureval).Sdlvideo.h in
            let f x y = 255 in
            let matrice1 = init width height f in
            matrice1

let emptymatrix2image pict2 =
 let emptymatrix = image2emptymatrix (pict2) in
  matrix2image emptymatrix pict2

let image2matrix matrix img =
  let width = ((Sdlvideo.surface_info img).Sdlvideo.w) and
    height = ((Sdlvideo.surface_info img).Sdlvideo.h) in
      for i = 0 to width -1 do
    for j = 0 to height -1 do
      if (Sdlvideo.get_pixel_color img i j) = (255,255,255) then
        set matrix i j 255
          else
        set matrix i j 0
        done;
      done;
        matrix
