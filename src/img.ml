(* Intensité luminseuse d'un pixel *)
let level (r,g,b) = ( 0.299 *. float_of_int r +. 0.587 *. float_of_int g +. 0.114 *. float_of_int b ) /. 255.0

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
(* Crée une nouvelle surface SDL au même format que l'image donnée *)
let new_img img = 
    let (w, h) = get_dims img in
    Sdlvideo.create_RGB_surface_format img [] w h

(*Calcule le seuil d'une matrice *)
let seuil matrix = 
	let (w,h) = Matrix.get_dims matrix in
	let t = ref 0 in
        let f x = t := !t+x in
        begin
          Matrix.iter f matrix;
	  !t / (w*h)
        end

(*Binarisation d'une matrice *)
let binarize inp out s =
  let f x y =
    if (Matrix.get inp x y) >= s then
      255
    else
      0
  in
  Matrix.modify out f

(*Best inversement d'image EU*)
let reverse input = 
        let (w,h) = Matrix.get_dims input in
                let f x y = 
        begin
           let x_alternate = w - x in
           if (x_alternate >= 0 && x_alternate < w) then
                Matrix.get input x_alternate y
           else 
                0
        end in
  Matrix.init w h f

(*Best rotation sans detection d'angle EU*)
let rotate input angle =
  let (w,h) = Matrix.get_dims input in
  let pi = 2. *. acos(0.) in
  let angle_tmp = pi *. ((0. -. angle) /. 180.) +. pi /. 2. in
  let wf = (float_of_int w) /. 2.0 in
  let hf = (float_of_int h) /. 2.0 in
    let f x y =
        begin
            let i = float_of_int x  in
            let j = float_of_int y  in
            let x2 = int_of_float ((i -. wf) *. (sin angle_tmp) -.
                                   (j -. hf) *. (cos angle_tmp) +. wf) in
            let y2 = int_of_float((i -. wf) *. (cos angle_tmp) +.
                                   (j -. hf) *. (sin angle_tmp) +. hf) in
            if (x2 >= 0 && x2 < w) && (y2 >= 0 && y2 < h) then
                Matrix.get input x2 y2
            else 
                0 
        end in
    Matrix.init w h f

(* matrice est un tableau de pixel *)
let hough matrice =
        (* Declaration des constantes *)
        let pi = acos(-1.) and pi02 = asin(1.) in
        let (w,h) = Matrix.get_dims matrice in
        let diagonal = int_of_float(sqrt(float_of_int(w*w+ h*h))) in
        let matrice_de_vote = Array.make_matrix diagonal ((int_of_float(pi*.100.))+1) 0 in
        (* on declare les variables qu'on va remplire : a savoir teta max et l
* angle le plus vote *)
        let teta_max = ref 0. in
        let vote_max = ref 0 in
        (* Allez les noobs, on va parcourire l'image *)
        for y=0 to h-1 do
           for x=0 to w-1 do
              if (Matrix.get matrice x y == 0) then
                begin
             (* t est l angle qu on fera variee -pi/2 a pi/2 *)
                let t = ref(-.pi02) in
                (* parcour de l'inertvalle d'angle *)
                 while ( !t <= pi02 ) do
                  let droite =int_of_float((float x *. (cos !t))+.(float
                  y *. (sin !t))) in
                       if droite>=0 then (* on remplis le tableau de vote *)
                         begin
                          let teta_i = int_of_float(!t*.100. +. pi02*.100.)in
                           matrice_de_vote.(droite).(teta_i) <-
                                  matrice_de_vote.(droite).(teta_i)+1;
                           if !vote_max < matrice_de_vote.(droite).(teta_i) then
                                  begin
                                  vote_max := matrice_de_vote.(droite).(teta_i);
                                  teta_max := !t;
                                  end;
                         end;
                         t := !t +. 0.01; (* on incremente l'angle de 0.01 *)
                 done;
                end;
           done;
        done;
        (!teta_max)

(* Convolution *)
let get m x y =
  let (width, height) = Matrix.get_dims m in
  if (x < 0) || (x >= width) then 255 else
  if (y < 0) || (y >= height) then 255 else  (* feed borders with white *)
  Matrix.get m x y
 
 
let convolve_get_value matrix kernel divisor offset = fun x y ->
  let sum = ref 0.0 in
 
  for i = -1 to 1 do
    for j = -1 to 1 do
      let c = get matrix (x+i) (y+j) in
      sum := !sum +. kernel.(j+1).(i+1) *. (float c);
    done;
  done;
  !sum /. divisor +. offset
 
 
let level_to_int c =
  truncate c
 
let bounded c =
  max 0 (min c 255)
 
let convolve_value inp out kernel divisor offset = 
  let conv = convolve_get_value inp kernel divisor offset in
  
  let f x y = 
    let level = conv x y in
    let level = level_to_int level in
    bounded level in
  Matrix.modify out f

let sharpen inp out=
    let kernel = [|
        [| -1.; -1.; -1. |];
        [| -1.;  9.; -1. |];
        [| -1.; -1.; -1. |];|] 
    in
    convolve_value inp out kernel 1.0 0.0
    
