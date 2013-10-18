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
let binarize m s =
  let (w,h) = Matrix.get_dims m in
  let f x y =
    if (Matrix.get m x y) >= s then
      255
    else
      0
  in
  Matrix.init w h f

(*Best rotation sans detection d'angle EU*)
let rotate input angle =
    let (w,h) = Matrix.get_dims input in
let wf = (float_of_int w) /. 2.0 in
let hf = (float_of_int h) /. 2.0 in
    let f x y =
        begin
            let i = float_of_int x  in
            let j = float_of_int y  in
            let x2 = int_of_float ((i -. wf) *. (sin angle) +.
                                   (j -. hf) *. (cos angle) +. hf) in
            let y2 = int_of_float((i -. wf) *. (cos angle) -.
                                   (j -. hf) *. (sin angle) +. hf) in
            if (x2 >= 0 && x2 < w) && (y2 >= 0 && y2 < h) then
                Matrix.get input x2 y2
            else
                0
        end in
    Matrix.init w h f
