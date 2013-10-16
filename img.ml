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