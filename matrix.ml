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

(* Met l'élément v à l'emplacement x y ) *)
let set m x y v =
  m.(y).(x) <- v

let iter f m =
  let parcours m2 = Array.iter f m2 in
  Array.iter parcours m

let iteri f m =
  let f2 y x = f x y in
  let parcours y m2 = Array.iteri (f2 y) m2 in
  Array.iteri parcours m

(* La fonction ne compile pas

(* Produit des matrices m1 et m2 *)
let produit m1 m2 =
  let w = width m1 in
  let f x y =
    let r = ref 0 in
    for i=0 to w do
      r := !r + m1.(y).(i) * m2.(i).(x)
    done
    !r
     in
    init w w f

*)
