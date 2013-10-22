module Layer =
  struct
    (* Définis une couche du réseau de neurones *)
    type t = {
      weight:(float array array); (* x = id y = weigh *)
      bias:(float array); (* bias.(id neurone) *)
      s:(float array); (* s.(id) sensibilibité *)
      o:(float array); (* o.(id) sortie *)
      f:(float->float); (* fonction d'activation *)
      f':(float->float); (* dérivée de la fonction d'activation *)
      g:float; (* Pourcentage d'apprentissage *)
    }
    
    (* Fonction de transfers *)
    let trans x = 1./.(1.+.exp(-.x))
    let trans' x = let a = exp(-.x) in a/.(1.+.a)**2.

    (* Fabrique une couche du réseau de neurones *)
    (* inputs = nombre d'inputs de la couche
     * n = nombre de neurones de la couche
     * g = pourcentage d'aquisition du réseau
     * f = fonction de transfert
     * f' = dérivée de la fonction de transfert *)
    let make inputs n g f f' =
      {
        weight=Matrix.init n inputs (fun _ _ -> (Random.float 2.)-.1.);
        bias=Array.init n (fun _ -> (Random.float 2.)-.1.);
        s=Array.make n 0.;
        o=Array.make n 0.;
        f=f;
        f'=f';
        g=g;
      }
    
    (* Création de la sortie de la couche *)
    let eval la inp =
      let nb = Matrix.width la.weight in
      for i=0 to nb - 1 do
        let x = ref 0. in
        let f i y = x := !x +. ( y *. (inp.(i)) ) in
        Array.iteri f (Matrix.get_column la.weight i);
        la.o.(i) <- !x
      done;
      Array.init (Array.length la.o) (fun x -> la.f la.o.(x))

    (* Met à jour la couche avec l'entrée *)
    let update la inp =
      let tmp = Matrix.mult (Matrix.to_column la.s) (Matrix.to_line inp) in
      let tmp2 = Matrix.plus (Matrix.init (Matrix.width tmp) (Matrix.height tmp)
      (fun x y -> (Matrix.get tmp x y) *. la.g)) la.weight in
      Matrix.iteri (fun x y -> Matrix.set la.weight x y) tmp2;
      let tmp = Array.init (Array.length la.s) (fun x -> la.g *. la.s.(x)) in
      for i=0 to (Array.length la.bias) - 1 do
        la.bias.(i) <- tmp.(i)
      done
     
    (* Rétropropagation pour le dernier niveau *)
    let last_retropropagate la d =
      let a = Array.init (Array.length la.o) (fun x -> la.f la.o.(x)) in
      let e = Array.init (Array.length a) (fun x -> d.(x) -. a.(x)) in
      for i=0 to (Array.length e) - 1 do
        la.s.(i) <- (la.f' a.(i)) *. e.(i)
      done

    (* Rétropropagation sur les autres niveaux *)
    let retropropagate la la' = 
      let nb = Matrix.width la.weight in
      let tmp = Matrix.transpose la'.weight in
      let fct i = 
         let x = ref 0. in
         let f i y = x := !x +. ( y *. la'.s.(i) ) in
         Array.iteri f (Matrix.get_column tmp i);
         !x  in
      let v = Array.init (nb - 1) fct in
      for i=0 to (Array.length la.s) - 1 do
        la.s.(i) <- (la.f' la.o.(i)) *. v.(i)
      done
  end
