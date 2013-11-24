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
        la.o.(i) <- !x -. la.bias.(i)
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
      let v = Array.init nb fct in
      for i=0 to (Array.length la.s) - 1 do
        la.s.(i) <- (la.f' la.o.(i)) *. v.(i)
      done
  end

  module Network =
    struct

      (* Fabrique un réseau avec [ne] entrées et n niveaux
       * où le niveau i à [si] neurones *)
      let make ne g t =
        let f i = Layer.make (if i=0 then ne else t.(i-1)) t.(i)
        g Layer.trans Layer.trans' in
        Array.init (Array.length t) f

      (* Sortie du réseau [nt] avec l'entrée [p] *)
      let eval nt p = Array.fold_left (fun x la -> Layer.eval la x) p nt
      
      (* Apprentissage du réseau [nt] qui apprends ce que [p] devrait retourner [d] *)
      let learn nt p d =
        let len = Array.length nt in
        ignore(eval nt p);
        Layer.last_retropropagate nt.(len - 1) d;
        for i = len - 2 downto 0 do
          Layer.retropropagate nt.(i) nt.(i+1);
        done;
        Layer.update nt.(0) p;
        for i=1 to len - 1 do
          let l = nt.(i-1) in
          let a = Array.init (Array.length l.Layer.o) (fun x -> l.Layer.f l.Layer.o.(x)) in
          Layer.update nt.(i) a
        done

      (* Entraine le réseau [nt] avec [base], [n] fois *)
      let training nt base n =
        for i=0 to n-1 do
          for j=0 to (Array.length base) - 1 do
            learn nt (fst base.(j)) (snd base.(j))
          done
        done

      let error_rate nt d =
        let len = Array.length nt in
        let l = nt.(len-1) in
        let d0 = Array.init (Array.length l.Layer.o) (fun x -> l.Layer.f l.Layer.o.(x)) in
        let ds = Array.init (Array.length d0) (fun x -> d.(x) -. d0.(x)) in
        let x = ref 0. in
        Array.iter (fun y -> x := !x +. y) ds;
        sqrt (!x)

      
      let set_of_bool = Array.map (function true -> 0.95 | false -> 0.05)
      let bool_of_set = Array.map ((<=) 0.5)
      let base_of_bool = Array.map set_of_bool

      let evalb nt p = bool_of_set (eval nt (set_of_bool p))
      let learnb nt p d = learn nt (set_of_bool p) (set_of_bool d)
      let trainingb nt base n =
        let b = Array.map (fun (i,j) -> set_of_bool i,set_of_bool j) base in
        training nt b n
    end
