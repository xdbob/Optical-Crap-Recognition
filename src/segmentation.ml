(** Segmentation class **)

let detect_line matrix = 
  let vect = Array.make (Matrix.height matrix) false in
    for j = 0 to (Matrix.height matrix) - 1 do
      for i = 0 to (Matrix.width matrix) - 1 do
        if (Matrix.get matrix i j = 0) then
          vect.(j) <- true
      done;
    done;
    vect


let make_line matrix j = 
  for i = 0 to (Matrix.width matrix) - 1 do
    Matrix.set matrix i j 1
  done

let make_column matrix i y1 y2 = 
  for j = y1 to y2 do 
    Matrix.set matrix i j 1
  done

let mini a b = 
  if a > b then
    b   
  else
    a  

let i_of_f x = 
  let sup10 = int_of_float x in
  let inf10 = x -. float_of_int(sup10) in
    if inf10 > 0.50 then
      (sup10 +1) 
else
  (sup10)

let list_y = ref []

let list_x = ref []

let x1 = ref 0

let x2 = ref 0

let list_four = ref []

let search_line matrix =
  let vect = detect_line matrix in
    list_y := [];
    for j = 0 to Array.length vect - 2 do
      if(vect.(j+1) && not vect.(j)) then
        begin
          make_line matrix (j);
          list_y := j :: !list_y	
        end
      else if (not vect.(j+1) && vect.(j)) then
        begin        
          make_line matrix (j);
          list_y := j :: !list_y
        end
    done;
    list_x := [];
    list_y := List.rev (!list_y)

let detect_column matrix y1 y2 =
  let vect = Array.make (Matrix.width matrix) false in
    for i = 0 to (Matrix.width matrix)-1 do
      for j = (y1+1) to (y2-1) do
        if (Matrix.get matrix i j = 0) then
          vect.(i) <- true
      done 
    done;
    vect

let search_column matrix y1 y2 =
  x1 := 0;
  x2 := 0; 
  let vect = detect_column matrix y1 y2 in
    for i = 0 to (Array.length (vect) - 2) do
      if (vect.(i+1) && not vect.(i)) then
        begin
          make_column matrix i y1 y2;
          list_x := i :: !list_x;
          x1 := i
        end 
      else if (vect.(i) && not vect.(i+1)) then
        begin
          make_column matrix i y1 y2;
          list_x := i :: !list_x;
          x2 := i
        end;
      if ((!x1 <> 0) && (!x2 <> 0)) then
        begin
          list_four := (!x1,!x2,y1,y2):: !list_four;
          x1 := 0;
          x2 := 0; 
        end;
    done;
    list_four := (List.rev !list_four)

let call_column matrix =
  let rec call_rec liste = match liste with
    | [] -> ()
    | e1::e2::l -> begin
        search_column matrix e1 e2;		   
        call_rec l
      end
    | _ -> invalid_arg "Bug nombre de colonnes"
  in call_rec (!list_y)


let to_couple_x liste =
  let rec couple_x = function
    | [] -> []
    | e1::e2::l -> (e1,e2) :: (couple_x l)
    | _ -> [] 
  in couple_x (List.rev liste)


let moy_char liste =
  let rec medium_space liste_x = match liste_x with
    |[] -> 0
    |(e1,e2)::l -> (e2 - e1) + medium_space l
  in ((medium_space (liste)) / (List.length liste))


let calcul_dist_char liste =
  let rec calcul_dist liste accu = match liste with
    |[] -> failwith "Liste paire!"
    |_::[] -> (List.rev (accu))
    |(e1,e2)::(e3,e4)::l -> calcul_dist ((e3,e4)::l) ((e3-e2)::accu)
  in calcul_dist liste []


let is_space listeX = 
  let new_listX = to_couple_x listeX in
  let moyenne_char = moy_char new_listX in
  let distance_char = calcul_dist_char new_listX in
  let vect = Array.make (List.length distance_char) false in
    for i = 0 to (List.length (distance_char) - 1) do
      if (((List.nth distance_char i) > (moyenne_char/2)) || ((List.nth distance_char i) < 0)) then
        vect.(i) <- true
    done;
    vect



let rec prince liste = match liste with 
  |[]   -> print_string "fuck you";
  |(x1,x2,y1,y2)::l -> print_string("(x1="^(string_of_int(x1))
                                    ^",x2="
                                    ^(string_of_int(x2))
                                    ^",y1="^(string_of_int(y1))
                                    ^",y2="^(string_of_int(y2))
                                    ^");");
                       prince l


let princesse vect =
  for i = 0 to Array.length (vect) - 1 do
    print_int 3;
  done


(* Build a list of 16*16 matrix that represent the chars *)
let reducted_matrix c rapport nmatrix =
  let inv_rapport = 1.0 /. rapport in
      for x = 0 to c - 1 do
    for y = 0 to c -1 do
      if float_of_int (Matrix.get nmatrix x y) >= inv_rapport then
        Matrix.set nmatrix x y 255
      else
        Matrix.set nmatrix x y 0;
    done
      done;
      nmatrix

let reduct_first_matrix (x1,y1,x2,y2) c matrix =
  let new_matrix = Matrix.empty c c and 
      rapport = mini (float_of_int(c - 1)
              /. float_of_int(x2 - x1))
    (float_of_int(c - 1) /. float_of_int(y2 - y1)) in
    begin
      for x = 0 to x2 - x1 do
    for y = 0 to y2 - y1 do
      let tx = i_of_f(float_of_int(x) *. rapport) and 
          ty = i_of_f(float_of_int(y) *. rapport) in
        begin
          if (Matrix.get matrix (x + x1) (y + y1)) != 0 then
        Matrix.set new_matrix (tx) (ty) 
                ((Matrix.get new_matrix tx ty) -1);
        end 
    done
      done;
      reducted_matrix c rapport new_matrix
    end 

let build_list_mat matrix liste = 
  let list_mat = ref [] in
    for i = 0 to (List.length (liste) - 1) do
        list_mat :=
	(reduct_first_matrix (List.nth liste i) 16 matrix)::(!list_mat);
    done;
    (List.rev !list_mat)

let call matrix = 
  build_list_mat matrix !list_four 



(*Algorithm which swap 0 to 1 if number of near 0 < 5*)

let fill_in_matrix count_zero matrix2 i j h_test =
  if h_test then
    if !count_zero < 5 then
      while !count_zero <> 0 do 
        Matrix.set matrix2 (i - !count_zero ) j 0;
        count_zero := !count_zero - 1
      done
    else
      while !count_zero <> 0 do
        Matrix.set matrix2 (i - !count_zero) j 255;
        count_zero := !count_zero - 1
      done
  else
    if !count_zero < 2 then
      while !count_zero <> 0 do
        Matrix.set matrix2 i (j - !count_zero) 0;
        count_zero := !count_zero - 1
      done
    else
      while !count_zero <> 0 do
        Matrix.set matrix2 i (j - !count_zero) 255;
        count_zero := !count_zero - 1
      done

(*Horizontally RLSA*)

let hrlsa matrix1 matrix2 =
  let count_zero = ref 0 in
    for j = 0 to (Matrix.height matrix1) -1 do 
      for i = 0 to (Matrix.width matrix1) -1 do
        if (Matrix.get matrix1 i j != 0) then
          count_zero := !count_zero + 1
        else
          begin
            fill_in_matrix count_zero matrix2 i j
              true;
            Matrix.set matrix2 i j 1
          end
      done;
      fill_in_matrix count_zero matrix2 (Matrix.width matrix1) j true
    done



let vrlsa matrix1 matrix2 = 
  let count_zero = ref 0 in
    for i = 0 to (Matrix.width matrix1) -1 do
      for j = 0 to (Matrix.height matrix1) -1 do
        if (Matrix.get matrix1 i j != 0) then 
          count_zero := !count_zero + 1
        else
          begin
            fill_in_matrix count_zero matrix2 i j
              false;
            Matrix.set matrix2 i j 1
          end
      done;
      fill_in_matrix count_zero matrix2 i (Matrix.height matrix1) false
    done



(*Combination of two matrix with an logical OR*)

let combination matrix1 matrix2 = 
  for i = 0 to (Matrix.width matrix1) - 1 do
    for j = 0 to (Matrix.height matrix1) - 1 do
      if ((Matrix.get matrix1 i j) = 0 ||
          (Matrix.get matrix2 i j) = 0) then
        Matrix.set matrix1 i j 0
      else
        Matrix.set matrix1 i j 255
    done;
  done;



(*let pixel_exist matrix1 i j = 
  i > 0 && i < Matrix.matrix1.w && j > 0 && Matrix.matrix1.h   
  
let near_zero matrix i j =
  (pixel_exist matrix i-1 j-1 && Matrix.get matrix[i-1, j-1] = 0) ||
  (pixel_exist matrix i-1 j && Matrix.get matrix[i-1,j] = 0) ||
  (pixel_exist matrix i-1 j+1 && Matrix.get matrix[i-1, j+1] = 0) ||
  (pixel_exist matrix i j-1 && Matrix.get matrix[i, j-1] = 0) ||
  (pixel_exist matrix i j+1 && Matrix.get matrix [i, j+1] = 0) ||
  (pixel_exist matrix i+1 j-1 && Matrix.get matrix[i+1, j-1] = 0) ||
  (pixel_exist matrix i+1 j && Matrix.get matrix[i+1, j] = 0) ||
  (pixel_exist matrix i+1 j+1 && Matrix.get matri[i+1, j+1] = 0)


let cadre matrix1 matrix2 =
 for j = 0 to (matrix1.Matrix.h - 1) do
  for i = 0 to (matrix1.Matrix.w - 1) do
   if (Matrix.get matrix1 i j = 0) then
     Matrix.set matrix2 i j 0
   else
     if near_zero matrix1 i j then
       Matrix.set matrix2 i j 1
     else
       Matrix.set matrix2 i j 0
  done;
 done*)
