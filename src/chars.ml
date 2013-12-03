type area = {
    debut : int * int ;
    fin : int * int ;
}


let matrix_of_area area img = 
    let ((x1,y1),(x2,y2)) = (area.debut,area.fin) in
    let f x y = 
        match (x,y) with
            | (n,_) when (n < x1 && n >= x2) -> 255
            | (_,n) when (n < y1 && n >= y2) -> 255
            | (_,_) -> Matrix.get img x y
    in
    Matrix.init (x2-x1) (y2-y1) f
    
