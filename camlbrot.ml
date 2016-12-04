#load "graphics.cma"

open Graphics

let nmax = 1000
let colorrange = 255./.(log(float_of_int nmax))

let largeur = 1280
let hauteur = 960
let ratio = (float_of_int largeur)/.(float_of_int hauteur)

let ymin = -1.25
let ymax = 1.25
let xmin = -2.4
let xmax = xmin+.(ymax-.ymin)*.ratio
let xpix = (xmax-.xmin)/.(float_of_int largeur)
let ypix = (ymax-.ymin)/.(float_of_int hauteur)


let test re im =
  let rec aux accu x y =
    match accu with
      | 0 ->
          nmax
      | n ->
          let x_sq = x*.x and y_sq = y*.y in
            if x_sq +. y_sq > 4.
            then nmax-n
            else aux (n-1) (x_sq -. y_sq +. re) (2. *. x *. y +.im)
  in aux nmax re im

let colore_point i j niter =
  if niter == nmax
  then set_color black
  else begin
    let clr = int_of_float(log(float_of_int niter)*.colorrange) in
      set_color (rgb clr clr 255)
  end;
  plot i j

let colore_naif imin imax jmin jmax =
  let x = ref 0. and y = ref 0. in
    for i=imin to imax do
      x := (float_of_int i)*.xpix +. xmin;
      for j=jmin to jmax do
        y := (float_of_int j)*.ypix +. ymin;
        colore_point i j (test !x !y)
      done
    done

let rec colore_recursif imin imax jmin jmax =
  if (imax-imin<3)||(jmax-jmin<3)
  then colore_naif imin imax jmin jmax
  else
    let interieur = ref true in
    let x1,x2,y1,y2= ref 0., ref 0., ref 0., ref 0. in

      y1 := (float_of_int jmin)*.ypix +. ymin;
      y2 := (float_of_int jmax)*.ypix +. ymin;
      for i=imin to imax do
        x1 := (float_of_int i)*.xpix +. xmin;
        let niter1 = test !x1 !y1  in
          colore_point i jmin niter1;
          interieur:= !interieur && (niter1==nmax);
          let niter2 = test !x1 !y2  in
            colore_point i jmax niter2;
            interieur:= !interieur && (niter2==nmax);
      done;

      x1 := (float_of_int imin)*.xpix +. xmin;
      x2 := (float_of_int imax)*.xpix +. xmin;
      for j=jmin+1 to jmax-1 do
        y1 := (float_of_int j)*.ypix +. ymin;
        let niter1 = test !x1 !y1  in
          colore_point imin j niter1;
          interieur:= !interieur && (niter1==nmax);
          let niter2 = test !x2 !y1  in
            colore_point imax j niter2;
            interieur:= !interieur && (niter2==nmax);
      done;

      if !interieur
      then
        begin
          set_color black;
          fill_rect (imin+1) (jmin+1) (imax-imin-2) (jmax-jmin-2)
        end
      else
        begin
          if imax-imin > jmax-jmin
          then
            begin
              let imilieu = (imin+imax)/2 in
                colore_recursif (imin+1) imilieu (jmin+1) (jmax-1);
                colore_recursif (imilieu+1) (imax-1) (jmin+1) (jmax-1);
            end
          else
            begin
              let jmilieu = (jmin+jmax)/2 in
                colore_recursif (imin+1) (imax-1) (jmin+1) jmilieu;
                colore_recursif (imin+1) (imax-1) (jmilieu+1) (jmax-1);
            end
        end


;;
open_graph (" "^(string_of_int largeur)^"x"^(string_of_int hauteur));
set_window_title ("Mandelbrot ["
                  ^(string_of_float xmin)^";"
                  ^(string_of_float xmax)^"] x ["
                  ^(string_of_float ymin)^";"
                  ^(string_of_float ymax)^"]");
colore_recursif 0 (largeur-1) 0 (hauteur-1);
wait_next_event [Key_pressed] |> ignore;
close_graph();

