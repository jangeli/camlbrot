#open "graphics";;



(********************************
 Un type fenêtre qui regroupe :
 hauteur et largeur en pixels,
 coordonnées horizontales et verticales,
 et taille d'un pixel.
********************************)

type window = {
	height : int;
	width : int;
	xmin : float;
	xmax : float;
	ymin : float;
	ymax : float;
	pixsize : float;
	}
;;



(********************************
 Pour fabriquer une fenêtre, on indique :
 la taille de la fenêtre en pixels,
 les coordonnées du centre de la fenêtre,
 la taille (horizontale) de la fenêtre
********************************)

let make_window (w,h) (xcenter,ycenter) xrange =
	let ratio = (float_of_int h)/.(float_of_int w) in
	{
		height = h;
		width = w;
		xmin = xcenter -. xrange/.2.;
		xmax = xcenter +. xrange/.2.;
		ymin = ycenter -. xrange*.ratio/.2.;
		ymax = ycenter +. xrange*.ratio/.2.;
		pixsize = xrange /.(float_of_int w)
	}
;;




(********************************
 Quelques exemples de fenêtres
 pour faire des jolis dessins,
 à donner en argument à la fonction mandelbrot plus loin
********************************)

let exemple1 = make_window (640,480) (0.,0.) 4.5;;
let exemple2 = make_window (640,480) (-1.375,0.085) 0.02;;
let exemple3 = make_window (640,480) (-0.562,-0.643) 0.15;;
let exemple4 = make_window (1280,960) (-0.7513,0.0251) 0.006;;
let exemple5 = make_window (1280,960) (0.3023,-0.024) 0.011;;





(********************************
 Prend un complexe (en fait parties réelle et imaginaire),
 un nombre limite d'itérations, et calcule la distance
 au Mandelbrot (ou plutôt le nombre d'itérations pour diverger,
 ou -1 si on est dedans).
 Avec de la bidouille pour avoir un résultat continu et non discret.
********************************)

let distance re im maxiter =
  let rec aux n x y =
    if n==maxiter then -1.
    else
      let x_sq = x*.x and y_sq = y*.y in
        if x_sq +. y_sq > 7.2259738e86 (* e^200 *)
        then (float_of_int n)-.(log(x_sq+.y_sq)/.200.)
        else aux (n+1) (x_sq -. y_sq +. re) (2. *. x *. y +.im)
  in aux 0 re im
;;




(********************************
 Convertit une distance en couleur.
 On aplatit avec un log (on pourrait aussi prendre sqrt pour avoir un style différent)
 et on tourne dans l'espace des couleurs.
********************************)
let color d =
	if d < 0.
	then black
	else
		let x = log d in
		rgb 
		(int_of_float (127.5 +. 127.5*.cos x))
		(int_of_float (127.5 +. 127.5*.sin x))
		254
;;






(********************************
 La fonction principale.
 Le bout compliqué est la fonction récursive rectangle.
 Elle commence par traiter les bords du rectangle.
 S'ils sont uniformément noirs, on colorie tout en noir.
 Sinon on coupe en deux selon la plus grande dimension, et on récurre.
********************************)

let mandelbrot w maxiter =
	open_graph (" " ^ (string_of_int w.width) ^ "x" ^ (string_of_int w.height));
	set_window_title ("Mandelbrot ["
                    ^ (string_of_float w.xmin) ^ ";"
                    ^ (string_of_float w.xmax) ^ "] x ["
                    ^ (string_of_float w.ymin) ^ ";"
                    ^ (string_of_float w.ymax) ^ "]");

	let rec rectangle imin imax jmin jmax =
		let allblack = ref true in
		let traite i j =
			match point_color i j with
				| x when x=black -> ()
				| x when x=white ->
					let c = color (distance
										(float_of_int i *. w.pixsize +. w.xmin)
										(float_of_int j *. w.pixsize +. w.ymin)
										maxiter) in
					allblack := !allblack & (c=black);
					set_color c;
					plot i j 
				| _ -> allblack := false
			in
		for i = imin to imax do traite i jmin done;
		for j = jmin+1 to jmax-1 do traite imin j done;
		for i = imax downto imin do traite i jmax done;
		for j = jmax-1 downto jmin+1 do traite imax j done;
		if !allblack
		then
			begin
				set_color black;
				if (imax-imin>1) && (jmax-jmin>1) then fill_rect (imin+1) (jmin+1) (imax-imin-1) (jmax-jmin-1)
			end
		else
			begin
				if imax-imin > jmax-jmin
				then
					begin
						let imid = (imin+imax)/2 in
						if imid>imin then rectangle imin imid jmin jmax;
						if imax>imid then rectangle (imid+1) imax jmin jmax;
					end
				else
					begin
						let jmid = (jmin+jmax)/2 in
						if jmid>jmin then rectangle imin imax jmin jmid;
						if jmax>jmid then rectangle imin imax (jmid+1) jmax;
					end
			end

	in rectangle 0 (w.width-1) 0 (w.height-1)
;;









(*mandelbrot exemple1 100;;*)
(*mandelbrot exemple2 300;;*)
(*mandelbrot exemple3 300;;*)
(*mandelbrot exemple4 10000;;*)
mandelbrot exemple5 10000;;









