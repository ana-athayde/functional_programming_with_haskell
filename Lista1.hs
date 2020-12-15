module Lista1 where

ehTriangulo a b c = if (a>b && a>c) then (if b+c>a then True else False)
					else if (b>a && b>c) then (if a+c>b then True else False)
					else if (a==b && b==c) then True
					else False
					
tipoTriangulo a b c = if ehTriangulo a b c 
						then (if a/=b && b/=c then "escaleno" 
								else if a==b && b==c then "equilatero"
								else if a==b|| b==c ||a==c then "isosceles"
								else "nao") 
						else "nao ehTriangulo"
						
triangulo a b c = if ehTriangulo a b c 
						then (if a/=b && b/=c then "escaleno" 
								else if a==b && b==c then "equilatero"
								else if a==b|| b==c ||a==c then "isosceles"
								else "nao eh um triangulo") 
						else "nao eh um triangulo"
						
somaPares 0 = 0
somaPares n = if rem n 2 == 0 then n+(somaPares(n-1)) else somaPares(n-1)

somaPot2m n m = if n==0 then m else (2^n)*m + somaPot2m (n-1) m

primo 1 = False
primo 2 = True
primo n = if (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 then False else True

seriePI n = seriePI' 1 1 n
seriePI' s i n = s*4/i + if (n>i+2) then seriePI' (negate s) (i+2) n else 0
