Korekce 2. p�edn�ky
====================

I. Definice
-----------

Infixn� oper�tory se daj� definovat v�emo�n�mi zp�soby, Kryl ukazoval na p�edn�ce pouze jeden. Jako p��klad si vezmeme op�t oper�tor `(.)` (kompozice funkc�):

    (.) :: (b -> c) -> (a -> b) -> a -> c

    (.) f g x = f (g x)

    (.) f g = \x -> f (g x)

    f . g = \x -> f (g x)

    -- matematick� z�pis
    (f . g) x = f (g x)

Op�t se na p�edn�ce vyskytl probl�m s kontexty, kter� nepat�� p�ed typovou signaturu, ale za `::`:

    Ord(a) => qs :: [a] -> [a]

    -- spr�vn� je
    qs :: ord a => [a] -> [a]

Viz korekce prvn� p�edn�ky.

II. Quicksort
-------------

Dal�� popul�rn� chybou je ozna�ovat definici:

    sort []     = []
    sort (x:xs) = [y | y <- xs, y <= x] ++ [x] ++ [y | y <- xs, y > x]

za quicksort. Jist�, jednou ze z�kladn�ch idej� quicksortu je metoda rozd�l a panuj, nicm�n� druhou d�le�itou ideou je in-place p�erozd�len� pole. Tato definice b�v� ozna�ov�na jako tree sort.

III. Oper�tory
--------------

Drobn� pozn�mka k sekc�m oper�tor� (operator section):

    (1+) :: Int -> Int
    (+1) :: Int -> Int

    (1-) :: Int -> Int
    (-1) :: Int

`(-1)` nen� sekce oper�toru, ale konstanta. Narozd�l od jazyka `ML` n�s Haskell nenut� pou��vat `~1` pro z�porn� ��sla. Na druhou stranu si ale program�tor mus� d�vat pozor na podobn� v�razy:

    succ -1
    -- parsuje se jako (succ) - (1)

    succ (-1)
    -- v po��dku

A samoz�ejm� probl�m se sekc� oper�toru `(-)`. Pro "sekci" typu `(-n)` nab�z� Haskell funkci:

    subtract n
    -- subtract n m = m - n

IV. Bottom
----------

Haskell pro bottom nepou��v� hodnotu `bot`, ale `undefined`, p��padn� je�t� `error`.

    undefined :: a
    undefined = ... -- implementation defined

    error :: String -> a
    error errormsg = ... -- implementation defined

Co� je tak� odpov�� na ot�zku z p�edn�ky: `undefined` m� univerz�ln� polymorfn� typ. To d�v� smysl, pokud uv��me, �e jsme schopni napsat nekone�nou rekurzi pro jak�koliv typ a divergence je s�manticky bottom.

    x :: [Int]
    x = undefined

V. Lazy patterns
----------------

Lazy patterny jsou s�manticky ekvivalent� lok�ln� definici:

    f ~(a, b) = 0

    f p = 0
      where (a, b) = p 

Pozor, pokud je pattern, ke kter�mu p�ip�eme `~` trivi�ln� (prom�nn� nebo podtr��tko), tak `~` nem� ��dn� efekt.

    f x y = ...
    -- je ekvivalentn� s
    f ~x ~y = ...

VI. Z�v�rem
-----------

Jako obvykle jsem k zasti�en� na adrese vituscze@gmail.com
