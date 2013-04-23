Korekce 2. pøednášky
====================

I. Definice
-----------

Infixní operátory se dají definovat všemožnými zpùsoby, Kryl ukazoval na pøednášce pouze jeden. Jako pøíklad si vezmeme opìt operátor `(.)` (kompozice funkcí):

    (.) :: (b -> c) -> (a -> b) -> a -> c

    (.) f g x = f (g x)

    (.) f g = \x -> f (g x)

    f . g = \x -> f (g x)

    -- matematický zápis
    (f . g) x = f (g x)

Opìt se na pøednášce vyskytl problém s kontexty, které nepatøí pøed typovou signaturu, ale za `::`:

    Ord(a) => qs :: [a] -> [a]

    -- správnì je
    qs :: ord a => [a] -> [a]

Viz korekce první pøednášky.

II. Quicksort
-------------

Další populární chybou je oznaèovat definici:

    sort []     = []
    sort (x:xs) = [y | y <- xs, y <= x] ++ [x] ++ [y | y <- xs, y > x]

za quicksort. Jistì, jednou ze základních idejí quicksortu je metoda rozdìl a panuj, nicménì druhou dùležitou ideou je in-place pøerozdìlení pole. Tato definice bývá oznaèována jako tree sort.

III. Operátory
--------------

Drobná poznámka k sekcím operátorù (operator section):

    (1+) :: Int -> Int
    (+1) :: Int -> Int

    (1-) :: Int -> Int
    (-1) :: Int

`(-1)` není sekce operátoru, ale konstanta. Narozdíl od jazyka `ML` nás Haskell nenutí používat `~1` pro záporná èísla. Na druhou stranu si ale programátor musí dávat pozor na podobné výrazy:

    succ -1
    -- parsuje se jako (succ) - (1)

    succ (-1)
    -- v poøádku

A samozøejmì problém se sekcí operátoru `(-)`. Pro "sekci" typu `(-n)` nabízí Haskell funkci:

    subtract n
    -- subtract n m = m - n

IV. Bottom
----------

Haskell pro bottom nepoužívá hodnotu `bot`, ale `undefined`, pøípadnì ještì `error`.

    undefined :: a
    undefined = ... -- implementation defined

    error :: String -> a
    error errormsg = ... -- implementation defined

Což je také odpovìï na otázku z pøednášky: `undefined` má univerzálnì polymorfní typ. To dává smysl, pokud uvážíme, že jsme schopni napsat nekoneènou rekurzi pro jakýkoliv typ a divergence je sémanticky bottom.

    x :: [Int]
    x = undefined

V. Lazy patterns
----------------

Lazy patterny jsou sémanticky ekvivalentí lokální definici:

    f ~(a, b) = 0

    f p = 0
      where (a, b) = p 

Pozor, pokud je pattern, ke kterému pøipíšeme `~` triviální (promìnná nebo podtržítko), tak `~` nemá žádný efekt.

    f x y = ...
    -- je ekvivalentní s
    f ~x ~y = ...

VI. Závìrem
-----------

Jako obvykle jsem k zastižení na adrese vituscze@gmail.com
