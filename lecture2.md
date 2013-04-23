Korekce 2. přednášky
====================

I. Definice
-----------

Infixní operátory se dají definovat všemožnými způsoby, Kryl ukazoval na přednášce pouze jeden. Jako příklad si vezmeme opět operátor `(.)` (kompozice funkcí):

    (.) :: (b -> c) -> (a -> b) -> a -> c

    (.) f g x = f (g x)

    (.) f g = \x -> f (g x)

    f . g = \x -> f (g x)

    -- matematický zápis
    (f . g) x = f (g x)

Opět se na přednášce vyskytl problém s kontexty, které nepatří před typovou signaturu, ale za `::`:

    Ord(a) => qs :: [a] -> [a]

    -- správně je
    qs :: ord a => [a] -> [a]

Viz korekce první přednášky.

II. Quicksort
-------------

Další populární chybou je označovat definici:

    sort []     = []
    sort (x:xs) = [y | y <- xs, y <= x] ++ [x] ++ [y | y <- xs, y > x]

za quicksort. Jistě, jednou ze základních idejí quicksortu je metoda rozděl a panuj, nicméně druhou důležitou ideou je in-place přerozdělení pole. Tato definice bývá označována jako tree sort.

III. Operátory
--------------

Drobná poznámka k sekcím operátorů (operator section):

    (1+) :: Int -> Int
    (+1) :: Int -> Int

    (1-) :: Int -> Int
    (-1) :: Int

`(-1)` není sekce operátoru, ale konstanta. Narozdíl od jazyka `ML` nás Haskell nenutí používat `~1` pro záporná čísla. Na druhou stranu si ale programátor musí dávat pozor na podobné výrazy:

    succ -1
    -- parsuje se jako (succ) - (1)

    succ (-1)
    -- v pořádku

A samozřejmě problém se sekcí operátoru `(-)`. Pro "sekci" typu `(-n)` nabízí Haskell funkci:

    subtract n
    -- subtract n m = m - n

IV. Bottom
----------

Haskell pro bottom nepoužívá hodnotu `bot`, ale `undefined`, případně ještě `error`.

    undefined :: a
    undefined = ... -- implementation defined

    error :: String -> a
    error errormsg = ... -- implementation defined

Což je také odpověď na otázku z přednášky: `undefined` má univerzálně polymorfní typ. To dává smysl, pokud uvážíme, že jsme schopni napsat nekonečnou rekurzi pro jakýkoliv typ a divergence je sémanticky bottom.

    x :: [Int]
    x = undefined

V. Lazy patterns
----------------

Lazy patterny jsou sémanticky ekvivalentí lokální definici:

    f ~(a, b) = 0

    f p = 0
      where (a, b) = p 

Pozor, pokud je pattern, ke kterému připíšeme `~` triviální (proměnná nebo podtržítko), tak `~` nemá žádný efekt.

    f x y = ...
    -- je ekvivalentní s
    f ~x ~y = ...

VI. Závěrem
-----------

Jako obvykle jsem k zastižení na adrese vituscze@gmail.com
