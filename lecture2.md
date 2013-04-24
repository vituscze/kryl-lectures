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
    qs :: Ord a => [a] -> [a]

Viz korekce první přednášky.

II. Quicksort
-------------

Další populární chybou je označovat definici:

    sort []     = []
    sort (x:xs) = [y | y <- xs, y <= x] ++ [x] ++ [y | y <- xs, y > x]

za quicksort. Jistě, jednou ze základních idejí quicksortu je metoda rozděl a panuj, nicméně druhou důležitou ideou je in-place přerozdělení pole. Tato definice bývá označována jako varianta tree sortu, která nepoužívá pomocný strom (deforested tree sort). Viz [Stack Overflow](http://stackoverflow.com/questions/7717691).

III. Operátory
--------------

Drobná poznámka k sekcím operátorů (operator section):

    (1+) :: Int -> Int
    (+1) :: Int -> Int

    (1-) :: Int -> Int
    (-1) :: Int

`(-1)` není sekce operátoru, ale konstanta. Narozdíl od jazyka ML nás Haskell nenutí používat `~1` pro záporná čísla. Na druhou stranu si ale programátor musí dávat pozor na podobné výrazy:

    succ -1
    -- parsuje se jako (succ) - (1)

    succ (-1)
    -- v pořádku

A samozřejmě problém se sekcí operátoru `(-)`. Pro "sekci" typu `(-n)` nabízí Haskell funkci:

    subtract n
    -- subtract n m = m - n

Nicméně, v některých případech nelze as-pattern použít, ačkoliv za normálních okolností by to problém nebyl:

    :i Either
    data Either a b = Left a | Right b      -- Defined in `Data.Either'

    mapEither :: (a -> b) -> Either e a -> Either e b
    mapEither f (Left e)  = Left e
    mapEither f (Right a) = Right (f a)

První řádek vypadá jako vhodný kandidát na as-pattern:

    mapEither f e@(Left _) = e

    <interactive>:5:81:
    Couldn't match type `a' with `b'
      `a' is a rigid type variable bound by
          the type signature for
            mapEither :: (a -> b) -> Either e a -> Either e b
          at <interactive>:5:56
      `b' is a rigid type variable bound by
          the type signature for
            mapEither :: (a -> b) -> Either e a -> Either e b
          at <interactive>:5:56
    Expected type: Either e b
      Actual type: Either e a
    In the expression: e
    In an equation for `mapEither': mapEither f e@(Left _) = e

Problém je ten, že `Left e` na levé straně rovnítka má typ `Either e a` zatímco na pravé straně potřebujeme `Either e b`. Pokud si dáme tu práci a na pravé straně přepíšeme znovu konstruktor `Left`, tak je vše v pořádku. Proč? `Left` sám o sobě má typ `e -> Either e x`, což nám umožní zvolit si libovolné `x`, v našem případě `x = b`.

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

V. Patterny
-----------

Lazy patterny jsou sémanticky ekvivalentí lokální definici:

    f ~(a, b) = 0

    f p = 0
      where (a, b) = p

Pozor, pokud je pattern, ke kterému připíšeme `~` triviální (proměnná nebo podtržítko), tak `~` nemá žádný efekt.

    f x y = ...
    -- je ekvivalentní s
    f ~x ~y = ...

As patterny se používají v situacích, kdy se často odkazujete na celý pattern, např.:

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x:xs') ys@(y:ys')
      | x <= y    = x:merge xs' ys
      | otherwise = y:merge xs  ys'

Opět, as pattern je sémanticky ekvivalentní lokální definici:

    case expr of as@pat -> -- ...

    let as = expr
    in case expr of pat -> -- ...

VI. Závěrem
-----------

Všiml jsem si, že spousta příkladů z přednášky je převzata z [language reportu](http://www.haskell.org/onlinereport/haskell2010/).

Jako obvykle jsem k zastižení na adrese vituscze@gmail.com
