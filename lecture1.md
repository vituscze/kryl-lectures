Korekce 1. přednášky
====================

I. Obecné
---------

Ačkoliv je do jisté míry pravda, že Haskell je ovlivněn prací Haskell B. Curryho, tvrdit, že je Haskell dílem jeho žáků je přehnané. Pro zájemce uvádím odkaz na [A History of Haskell: Being Lazy With Class](http://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/history.pdf)

Funkce v Haskellu skutečně berou pouze jeden argument, ale tohle je v rozporu s pozdějším Krylovo tvrzením, že konstanty jsou vlastně nulární funkce. Haskell na úrovni typů velice ostře rozlišuje funkce od konstant. Tedy například:

    f :: Int -> Char   -- funkce
    x :: Int           -- konstanta
    l :: [Int -> Char] -- list funkcí, stále konstanta

Pokud si přepíšete typovou signaturu jako strom, tak funkce jsou právě ty hodnoty, které mají šipku (`->`) v kořeni stromu. Příklad:

    f :: Int -> Char -> (Bool, String)

            (->)
            /  \
           /    \
        Int     (->)
                /  \
               /    \
           Char      (,)
                     / \
                    /   \
                 Bool    String

    l :: [Int -> Int]

            [ ]
             |
             |
            (->)
            /  \
           /    \
        Int     Int

Zde je také nutné podotknout, že "funkční šipka" je operátor, který se asociuje vpravo, proto si můžeme dovolit vynechat zbytečné závorky.

    a -> b -> c == a -> (b -> c)

ale pozor!

    a -> b -> c /= (a -> b) -> c

V tomto případě je `a -> b -> c` funkce, která bere jeden argument typu `a` a vrací funkci typu `b -> c`, zatímco `(a -> b) -> c` je funkce, která bere jako argument FUNKCI typu `a -> b` a vrací `c`.

Na toto téma mohu doporučit (relativně krátký) [blog post Conala Elliota](http://conal.net/blog/posts/everything-is-a-function-in-haskell).

II. Syntaxe
-----------

### II.I. Funkce

Co Kryl na přednášce nezmínil a přitom je to velice podstatné (hlavně abyste neskončili u metody pokus/omyl a také abyste psali pěkný kód) je syntaxe pro aplikaci funkcí.

V Haskellu totiž žádná syntaxe pro aplikaci funkcí není, aplikace je jednoduchá juxtapozice výrazů (tj. dáte dvě věci za sebe), závorky slouží pouze ke seskupování výrazů. Několik příkladů:

    add :: Int -> Int -> Int
    add = (+)

    -- 1 + 2
    add 1 2

    -- (1 + 2) + 3
    add (add 1 2) 3

    -- 1 + (2 + 3)
    add 1 (add 2 3)

pozor:

    add 1 add 2 3

se pokouší volat funkci `add` s 4 argumenty, speciálně `1`, `add`, `2` a `3`.

    f :: c -> d
    g :: a -> b -> c

    -- f(g(x, y))
    f (g x y)

opět

    f g x y

se pokouší volat funkci `f` s 3 argumenty, `g`, `x` a `y`.

Prefixní aplikace funkcí má nejvyšší prioritu, tj.

    f x + g y

je vždy parsováno jako

    (f x) + (g y)

bez ohledu na prioritu operátoru `+`.


Dále Kryl zmínil převádění prefixních funkcí na funkce infixní, k čemuž bych jen dodal pár příkladů, kdy se tohle může hodit:

    sum :: [Int] -> Int
    sum list = foldr (+) 0 list

    halfs :: [a] -> ([a], [a])
    halfs list = splitAt (length list `div` 2) list

    Speciálně se tedy dají dělat podobné srandy:

    memberOf :: Int -> [Int] -> Bool
    memberOf x xs = ... -- x se nalézá v seznamu xs

    if 1 `memberOf` someList then ... else ...


### II.II. Typové třídy

Další poznámka se týká kontextů. Kryl ukazoval příklad:

    f :: [Int] -> [Int]
    f list = [2 * x | x <- list]

Tahle funkce ovšem funguje pro více věcí, než jen `Int`.

    f :: [a] -> [a]

je však moc obecné (násobení není definováno např. pro funkce).

    Num a => f :: [a] -> [a]

je však špatně, všechny kontexty skutečně přijdou na začátek typové signatury před `=>`, ale až po `::`, správně je tedy:

    f :: Num a => [a] -> [a]

Kontexty se dají libovolně kupit, např. tedy:

    f :: (Ord a, Num a, Show b) => a -> [b]

tento kontext specifikuje podmínku, že typ `a` musí mít uspořádání (`Ord`) a musí podporovat numerické operace (`Num`) a že typ `b` musí být "showable" (`Show`).

Pokud chcete vědět, co jednotlivé typové třídy obsahují za funkce, můžete v `GHCi` použít příkaz `:i` třída

    ghci> :i Ord
    class Eq a => Ord a where
      compare :: a -> a -> Ordering
      (<) :: a -> a -> Bool
      (>=) :: a -> a -> Bool
      (>) :: a -> a -> Bool
      (<=) :: a -> a -> Bool
      max :: a -> a -> a
      min :: a -> a -> a
            -- Defined in `GHC.Classes'
      ...


### II.III. Datové typy

Dále tu máme definice nových typů a keyword `data`. Uvažme opět binární stromy:

    data Tree a
        = Nil
        | Node (Tree a) a (Tree a)

`data` deklaruje nový datový typ.

`Tree` je typový konstruktor, za kterým následuje seznam parametrů. Můžete se na to dívat jako na šablony v C++ nebo generics v Javě a C#. Příklad:

    data Tree a = ...

přibližně odpovídá

    template <typename A>
    class Tree
    { ... };

Zde je krásně vidět rozdíl mezi typem a typovým konstruktorem, stejně jako v C++ nemůžete napsat:

    Tree t = ...;

tak v Haskellu nelze použít:

    f :: Tree -> ...

`Tree` není sám o sobě datový typ, očekává ještě jeden parametr, tedy:

    Tree<int> t = ...;

    f :: Tree Int -> ...

Za rovnítkem následuje výčet možných hodnot, kterých může tento datový typ nabývat. Jednotlivé položky jsou odděleny svislítkem. Typ `Bool` je např. definován takto:

    data Bool
        = False
        | True

Každá alternativa definuje datový konstruktor a případné argumenty, např:

    data IntPair
        = Pair Int Int

    data IntList
        = ListNil
        | Cons Int IntList

Nulární konstruktory jsou potom konstanty daného datového typu.

Je velice důležité si uvědomit, že konstruktory se dají použít jako obyčejné funkce, tj. z předchozích deklarací dostáváme:

    Nil :: Tree a
    Node :: Tree a -> a -> Tree a -> Tree a

    False :: Bool
    True :: Bool

    Pair :: Int -> Int -> IntPair

    ListNil :: IntList
    Cons :: Int -> IntList -> IntList

Můžeme tedy provést například toto:

    -- add new element to every IntList in a list
    f :: Int -> [IntList] -> [IntList]
    f number list = map (Cons number) list


### II.IV. Pattern matching

Nyní se dostáváme k nejdůležitější části a to je pattern matching. Pár poznámek k vyhodnocování a odpověď na nezodpovězenou otázku z přednášky.

    crazy :: [[a]] -> Int
    crazy ([]:xs)    = 1
    crazy (x:[y]:xs) = 2
    crazy ([]:[]:xs) = 3
    crazy _          = 4

Vyhodnocování probíhá shora dolů, tj. použije se pravá strana prvního patternu, který uspěje. Narozdíl od Prologu Haskell nezkouší jiné možnosti a taky nepodporuje unifikaci v Prologovském smyslu. **Nemůžete** tedy napsat toto:

    same :: (a, a) -> Bool
    same (x, x) = True
    same _      = False

Proměnná se v patternu může vyskytnout právě jednou, tj. musíte napsat:

    same :: Eq a => (a, a) -> Bool -- budeme potřebovat test na rovnost
    same (a, b) = a == b

Když se nad tím zamyslíte, dává to smysl. Představte si, že funkci `same` předáme dvojici **funkcí**. Unifikace funkcí, speciálně tedy test na rovnost, je algoritmicky nerozhodnutelný problém (viz Halting Problem).

Než se do Haskellu ponoříte trochu více, tak zatím můžete používat jednoduché pravidlo k závorkování: závorkujte každý netriviální pattern. Polovina Krylových příkladů vám nebude fungovat právě kvůli špatnému závorkování.

    root :: Tree a -> a
    root Node l x r = x

Pokud napíšete toto, snažíte se definovat funcki 4 proměnných. Kromě toho, že `Node` samotný není pattern, tak dostanete typovou chybu, protože typová signatura funkce `root` specifikuje pouze 1 argument. Správně je:

    root (Node l x r) = x

Závorky se dají vynechat u triviálních patternů, tj. např.

    map _ []     = []
    map f (x:xs) = f x:map f xs


Definování funkcí výčtem "rovnic" je pouze syntaktická zkratka za `case`, předchozí definici tedy můžeme zapsat jako:

    map f list = case list of
        []     -> []
        (x:xs) -> f x:map f xs

Tímto způsobem pak můžeme provádět pattern matching i uvnitř anonymních funkcí:

    \list -> case list of
      []     -> ...
      (x:xs) ->

V některých případech lze pattern napsat přímo do seznamu argumentů, ale tohle se hodí pouze pokud neexistuje více alternativ, kterých daná hodnota může nabývat (pak ošetříte pouze jeden případ a funkce "spadne" pro některé vstupy).

    \(x, y) -> x + y

předchozí anonymní funkce má typ `Num a => (a, a) -> a`.


### II.V. Operátory

Prioritu operátorů si můžete sami zjisit pomocí GHCi takto:

    ghci> :i :
    data [] a = ... | a : [a]       -- Defined in `GHC.Types'
    infixr 5 :


    ghci> :i +
    class Num a where
      (+) :: a -> a -> a
      ...
            -- Defined in `GHC.Num'
    infixl 6 +

`infixr` označuje pravou, `infixl` levou asociativitu, číslo je pak priorita. Čím vyšší číslo, tím těsněji se daný operátor váže. Příklad:

    infixl 6 +
    infixl 7 *

    a + b * c
    -- se naparsuje jako
    a + (b * c)


    infixl 5 # -- levá asociativita

    a # b # c
    -- se naparsuje jako
    (a # b) # c

Ale naproti tomu:

    infixr 5 # -- pravá asociativita

    a # b # c
    -- se naparsuje jako
    a # (b # c)


### II.VI. Intervaly

Správná syntaxe pro intervaly je:

    [1 ..]      -- od 1 dále
    [1, 3 ..]   -- od 1 dále s krokem 2
    [1 .. 5]    -- od 1 do 5
    [1, 3 .. 5] -- od 1 do 5 s krokem 2

Všimněte si, že funkce:

    fromTo x y = [x .. y]

má typ `Enum a => a -> a -> [a]`, což naznačuje, že tato syntaxe funguje pro více věcí než čísla. Zeptejme se, co umí typová třída `Enum`.

    ghci> :i Enum
    class Enum a where
      succ :: a -> a
      pred :: a -> a
      toEnum :: Int -> a
      fromEnum :: a -> Int
      enumFrom :: a -> [a]
      enumFromThen :: a -> a -> [a]
      enumFromTo :: a -> a -> [a]
      enumFromThenTo :: a -> a -> a -> [a]
            -- Defined in `GHC.Enum'
    instance Enum Ordering -- Defined in `GHC.Enum'
    instance Enum Integer -- Defined in `GHC.Enum'
    instance Enum Int -- Defined in `GHC.Enum'
    instance Enum Char -- Defined in `GHC.Enum'
    instance Enum Bool -- Defined in `GHC.Enum'
    instance Enum () -- Defined in `GHC.Enum'
    instance Enum Float -- Defined in `GHC.Float'
    instance Enum Double -- Defined in `GHC.Float'

Je tu například instance pro typ `Char`. Můžeme tedy napsat:

    ['a' .. 'z'] == "abcdefghijklmnopqrstuvwxyz"

Poznámka:

    -- [...] se převádí na volání
    [a .. ]          enumFrom
    [a, b .. ]       enumFromThen
    [a .. c]         enumFromTo
    [a, b .. c]      enumFromThenTo


III. Závěrem
------------

Celý tento dokument je sepsán velmi narychlo, pokud najdete chybu nebo si dáte práci s pěknějším formátováním, můžete mě kontaktovat na adrese vituscze@gmail.com
