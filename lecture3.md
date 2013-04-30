Korekce 3. přednášky
====================

I. Pattern matching
-------------------

Číslené konstanty nejsou konstruktory žádného datového typu. Numerické konstanty se převádějí na volání konverzních funkcí takto:

    1   ==> fromInteger  (1   :: Integer )
    1.0 ==> fromRational (1.0 :: Rational)
    -- Rational jsou racionální čísla reprezentované zlomkem

`1 :: Integer` také není datovým konstruktorem typu `Integer`:

    ghci> :i Integer
    data Integer
      = integer-gmp:GHC.Integer.Type.S# GHC.Prim.Int#
      | integer-gmp:GHC.Integer.Type.J# GHC.Prim.Int# GHC.Prim.ByteArray#
            -- Defined in `integer-gmp:GHC.Integer.Type'

Ačkoliv je předchozí definice poněkud nepřehledná, je zde vidět, že `Integer` obsahuje konstruktory `S#` (pro čísla, která se vejdou do `Int`u) a `J#` (pro čísla větší).

Pattern matching na číslech se tedy musí převádět na volání operátoru `(==)`. To je také důvod, proč v novějších verzích GHC vyžaduje takovýto pattern matching instanci třídy `Eq`.

    ghci> let f 0 = 1; f n = n * f (n - 1)
    ghci> :t f
    f :: (Eq a, Num a) => a -> a

Další poznámka se týká funkce `sumPair`. Kryl nás informoval, že následující dvě definice jsou ekvivalentní:

    sumPair []        = []
    sumPair ((a,b):r) = (a + b):sumPair r

    sumPair []    = []
    sumPair (p:r) = (a + b):sumPair r
      where
        (a, b) = p

První funkce provede pattern match na dvojici `(a, b)` hned při zavolání, zatímco druhá funkce odloží pattern match až na první použití součtu `a + b`.

Kde se tyto funkce liší?

    ghci> let sumPair [] = []; sumPair ((a,b):r) = (a+b):sumPair r
    ghci> length . sumPair $ [undefined]
    *** Exception: Prelude.undefined

    ghci> let sumPair [] = []; sumPair (p:r) = (a+b):sumPair r where (a,b) = p
    ghci> length . sumPair $ [undefined]
    1

Jedná se o drobný detail, ale potom, co Kryl strávil polovinu minulé přednášky vysvětlováním pattern matchingu... asi by bylo dobré, kdybyste tento rozdíl také viděli. První funkce lze jednoduše přepsat na již ekvivalentní verzi:

    sumPair []        = []
    sumPair (~(a,b):r) = (a + b):sumPair r

A vskutku:

    ghci> let sumPair [] = []; sumPair (~(a,b):r) = (a+b):sumPair r
    ghci> length . sumPair $ [undefined]
    1

Poslední poznámka se vztahuje k `where` klauzuli funkce `sumPair`.

      where
        p = (a, b)

Je samozřejmě špatně. Na levé straně jsou proměnné, které právě definujeme; na pravé pak nějaký výraz. Tímto bychom se pokoušeli předefinovat proměnnou `p` výrazem `(a, b)` - proměnné `a` a `b` však nejsou nikde k nalezení, pokus o kompilaci tedy skončí chybou.

    ghci> let sumPair [] = []; sumPair (p:r) = (a+b):sumPair r where p = (a,b)

    <interactive>:24:39: Not in scope: `a'

    <interactive>:24:41: Not in scope: `b'

II. Prelude
-----------

Ačkoliv Kryl trvdí, že se s funkcemi `fst` a `snd` příliš nesetkáte, opak je pravdou. S čím se skutečně nesetkáte je kód typu:

    f pair = fst pair + snd pair

Ale funkce `fst` a `snd` jsou velice užitečné jako argumenty jiných funkcí, např:

    map fst [(1, 'a'), (2, 'b'), (3, 'c')] == [1, 2, 3]

Kromě toho se Kryl snažil definovat jakýsi operátor `>.>`, o kterém tvrdil, že je velice používaný. Opět - opak je pravdou. Žádný obdobný operátor není součástí `Prelude` a rozhodně není používaný. Naopak s operátorem `.` se potkáte na každém kroku.

Pro zájemce, operátor se stejným účelem se nachází v rozšiřujících knihovnách:

    ghci> :m +Control.Arrow
    ghci> :t (>>>)
    (>>>)
      :: Control.Category.Category cat => cat a b -> cat b c -> cat a c

    -- typ specializovaný pro funkce: (a -> b) -> (b -> c) -> (a -> c)

Pro nalezení počtu prvků, které jsou stejné jako zadaný prvek, existuje velice elegantní způsob:

    eql k = length . filter (== k)
    -- vyfiltrujeme prvky nerovné 'k' a vezmeme délku zbylého seznamu

III. Algoritmy
--------------

Nekonečný seznam prvočísel prezentovaný Krylem není příklad Eratosthenova síta. Základní idea tohoto algoritmu je vyškrtat všechny násobky daného prvočísla ze seznamu, ale Krylův algoritmus prochází všechny prvky a kontroluje, zda-li je zbytek po dělení daným prvočíslem nenulový.

Pravé Eratosthenovo síto s modulem ovšem nepracuje, pokusme se to napravit:

    -- rozdíl dvou setříděných seznamů
    diff :: Ord a => [a] -> [a] -> [a]
    diff []     _      = []
    diff xs     []     = xs
    diff (x:xs) (y:ys) = case x `compare` y of
        LT -> x:diff xs     (y:ys)
        EQ ->   diff xs     (y:ys)
        GT ->   diff (x:xs) ys

    -- síto
    sieve (p:ps) = p:sieve (ps `diff` [p * p, p * p + p ..])

    -- prvočísla
    primes = 2:sieve [3, 5 ..]

IV. Fixity deklarace
--------------------

Někdy se vám může hodit, že Haskell umožňuje nastavit prioritu a asociativitu i pro operátory, které vznikly z prefixových funkcí pomocí backticků:

    infixl 7 `div`

V. Typové třídy
---------------

Jak bylo uvedeno na přednášce, Haskell umožňuje v deklaraci typové třídy poskytnout defaultní implementace některých funkcí. `Eq` např. vypadá takto:

    class Eq a where
        (==), (/=) :: a -> a -> Bool

        a == b = not (a /= b)
        a /= b = not (a == b)

Všimněte si, že `Prelude` poskytuje defaultní implementace pro obě funkce. To např. znamená, že následující kód je v pořádku:

    data Q = Q

    instance Eq Q where

Při pokusu o porovnání `Q == Q` se výpočet zacyklí, protože defaultní implementace závisejí jedna na druhé.

Ve většině knihoven se tedy v dokumentaci setkáte s pojmem "minimal complete definition", tj. podmnožina funkcí třídy, které se musejí implementovat, aby byly všechny funkce definovány. Pro `Eq` je např. minimální definice `(==)` nebo `(/=)`.

Podívejme se na komplexnější třídu:

    class Eq a => Ord a where
        compare :: a -> a -> Ordering
        (<)  :: a -> a -> Bool
        (>=) :: a -> a -> Bool
        (>)  :: a -> a -> Bool
        (<=) :: a -> a -> Bool
        max  :: a -> a -> a
        min  :: a -> a -> a

    -- povšimněte si funkce 'compare', kterou Kryl na přednášce jaksi vynechal

    -- Ordering je datový typ:
    data Ordering = LT | EQ | GT
    -- LT = less than
    -- EQ = equal
    -- GT = greater than

Minimální kompletní definice je v [tomto případě](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Ord) `compare` nebo `(<=)`.

Proč vůbec tyto typové třídy obsahují tolik funkcí? Vždyť bychom mohli napsat toto:

    class Eq a where
        (==) :: a -> a -> Bool

    -- mimo třídu
    (/=) :: Eq a => a -> a -> Bool
    a /= b = not (a == b)

Odpovědi jsou dvě. První důvod je pohodlí programátora - občas je jednodušší definovat `(==)` a automaticky dostat `(/=)`, někdy je to naopak (což platí hlavně u složitějších tříd). Druhým důvodem je efektivita - v některých případech může být defaultní implementace méně efektivní, např.:

    class Container c where
        toList :: c a -> [a]
        find   :: a -> c a -> Bool

        find k = elem k . toList

Např. pro BST by byla defaultní implementace značně neefektivní (nejdřív zplošti strom do seznamu a pak v něm sekvenčně hledej daný prvek).

VI. Závěrem
-----------

Jako obvykle jsem k zastižení na adrese vituscze@gmail.com
