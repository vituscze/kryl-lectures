Korekce 5. přednášky
====================

I. `IO`
-------

V předchozí korekci jsem odbyl `IO a = RealWorld -> (a, RealWorld)` jako mýtus, ale Kryl se k němu dnes opět vracel. Jako mentální model může být tato korespondence postačující, ale za chvíli vyvstanou problémy, které do tohoto modelu nezapadají.

Pokud se nad tím zamyslíte, `putStrLn` jakožto `RealWorld -> ((), RealWorld)` by vzalo nějaký stav světa, změnilo stav vaší konzole a vrátilo nový stav světa se změněnou konzolí. Tohle by ale implikovalo, že se během vypisování řetězce do konzole stav světa nesmí změnit žádným jiným způsobem (nebo vždy stejným způsobem), celý svět by se tedy musel "zastavit" a počkat, dokud váš program neskončí s vypisováním řetězce - změnu světa mimo `putStrLn` není tento model schopen zachytit. Ale i když se omezíme na stav našeho programu (místo celého světa), tak vyvstane obdobný problém s vláknováním.

II. Syntaxe
-----------

Pouze drobný detail: znakové konstanty jsou uzavřeny v apostrofech, tedy:

    'a' :: Char

a infixové funkce jsou uzavřeny ve dvojici symbolů, kterým se říká backtick nebo backquote:

    4 `div` 7

III. Typové třídy
-----------------

Co na přednášce nebylo zmíněno je to, že funkce `return` a `(>>=)` jsou součástí typové třídy `Monad`. Pokud chcete používat `do` notaci, nestačí tedy pouze kamsi napsat:

    type I a = a

    return x = x
    m >>= f = f m

Je nutná deklarace celé typové třídy `Monad`. Zde ještě poznamenám, že Haskell 98 a 2010 neumožňují definovat instanci typové třídy pro typové synonymy `type` - z dobrých důvodů.

    type L x = [x]

    instance Monoid (L x) where
        mempty  = []
        mappend = (++)

    instance Monoid [x] where
        mempty  = []
        mappend = (++)

Tato definice vypadá na první pohled v pořádku, ale tyto instance se překrývají, což povede ke compile-time chybě. Dalším problémem jsou typové synonymy takovéhoto druhu:

    type T = [Int]

    instance Eq T where
        x == y = all (uncurry (==)) $ zip x y

Po odstranění synonyma dostáváme:

    instance Eq [Int] where
        -- ...

Což je definice, kterou Haskell 98 i 2010 zakazují.

Pokud v GHC zapnete dost rozšíření, tak se vám tento kód podaří zkompilovat. Co ale nejde použít ani pokud se rozhodnete použít rozšíření, jsou částečně aplikované synonyma:

    type I a = a

    instance Monad I where
        -- ...

Pokud byste tedy chtěli napsat instanci pro `I`, tak jako Kryl na přednášce, musíte definovat nový typ - ideálně pomocí `newtype`:

    newtype I a = I a

    runI :: I a -> a
    runI (I a) = a

    -- nebo v jednom:
    -- newtype I a = I { runI :: a }

    instance Monad I where
        return = I
        I m >>= f = f m

`newtype` se chová jako `data`, ale s tím rozdílem, že `newtype` se tváří jako nový typ jen pro potřeby kompilátoru - runtime reprezentace je poté shodná s původním typem. Proto je také `newtype` omezen na jeden unární datový konstruktor:

    newtype Pair a = P a a -- špatně
    newtype Either a b = Left a | Right b -- špatně

    newtype Func a b = Func (a -> b) -- OK

Poslední poznámka se týká monády `State` z přednášky. Typ `Table` je (ať je to cokoliv) zcela zbytečný a v implementaci v modulu `Control.Monad.State` žádný takovýhle typ nenajdete. Nejgeneričtější `State` vypadá takto:

    newtype State s a = State { runState :: s -> (a, s) }

IV. Monády
----------

Pro doplnění znalostí o monádách doporučuji:

* [You Could Have Invented Monads!](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
* Learn You a Haskell - kapitoly [11](http://learnyouahaskell.com/functors-applicative-functors-and-monoids), [12](http://learnyouahaskell.com/a-fistful-of-monads) a [13](http://learnyouahaskell.com/for-a-few-monads-more)

V. Závěrem
-----------

Hmm, proč ty závěry vlastně píšu? Jako vždy: vituscze@gmail.com
