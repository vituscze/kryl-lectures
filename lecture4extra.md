I. Typová třída `Monad`
=======================

Ve 4. korekci jsou detailně rozebrané funkce `return` a `(>>=)`, ale typová třída `Monad` je trochu rozsáhlejší:

    class Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b

        (>>)   :: m a -> m b -> m b
        fail   :: String -> m a

        m1 >> m2 = m1 >>= \_ -> m2
        fail     = error

`(>>)` je také celkem slušně popsán v korekci, ale ještě je tu funkce `fail`, jejíž defaultní implementace je poněkud podivná. K čemu tato funkce slouží?

`do` bloky se sice překládájí na volání funkcí `return`, `(>>=)` a `(>>)`, ale možná jste si všimli, že `do` bloky umožňují pattern matching:

    f xs = do
        Just x <- xs
        return x

Jednoduchý překlad není příliš ideální, protože ve většině případů bude produkovat parciální funkce:

    f xs = xs >>= \(Just x) -> return x

Jakmile se někde objeví `Nothing`, funkce `f` selže s nepěknou hláškou. Typová třída `Monad` nám tedy dává možnost specifikovat, co se v takovýchto případech má dít. Než se podíváme, jak nám `fail` může pomoct, podívejme se na to, jak se `do` notace skutečně převádí:

    do {e}                = e
    do {e; stmts}         = e >> do {stmts}
    do {p <- e; stmts}    = let ok p = do {stmts}
                                ok _ = fail "implementation defined"
                            in  e >>= ok
    do {let decls; stmts} = let decls in do {stmts}

Tady je vidět, kde se přesně uplatní funkce `fail`: místo použití parciální funkce definujeme totální funkci `ok`, která v případě, že pattern nepasuje, zavolá funkci `fail` (čímž nám dává možnost ošetřit tyto případy).

Některé monády jsou schopny implementovat `fail` tak, že k výjimce nedojde a pouze se přeruší daný výpočet (dle kontextu monády). Jedním takovým případem je `Maybe`:

    instance Monad Maybe where
        -- ...

        fail _ = Nothing

Pokud se v `Maybe` `do` bloku nepodaří pattern matching, celý výpočet se jednoduše vyhodnotí na `Nothing` (viz `throw` v korekci, část III.IV.II).

Stejně tak `[]` monáda podporuje rozumný `fail`:

    instance Monad [] where
        -- ...

        fail _ = []

Pro `Either` můžeme `fail` implementovat jen částečně:

    instance Monad (Either e) where
        -- ...

        fail e = Left (error e)

Tím se sice dozvíme, že k chybě došlo, ale ve chvíli, kdy se pokusíme podívat na chybu uloženou v konstruktoru `Left`, tak celý výpočet diverguje. Problém je ten, že `e` není `String`. Nicméně, občas by se nám hodilo si nějakým způsobem poznamenat výjimku, která nastala při volání `fail`:

    class ErrorMsg a where
        noMsg  :: a
        strMsg :: String -> a

    newtype Error e a = Error { runError :: Either e a }

    instance ErrorMsg e => Monad (Error e) where
        -- ...

        fail = Error . Left . strMsg

Typová třída `ErrorMsg` označuje typy, které podporují generování defaultní chybové zprávy a také převádění `String`u na chybovou zprávu:

    instance ErrorMsg String where
        noMsg  = ""
        strMsg = id

Nicméně, pro mnoho monád neexistuje rozumná implementace funkce `fail`.
