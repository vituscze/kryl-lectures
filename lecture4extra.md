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

II. Monad transformers
======================

Monády nám umožňují popsat mnoho různých typů výpočtů, ale zatím neumíme kombinovat efekty různých výpočtů do jednoho. Co se tím myslí? Pokud bychom chtěli kombinovat lokální stav (`State s`) s možností chyby (`Either e`), tak si musíme napsat vlastní monádu.

    newtype StateError s e a = StateError { runStateError :: s -> Either e (a, s) }

Pokud se nalézáme v situaci, kdy často potřebujeme používat různé efekty různých monád a vytvářet si speciální datový typ pro každou takovou příležitost je značně nepohodlné, nepřehledné či časově náročné, můžeme použít takzvaný monad transformer.

Monad transformery nám dávají jednoduchý nástroj jak kombinovat více monád dohromady. Místo obyčejného `State s` budeme mít `StateT s m`, kde `m` označuje další "vnitřní" monádu:

    newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

Místo `Either e` budeme mít `ErrorT e m`:

    newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

Pokud chceme tento řetěz ukončit, dáme na dno monádu, která nic nedělá:

    newtype Identity a = Identity { runIdentity :: a }

Náš předchozí typ by tedy mohl vypadat:

    type StateError s e a = StateT s (ErrorT e Identity) a

Instance se definují obdobně:

    instance Monad m => Monad (StateT s m) where
        return a = StateT $ \s -> return (a, s)
        StateT m >>= f = StateT $ \s1 -> do
            (a, s2) <- m s1
            runStateT (f a) s2

        fail s = StateT $ \_ -> fail s

    instance (ErrorMsg e, Monad m) => Monad (ErrorT e m) where
        return = ErrorT . return . Right
        ErrorT m >>= f = ErrorT $ do
            ea <- m
            case ea of
                Left e  -> return (Left e)
                Right a -> runErrorT (f a)

        fail = ErrorT . return . Left . strMsg

Všimněte se, že v definici `Monad (StateT s m)` používáme `fail` "spodní" monády, což nám umožňuje rozumně ukončit výpočet, pokud to spodní monáda podporuje.

Pokud si přidáme pár primitivních operací:

    throwError :: e -> EitherT e m a
    throwError = EitherT . return . Left

    get :: StateT s m s
    get = StateT $ \s -> return (s, s)

    put :: s -> StateT s m ()
    put s = StateT $ \_ -> return ((), s)

Na první pohled je vidět, že pokud je náš typ transformeru např. `StateT s (ErrorT e m) a`, tak sice můžeme použít `get` a `put`, ale `throwError` má "špatný" typ. Původním účelem ale bylo právě kombinování efektů, což nám tento problém znemožňuje.

Definujeme tedy třídu, která nám umožní vzít výpočet ve spodní monádě a vyzvednout ho do celého transformeru:

    class MonadTrans t where
        lift :: Monad m => m a -> t m a

Nyní můžeme definovat instanci pro `StateT` a `ErrorT`:

    instance MonadTrans (StateT s) where
        lift m = StateT $ \s -> do
            a <- m
            return (a, s)

    instance MonadTrans (ErrorT e) where
        lift m = ErrorT $ do
            a <- m
            return (Right a)

Díky `lift` tedy např. můžeme vzít `throwError` a vyzvednout ho do kontextu celého transformeru:

    run :: StateT s (ErrorT e Identity) a -> s -> Either e (a, s)
    run m = runIdentity . runErrorT . runStateT m

    f :: Int -> StateT Int (ErrorT String Identity) ()
    f x = do
        s <- get
        when (s == x) . lift . throwError $ "Some error"
        put x

    ghci> run (f 4) 4
    Left "Some error"

    ghci> run (f 4) 5
    Right ((),4)
