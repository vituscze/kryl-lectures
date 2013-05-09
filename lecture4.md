Korekce 4. přednášky
====================

I. Operátor `(.)`
-----------------

Ačkoliv by se mohlo zdát, že ve výrazu `f . g` se nejprve provede funkce `g` a teprve pak až `f`, je to přesně opačně. Pokud funkce `f` skutečně potřebuje ihned vyhodnotit svůj první argument, pak se jistě ihned zavolá funkce `g`.

    -- ignoruje druhý argument
    const :: a -> b -> a
    const x _ = x

    -- pro jakýkoliv argument se zacyklí
    boom :: a -> b
    boom x = boom x


    (const 4 . boom) undefined == 4
    -- const 4 (boom undefined)

Zde je vidět, že pokud by se nejprve prováděla funkce `boom`, tak by celý výpočet divergoval, ale `const 4` ignoruje svůj arugment, proto tento výpočet skončí úspěšně.

II. Typové třídy
----------------

V deklaracích typových tříd není třeba uvádět explicitně kontext pro konkrétní funkce, je totiž implikován deklarací:

    class Q a where
        f :: Q a => a -> Int

    class Q a where
        f :: a -> Int
    -- kontext (Q a) je implikován deklarací typové třídy

III. Monády
-----------

### III.I. Typová třída

Je důležité uvědomit si, že kromě syntaktického pozlátka ve formě `do` nejsou monády ničím výjimečné. Podívejme se na typovou třídu `Monad`:

    class Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b

        -- ostatní funkce již nejsou tak podstatné
        (>>)   :: m a -> m b -> m b
        m1 >> m2 = m1 >>= \_ -> m2

        fail :: String -> m a

Na této deklaraci je něco zvláštního: obyčejně máme typové třídy pro nějaké konkrétní typy, např. `Int`, `[a]`, `Char` atp. Pokud bychom se ale pokusili definovat instanci např.:

    instance Monad Int where
        -- ...

funkce by měly podivný (invalidní) typ:

    return :: a -> Int a

Typová třída `Monad` tedy neočekává nějaký konkrétní typ, ale typový konstruktor s jedním parametrem!

    data Maybe a = Nothing | Just a -- typový konstruktor s jedním parametrem

    instance Monad Maybe where
        -- return :: a -> Maybe a
        return = Just

        -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
        Nothing >>= _ = Nothing
        Just x  >>= f = f x

Typové proměnné `a` a `b` uvnitř deklarace pak jen určují, že `return` a `(>>=)` jsou schopné pracovat s libovolnými typy `a` a `b`.

* * *

Pokud zavedeme kvantifikaci typových proměnných explicitně (např. pragma `ExplicitForAll`), pak dostáváme trochu výstižnější deklaraci:

    class Monad m where
        return :: forall a.   a -> m a
        (>>=)  :: forall a b. m a -> (a -> m b) -> m b

* * *

Klíčový poznatek je, že Haskell umožňuje deklarovat typové třídy nejen pro konkrétní typy, ale i pro typové konstruktory. Nic vám nebrání napsat:

    class Category arr where
        id  :: arr a a
        (.) :: arr b c -> arr a b -> arr a c

    -- definováno v modulu Control.Category

### III.II. IO

Spíše než na pohádku o typu:

    IO a = RealWorld -> (a, RealWorld)

se dívejte na `IO` jako na abstraktní datový typ reprezetující výpočty s vedlejšími efekty. Tedy, `IO Int` je výpočet, který může provést nějaké I/O a eventuálně vyprodukuje hodnotu typu `Int`.

Je důležité si uvědomit, že nelze napsat funkci typu `:: IO a -> a`, která by nám umožnila vzít nějaký výpočet, který provádí I/O, a tvrdit o něm, že ve skutečnosti žádné I/O neprovádní.

Tohle je určitém smyslu featura, protože nám Haskell umožňuje na úrovni typů separovat funkce, které žádný vedlejší efekt mít nemohou od potenciálně "nebezpečných" (přeci jen s I/O můžete dělat takřka cokoliv) funkcí.

Jak se tedy s `IO` pracuje? Základ tvoří předdefinované `IO` operace:

    getLine  :: IO String
    putStr   :: String -> IO ()
    putStrLn :: String -> IO ()
    getChar  :: IO Char
    putChar  :: Char -> IO ()
    -- a mnoho dalších

    -- podívejme se na typovou signaturu IO String
    -- je to výpočet, který za pomoci I/O eventuálně vyprodukuje String
    -- což je přesně to, co bychom od I/O operace očekávali

* * *

Poznámka: co je vlastně `()`? Je to tzv. "unit", "empty tuple" nebo chcete-li "prázdná n-tice". Jedná se o speciální datový typ, který má právě jednu hodnotu. `()` je zabudovaný typ, ale pokud bychom ho mohli definovat, vypadal by takto:

    data () = () -- jediná hodnota ()

Protože má právě jednu (triviální) hodnotu, používá se obdobně jako `void` v imperativních jazycích.

* * *

Pokud chcete svůj program zkompilovat, musíte definovat speciální hodnotu `main` typu `IO ()`.

    main :: IO ()
    main = putStrLn "Hello world"

    -- definujeme main jako akci putStrLn "Hello world"

Nyní je ale jasné, že pouze s předdefinovanými I/O operacemi bychom se daleko nedostali. Potřebujeme způsob, jak tyto operace slepovat do větších operací. A přesně tohle je chvíle, kdy se hodí monády.

Základní slepovací operátor je `(>>=)`. Prozkoumejme znovu typ tohoto operátoru (specializovaného pro `IO`):

    (>>=) :: IO a -> (a -> IO b) -> IO b

Prvním argumentem je tedy nějaký I/O výpočet, který eventuálně vyprodukuje `a`-čko, druhým argumentem je funkce, která říká, co s tímto `a`-čkem máme dělat.

Implementace je nasnadě: provedeme první I/O akci, její výsledek (nějaké `a`) předáme funkci `:: a -> IO b` a provedeme akci `:: IO b`, v pseudokódu:

    (>>=) ioakce funkce =
        let a       = proveď ioakci
            ioakce2 = funkce a
        in proveď ioakci2

Tohle nám umožňuje napsat první I/O, které dělá něco více než předdefinované operace:

    echo :: IO ()
    echo = getLine >>= ?

Na místo otazníku přijde funkce, která vezme řetězec typu `String` a provede s ním další `IO` akci. Když se podíváme na `putStrLn :: String -> IO ()`, je vidět, že tyto typy přesně souhlasí:

    echo :: IO ()
    echo = getLine >>= putStrLn

Pokud bychom chtěli s řetězcem dělat něco více než jen vypsat, například přidat vykřičník:

    putStrLnWithExcl :: String -> IO ()
    putStrLnWithExcl s = putStrLn (s ++ "!")

Definovat si funkci pro každé použití operátoru `(>>=)` by bylo značně nepraktické, proto odteď budeme používat lambda výrazy:

    echoWithExcl :: IO ()
    echoWithExcl = getLine >>= \s -> putStrLn (s ++ "!")

Umíme již kombinovat dvě `IO` akce, zkusme si napsat akci typu `:: IO (String, String)`, která přečtě dvě řádky ze vstupu a vrátí je jako dvojici:

    getTwoLines :: IO (String, String)
    getTwoLines = getLine >>= \line1 ->
                  getLine >>= \line2 ->
                  ???

* * *

Všimněte si, že přechozí výraz se parsuje přesně tak, jak potřebujeme, tedy:

    getLine >>= (\line1 ->
      getLine >>= (\line2 ->
        ???))

Nicméně zápis bez závorek je poněkud přehlednější.

* * *

Pokud se podíváte na typ operátoru `(>>=)`, je jasné, že na místo otazníků musí přijít nějaký výraz typu `IO b` pro nějaké `b`. Ale my už nechceme provádět žádné I/O, co s tím?

Teď přichází na řadu druhá funkce typové třídy `Monad`, tedy `return :: a -> IO a`. Nenechte se zmást jménem, `return` není jako `return` imperativních jazyků, speciálně pak neukončuje průběh nějakého výpočtu.

`return` dělá velice jednoduchou věc: vezme hodnotu a vytvoří z ní I/O akci, která nic nedělá (a jejím výsledkem je daná hodnota). To je ale přesně to, co tady potřebujeme!

    getTwoLines :: IO (String, String)
    getTwoLines = getLine >>= \line1 ->
                  getLine >>= \line2 ->
                  return (line1, line2)

Výsledek dané I/O akce můžeme samozřejmě ignorovat, např:

    printTwoLines :: String -> String -> IO ()
    printTwoLines line1 line2 =
        putStrLn line1 >>= \_ ->
        putStrLn line2

Tedy, pomocí operátoru `(>>=)` slepíme dvě `IO` akce, ale výsledek první jednoduše ignorujeme. Toto je samozřejmě častý požadavek, proto Haskell nabízí operátor `(>>)`, který provede první akci, výsledek ignoruje a provede druhou akci.

    akce1 >> akce2 = akce1 >>= \_ -> akce2

Pořád je tu ale takový nepříjemný problém: celý tento zápis je poněkud kostrbatý. Přesně kvůli tomu nám Haskell nabízí klíčové slovo `do`, které toto slepování akcí řeší za nás.

    getLine >>= \x -> -- ...
    -- se zapíše jako
    x <- getLine

    putStrLn "Name:" >> getLine >>= \x -> -- ...
    -- se zapíše jako
    putStrLn "Name:"
    x <- getLine

Tedy například naše `IO` akce na načítání dvou řetězců by mohla vypadat takto:

    getTwoLines = do
        line1 <- getLine
        line2 <- getLine
        return (line1, line2)

Přidávání vykřičníku:

    echoWithExcl = do
        line <- getLine
        putStrLn (line ++ "!")

Vypsání dvou řetězců:

    printTwoLines line1 line2 = do
        putStrLn line1
        putStrLn line2

Je taky dobré si uvědomit, že `do` slouží ke slepování akcí; pokud není co slepovat, `do` nedělá nic:

    printRev str = do
        putStrLn (reverse str)

    -- je ekvivalentní

    printRev str = putStrLn (reverse str)

Pro pohodlí programátora umožňují `do` bloky používat speciální formu `let` určenou pro lokální definice:

    echoWithExcl = do
        str <- getLine
        let excl        = "!"
            strWithExcl = str ++ excl
        putStrLn strWithExcl

Lokální definice jsou viditelné od místa deklarace až do konce `do` bloku.

Podívejme se na tuto funkci:

    getLinePrompt prompt = do
        putStrLn prompt
        s <- getLine

Vidíte, kde je problém? Pokud zapomeneme, že nějaké `do` bloky máme, tak je tento výraz ekvivalentní:

    getLinePrompt prompt =
        putStrLn prompt >>= \_ ->
        getLine >>= \s ->

Poslední lambda výraz nemá tělo! Přesně proto musí být posledním výrazem v `do` bloku nějaká `IO` akce.

Stejně jako u `(>>=)` je výsledkem celé `IO` akce výsledek posledního výrazu, tak obdobně v `do` bloku je výsledkem celé akce výsledek poslední řádky. Je tedy možné napsat:

    ask :: IO String
    ask = do
        putStrLn "What is your favourite colour?"
        answer <- getLine
        return answer

Ale jednodušší a přehlednější je psát:

    ask = do
        putStrLn "What is your favourite color?"
        getLine

* * *

Pokud I/O operace neuspěje, tak se ve většině případů vyhodí výjimka. Tuto výjimku je možno chytnout pouze z jiného `IO` bloku. Pokud by vás toto zajímalo více (nebo byste to potřebovali pro zápočtový program) napište mi mail a pokusím se dát něco dohromady.

* * *

### III.III. Axiomy monád

Abychom mohli prohlásit určitý typový konstruktor za monádu, musíme implementovat funkce `return` a `(>>=)`, které musejí splňovat určitou sadu axiomů.

    -- (1)
    return x >>= f == f x

    -- (2)
    f >>= return == f

    -- (3)
    (f >>= g) >>= h == f >>= \a -> g a >>= h

Pokud hodně přivřete obě oči, tak `(1)` a `(2)` říkají, že `return` je levá a pravá identita, `(3)` pak říká, že `(>>=)` je asociativní. Tyto axiomy dávájí trochu víc smysl, pokud se na ně podíváme pomocí `do`:

    -- (1)
    do
        x' <- return x
        f x'

    ==
    do
        f x

    -- (2)
    do
        x <- f
        return x
    ==
    do
        f

Axiom `(2)` nám říká, že vynechání `return` na konci je korektní a nic se tím nezmění; viz `ask` výše.

`(1)` a `(2)` spolu dávají přirozený požadavek, že `return` vrací právě tu hodnotu, na kterou byl aplikován a také to, že `return` jinak skutečně nic jiného nedělá.

    -- (3)
    do
        x <- do y <- a
                b y
        c x
    ==
    do
        y <- a
        x <- b y
        c x

Axiom `(3)` nám říká, že můžeme vzít kus `IO` bloku a separovat ho do vlastní akce tak, že význam programu zůstane stejný. Praktický příklad osvětlí, k čemu je tohle vlastně dobré:

    do
        putStrLn "What is your quest?"
        answer <- getLine
        when (answer == "To seek the Holy Grail.") $
            putStrLn "What is your favourite colour?"
        -- etc

Díky třetímu axiomu můžeme vzít první dva řádky `IO` bloku a vytvořit z nich separátní akci:

    askQuest = putStrLn "What is your quest?" >> getLine

    do
        answer <- askQuest
        when (anser == -- etc

Což se jistě bude hodit, pokud nějakou posloupnost `IO` akcí rádi používáte na více místech.

* * *

Pokud nejste spokojeni s vysvětlením, že tyto axiomy říkají cosi o tom, že `return` je identita a `(>>=)` je asociativní:

Definujme kompozici funkcí typu `a -> m b` pro nějakou monádu `m`:

    (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
    f <=< g = \x -> g x >>= f

Tyto axiomy se pak dají zapsat ve velice sugestivní formě:

    -- (1)
    f <=< return == f

    -- (2)
    return <=< f == f

    -- (3)
    f <=< (g <=< h) == (f <=< g) <=< h

* * *

### III.IV. Konkrétní monády

Jedním příkladem monády je tedy typový konstruktor `IO`, ten jsme však již podrobně rozebrali v předchozích částech, takže se o `IO` v této části nebudu zmiňovat.

#### III.IV.I `Identity`

Nejjednodušší monádou je typový konstruktor `Identity` definovaný takto:

    newtype Identity a = Identity { runIdentity :: a }

`Identity a` neobsahuje kromě hodnoty typu `a` nic jiného, takže je na první pohled patrné, že tato monáda nebude dělat nic zajímavého; uvádím ji zde pro úplnost.

    instance Monad Identity where
        return a = Identity a
        Identity a >>= f = f a

Můžete si ověřit, že tato monáda skutečně splňuje všechny axiomy.

#### III.IV.II `Maybe`

Pokud neuspěje pattern matching nebo se někde pokusíte vyhodnotit `undefined` či `error "..."` (a mnoho dalších případů), tak Haskell vyhodí výjimku. Z předchozích kapitol ale víme, že výjimky se dají chytat pouze v `IO` kódu. Nicméně v mnoha situacích by bylo užitečné zeptat se nějakého výrazu, jestli se výpočet podařil nebo ne. Přesně od toho je tu typ `Maybe` definovaný takto:

    data Maybe a = Just a | Nothing

Konstruktor `Just` reprezentuje úspěch s hodnotou typu `a` a `Nothing` pak reprezetuje neúspěch. Můžeme si tedy například napsat funkci `head`, která "nehavaruje" pro prázdné seznamy:

    headSafe :: [a] -> Maybe a
    headSafe []    = Nothing
    headSafe (x:_) = Just x

Po chvíli si však všimnete, že pracovat s podobnými funkcemi je mnohem nepohodlnější, protože vždy musíte explicitně kontrolovat, jestli se výsledek podařilo spočítat nebo ne.

    compute :: Int -> Maybe Int
    convert :: Int -> Maybe Char

    doWork :: [Int] -> Maybe Char
    doWork xs = case headSafe xs of
        Nothing -> Nothing
        Just x  -> case compute x of
            Nothing -> Nothing
            Just n  -> convert n

Na pomoc přicházejí (nečekaně) monády. Když se podíváte na předchozí kód, snažíme se vlastně slepit několik výpočtů, které v každém kromu mohou selhat. Zkuste napsat `instance Monad Maybe where` a ověřte, že dané axiomy skutečně platí!

Konkrétní instance je zde pro kontrolu uvedená:

    instance Monad Maybe where
        return = Just
        Nothing >>= _ = Nothing
        Just x  >>= f = f x

Všimněte si, že `Nothing` slouží jako jakýsi exception:

    throw :: Maybe a
    throw = Nothing

Stejně tak můžeme napsat `catch`:

    -- první argument reprezetuje výpočet, který může vyhodit "výjimku"
    -- druhý argument pak co dělat v případě, že výjimka nastala
    catch :: Maybe a -> Maybe a -> Maybe a
    catch Nothing e = e  -- výpočet skončil výjimkou
    catch x       _ = x  -- výpočet skončil v pořádku

Stejně jako pro `IO` dostáváme zadarmo `do` bloky. Náš předchozí příklad `doWork` se dá zapsat velice stručně jako:

    doWork xs = do
        x <- headSafe xs
        n <- compute x
        convert n

Pokud například zjistíme, že `convert` nechceme použít pokud `n == 0`, můžeme napsat jednoduše:

    doWork xs = do
        x <- headSafe xs
        n <- compute x
        when (n == 0) throw
        convert n

* * *

Funkce `when` je definovaná v standardních knihovnách:

    when :: Monad m => Bool -> m () -> m ()
    when True  action = action
    when False _      = return ()

Tj. `when cond action` provede akci `action` pouze pokud je podmínka `cond` splněná.

* * *

#### III.IV.III `Either`

`Maybe` nám umožňuje ohlašovat neúspěšné výpočty, ale o typu chyby, která při výpočtu nastala, nemáme ani zdání. Přidejme tedy k datovému konstruktoru `Nothing` informaci o typu chyby:

    data Either e a = Left e | Right a
    -- Left e znamená neúspěch se zprávou typu e
    -- Right a znamená úspěch s výsledkem typu a

Pokud chceme implemenovat instanci `Monad` pro `Either` narazíme na problém: `Either` je typový konstruktor, který očekává dva parametry - my však potřebujeme typový konstruktor s jedním parametrem. Řešení je jednoduché, `Either` aplikujeme na nějaký typ.

Následující kód definuje instanci `Monad` pro typový konstruktor `Either e` a libovolné `e`:

    instance Monad (Either e) where
        return = Right
        Left e  >>= _ = Left e
        Right a >>= f = f a

Funkcím `throw` a `catch` nyní můžeme dát poněkud lepší typy:

    throw :: e -> Either e a
    throw exc = Left exc

    catch :: Either e a -> (e -> Either e a) -> Either e a
    catch (Left ex) handle = handle ex
    catch x         _      = x

Přepišme původní `doWork` za pomoci `Either`:

    compute :: Int -> Either String Int
    convert :: Int -> Either String Char

    headSafe :: [a] -> Either String a
    headSafe []    = throw "List was empty!"
    headSafe (x:_) = return x

    doWork :: [Int] -> Either String Char
    doWork xs = do
        x <- headSafe xs
        n <- compute x
        when (n == 0) $ throw "n was zero!"
        convert n

#### III.IV.IV `[]`

Další velice zajímavou strukturou, která připouští monádu, jsou seznamy. Na nějaký seznam typu `[a]` se můžeme dívat jako na nedeterministickou hodnotu typu `a`. Pokud bychom například chtěli definovat hodnotu typu `Int`, která může (nedeterministicky) nabývat dvou hodnot:

    x :: [Int]
    x = [1, 2]

Pak také můžeme mít funkce, které produkují nedeterministické hodnoty, například odmocnina komplexních čísel:

    cSqrt :: Complex Double -> [Complex Double]
    cSqrt c = -- ...

    -- 1.0 :+ 0.0 == 1 + 0i
    cSqrt (1.0 :+ 0.0) == [1.0 :+ 0.0, -1.0 :- 0.0]

V tomto případě je `cSqrt` funkce, která (nedeterministicky) produkuje oba dva kořeny. Jak bychom pomocí `cSqrt` definovali 4. odmocninu?

    cSqrt4 :: Complex Double -> [Complex Double]
    cSqrt4 x = cSqrt x -- ??

Potřebujeme vzít výsledek `cSqrt` a aplikovat `cSqrt` na každý prvek: tím dostaneme `[[Complex Double]]`, na který můžeme použít funkci `concat :: [[a]] -> [a]`.

    cSqrt4 x = concat (map cSqrt (cSqrt x))

Můžeme od tohoto abstrahovat? Asi už víte kam tím mířím:

    instance Monad [] where
        return x = [x]
        m >>= f  = concat (map f m)

`return` v tomto případě říká, že každá deterministická hodnota je triviálně nedeterministická (vybíráme pouze z jedné hodnoty).

Jak bychom provedli například kartézský součin dvou seznamů?

    cross xs ys = do
        x <- xs
        y <- ys
        return (x, y)

Co takhle `map`u pomocí `do` bloku?

    map' f xs = do
        x <- xs
        return (f x)

Kde se v tomhle schovává nedeterminismus? Když napíšete `x <- xs`, tak se do `x` postupně dosadí každá hodnota seznamu `xs`. Všimněte si, že tohle pracuje na stejném principu jako:

    map'' f xs = [f x | x <- xs]

A skutečně! `(>>=)` nám dává generátory a `return` nám dává výraz před svislítkem, potřebujeme ještě možnost ověřovat podmínky. Pokud se v nějakém kroku objeví prázdný seznam, tak pro tuto větev výpočtu není co počítat (takže se `[]` chová v jistém smyslu jako lokální `Nothing`), můžeme tedy selektivně ořezávat výpočty, které nevedou k cíli:

    fail :: [a]
    fail = []

    pyth :: [(Int, Int, Int)]
    pyth = do
        c <- [1..100]
        b <- [1..c]
        a <- [1..b]
        when (c^2 /= a^2 + b^2) fail
        return (a, b, c)

Toto funguje díky tomu, že `map f [] == []` pro libovolnou funkci `f`.

#### III.IV.V. `Reader`

Pokud náš program používá nějaký konfigurační soubor, který si vždy při spuštění načte a podle něj se odvíjí chování některých funkcí, například:

    data Config = Config { caseSensitive :: Bool }
        deriving (Show, Read)

    main = do
        configString <- readFile "config.ini"
        let config = read configString
        ...

    strEq :: Config -> String -> String -> Bool
    strEq c s1 s2
        | caseSensitive c = -- case sensitive rovnost
        | otherwise       = -- etc.

    shouldExit :: Config -> String -> Bool
    shouldExit c s = strEq c s "exit"

Je vidět, že když chceme použít nějakou funkci, jejíž chování je závislé na našem konfiguračním souboru, musíme `Config` neustále předávat dalším a dalším funkcím. Ve chvíli, kdy jedna hluboko zanořená funkce vyžaduje `Config`, všechny funkce, které ji volají musejí taky pracovat s `Config`em. Od tohoto však můžeme abstrahovat:

    newtype Reader e a = Reader { runReader :: e -> a }

`Reader e a` tedy reprezenuje nějaký výpočet, který produkuje nějakou hodnotu typu `a` za pomoci "globálního" stavu typu `e`. V našem případě bychom použili `Reader Config`.

    instance Monad (Reader e) where
        return a = Reader $ \_ -> a
        Reader m >>= f = Reader $ \e -> runReader (f (m e)) e

`return` říká, že hodnotu, kterou jsme vytvořili bez pomoci globálního stavu, můžeme jednoduše vnořit do výpočtu s globálním stavem tak, že tento stav jednoduše ignorujeme.

`(>>=)` pak jednoduše předává stav jak prvnímu, tak druhému výpočtu.

Ještě by se nám hodila jedna funkce: vyřešili jsme distribuci stavu všem výpočtům, ale uvnitř výpočtu nemáme možnost zeptat se, jak stav vlastně vypadá:

    ask :: Reader e e
    ask = Reader $ \e -> e

Nyní můžeme přepsat `strEq` z posledního příkladu:

    strEq :: String -> String -> Reader Config Bool
    strEq s1 s2 = do
        c <- ask
        if caseSensitive c
            then return (s1 == s2)
            else return (map toLower s1 == map toLower s2)

Případně si ještě můžeme dovolit zlepšovák:

    asks :: (e -> a) -> Reader e a
    asks f = Reader $ \e -> f e
    -- nebo také
    -- asks = Reader

    strEq s1 s2 = do
        cond <- asks caseSensitive
        -- ...

Celý takovýto výpočet se pak "spustí" pomocí funkce (projekce) `runReader` definované spolu s typem `Reader`:

    -- runReader :: Reader e a -> e -> a
    compute :: Reader Config String

    main = do
        configString <- readFile "config.ini"
        let config = read configString
            result = runReader compute config
        putStrLn result

#### III.IV.VI. `Writer`

Už umíme reprezentovat výpočty, které mohou selhat, nedeterminismus a výpočty, které čtou z "globálního" neměnného stavu. Přidejme ještě výpočty, které mohou produkovat nějaký pomocný výstup.

Velice jednoduché debugování můžeme napsat tak, že kromě své obvyklé hodnoty bude produkovat ještě nějaký `String`:

    addOne :: Int -> (Int, String)
    addOne n = (n + 1, "Added one!")

    toChar :: Int -> (Char, String)
    toChar n = (chr n, "Converted to Char!")

Co kdybyhom chtěli přidat jedničku a výsledek konvertovat na znak? Kromě toho, že si předáme výsledky, musíme ještě nějak zkombinovat debug výstupy:

    addOneAndConvert :: Int -> (Char, String)
    addOneAndConvert n
        let (m, debug1) = addOne n
            (c, debug2) = toChar m
        in  (c, debug1 ++ debug2)

Opět je vidět určitý pattern, od kterého můžeme abstrahovat:

    newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }

    instance Monad StringWriter where
        return a = StringWriter (a, "")
        StringWriter (a, debug1) >>= f =
            let StringWriter (b, debug2) = f a
            in  StringWriter (b, debug1 ++ debug2)

`return a` vrací `a` a žádný debug výstup neprodukuje. Ještě by se nám hodila funkce, která umožňuje debug výstup jednoduše produkovat:

    tell' :: String -> StringWriter ()
    tell' s = StringWriter ((), s)

A teď již můžeme přepsat předchozí příklad:

    addOne :: Int -> StringWriter Int
    addOne n = do
        tell' "Added one!"
        return (n + 1)

    toChar :: Int -> StringWriter Char
    toChar n = do
        tell' "Converted to Char!"
        return (chr n)

    addOneAndConvert :: Int -> StringWriter Char
    addOneAndConvert n = do
        m <- addOne
        toChar m

Pokud se pokusíte dokázat, že pro tuto konkrétní monádu platí všechny axiomy, všimněte si, že potřebujete využít toho, že `[]` je neutrální prvek vůči `(++)`, tedy:

    -- (1)
    [] ++ x  == x

    -- (2)
    x  ++ [] == x

A také, že `(++)` je asociativní:

    -- (3)
    x ++ (y ++ z) == (x ++ y) ++ z

To nám napovídá, že `StringWriter` by mohl fungovat pro více než jen `String`. Struktura, která splňuje tyto tři axiomy se nazývá monoid. Je to taková redukovaná grupa, které chybí inverzní prvek. Haskell nabízí typovou třídu pro monoidy v modulu `Data.Monoid`:

    class Monoid m where
        mempty  :: m
        mappend :: m -> m -> m

`mempty` je neutrální prvek (`[]` v případě seznamů) a `mappend` je binární operace (`(++)` v případě seznamů). Typová třída `Monoid` nikde nevynucuje, aby dané axiomy platily, ale každá implementace by se jimi měla řídit.

Nyní můžeme definovat obecnější `StringWriter`:

    newtype Writer w a = Writer { runWriter :: (a, w) }

    instance Monoid w => Monad (Writer w) where
        return a = Writer (a, mempty)
        Writer (a, w1) >>= f =
            let Writer (b, w2) = f a
            in  Writer (b, w1 `mappend` w2)

    tell :: w -> Writer w ()
    tell w = Writer ((), w)

Kromě seznamů tvoří monoid například celá čísla (`Integer`) s nulou a sčítáním. Zde je například nepříliš šikovný způsob, jak určit počet výskytů daného prvku v seznamu:

    count :: Eq a => a -> [a] -> Integer
    count what = getSum . snd . runWriter . action
      where
        action [] = return ()
        action (x:xs)
          | x == what = tell (Sum 1) >> action xs
          | otherwise =                 action xs

* * *

K čemu je tady `Sum`? Celá čísla připouští (alespoň) dva monoidy, jeden s nulou a sčítáním, druhý s jedničkou a násobením. Pro rozlišení, který z nich máme vlastně na mysli se používají datové typy:

    newtype Sum a = Sum { getSum :: a }
    newtype Product a = Product { getProduct :: a }

* * *
