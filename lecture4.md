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

    class Category cat where
        id  :: cat a a
        (.) :: cat b c -> cat a b -> cat a c

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
    m >>= return == m

    -- (3)
    (m >>= f) >>= g == m >>= \x -> f x >>= g

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
        x <- m
        return x
    ==
    do
        m

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

Axiom `(3)` nám říká, že můžeme vzít kus `do` bloku a separovat ho do vlastní akce tak, že význam programu zůstane stejný. Praktický příklad osvětlí, k čemu je tohle vlastně dobré:

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
    cSqrt (1.0 :+ 0.0) == [1.0 :+ 0.0, -1.0 :+ 0.0]

V tomto případě je `cSqrt` funkce, která produkuje oba dva kořeny. Jak bychom pomocí `cSqrt` definovali 4. odmocninu?

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

    fail' :: [a]
    fail' = []

    pyth :: [(Int, Int, Int)]
    pyth = do
        c <- [1..100]
        b <- [1..c]
        a <- [1..b]
        when (c^2 /= a^2 + b^2) fail'
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

Velice jednoduché debugování můžeme napsat tak, že funkce budou kromě své obvyklé hodnoty produkovat ještě nějaký `String`:

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

#### III.IV.VII. `State`

`Reader` nám umožňuje ze stavu číst, ale nikoliv do něj psát; na druhou stranu `Writer` umožňuje psát, ale ne číst. Co kdybychom tyto dva přístupy zkombinovali a v každém kroku bychom mohli jak číst tak psát? Dostali bychom výpočet, který si vedle udržuje nějaký lokální stav.

Ideálním příkladem je prohledávání stavového prostoru. Naším lokálním stavem pak bude seznam již navštívených stavů; obdobně se také můžeme implementovat BFS - lokálním stavem pak bude fronta.

    bfs :: (s -> [s]) -> (s -> Bool) -> Maybe s
    bfs trans end []     = Nothing
    bfs trans end (x:xs) =
      | end x     = Just x
      | otherwise = bfs trans end (xs ++ trans x)

`trans` je v našem případě přechodová funkce, `end` je koncová podmínka a v třetím argumentu si předáváme frontu. Všimněte si, že v každém kroku čteme stav (frontu) a také stav měníme (zavoláme `bfs` na `xs ++ trans x`).

Takto bychom například očíslovali všechny uzly stromu jejich pořadím v DFS:

    data Tree a = Nil | Node a (Tree a) (Tree a)
        deriving (Show)

    label :: Tree a -> Tree Int
    label = fst . go 0
      where
        go n Nil = (Nil, n)
        go n (Node x l r) =
            let (l', nl) = go (n + 1) l
                (r', nr) = go nl r
            in  (Node n l' r', nr)

Pomocná funkce `go` dostává ve svém prvním argumentu momentální pozici v prohledávání a vyprodukuje nový strom a novou pozici. Pokud je strom prázdný, není co označovat, vrátíme prázdný strom a pozici nezměníme. Pokud je strom neprázdný, tak zavoláme `go` na levý podstrom (s pozicí o jedna větší - momentální pozice přijde do právě zpracovávaného vrcholu), ta nám vrátí označený levý podstrom a novou pozici, atp.

Sloučením `Writer`u a `Reader`u se nabízí abstrakce, která by tento problém elegantně řešila:

    newtype State s a = State { runState :: s -> (a, s) }

    instance Monad (State s) where
        return a = State $ \s -> (a, s)
        State m >>= f = State $ \s1 ->
            let (a, s2) = m s1
                (b, s3) = runState (f a) s2
            in  (b, s3)
    -- nebo jednodušeji:
    --  State m >>= f = State $ \s1 ->
    --      let (a, s2) = m s1
    --      in  runState (f a) s2

`return` prostě stav `s` předá dál aniž by něco měnil. `(>>=)` slepí dva stavové výpočty takto: vytvoří nový stavový výpočet, který bere nějaký stav `s1`. Pomocí stavu `s1` pustíme první akci `m` - ta nám vrátí hodnotu `a` a nový stav `s2`. Nyní můžeme aplikovat funkci `f` na `a` a tím dostaneme nový stavový výpočet, který pustíme se stavem `s2`.

Kromě toho ještě potřebujeme přístup k momentálnímu stavu a taky možnost, jak stav změnit:

    get :: State s s
    get = State $ \s -> (s, s)

    put :: s -> State s ()
    put s = State $ \_ -> ((), s)

    modify :: (s -> s) -> State s ()
    modify f = State $ \s -> ((), f s)

`get` prostě vezme momentální stav, předá ho dál a zároveň ho vrátí jako výsledek. `put s` pak stav ignoruje a nahradí jej novým stavem `s`. `label` můžeme napsat takto:

    label :: Tree a -> Tree Int
    label t = fst $ runState (go t) 0
      where
        go Nil = return Nil
        go (Node x l r) = do
            n <- get
            put (n + 1)
            l' <- go l
            r' <- go r
            return (Node n l' r')

#### III.IV.VIII. Další monády

Pro zajímavost uvadím další hrstku monád, tentokrát již bez detailní motivace a použití.

##### III.IV.VIII.I. `Cont`

`Cont` (continuation) monáda slouží pro popis výpočtů, které mohou být přerušeny a poté znovu spuštěny. Podobně jako `Either` se dá použít pro zpracovávání chyb.

    newtype Cont r a = Cont { runCont :: (a -> r) -> r }

    instance Monad (Cont r) where
        return a = Cont $ \k -> k a
        Cont m >>= f = Cont $ \k ->
            m (\a -> runCont (f a) k)

    callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
    callCC e = Cont $ \k -> runCont (e (\a -> Cont $ \_ -> k a)) k

##### III.IV.VIII.II. `RWS`

`RWS` je kombinace monád `Reader`, `Writer` a `State`:

    newtype RWS r w s a = RWS { runRWS :: r -> s -> (a, s, w) }

    instance Monoid w => Monad (RWS r w s) where
        return a = RWS $ \_ s -> (a, s, mempty)
        RWS m >>= f = RWS $ \r s1 ->
            let (a, s2, w1) = m r s1
                (b, s3, w2) = runRWS (f a) r s2
            in  (b, s3, w1 `mappend` w2)

    tell :: w -> RWS r w s ()
    tell w = RWS $ \_ s -> ((), s, w)

    ask :: Monoid w => RWS r w s r
    ask = RWS $ \r s -> (r, s, mempty)

    put :: Monoid w => s -> RWS r w s ()
    put s = RWS $ \_ _ -> ((), s, mempty)

    get :: Monoid w => RWS r w s s
    get = RWS $ \_ s -> (s, s, mempty)

##### III.IV.VIII.III. `Dist`

`Dist` reprezentuje distribuci náhodné veličiny:

    newtype Dist a = Dist { runDist :: [(a, Rational)] }

    instance Monad Dist where
        return a = Dist [(a, 1)]
        Dist m >>= f = Dist $ concatMap
            (\(a, r) -> map (\(b, r2) -> (b, r * r2)) (runDist (f a))) m

##### III.IV.VIII.IV. `ST`

`ST` monáda slouží k popisu imperativních výpočtů, které pracují s in-place mutací. Na úrovni typů je garantováno, že žádný vedlejší efekt neproklouzne ven z výpočtu.

##### III.IV.VIII.V. `STM`

`STM` neboli software transactional memory je alternativní přístup k synchronizaci vláken. Narozdíl od klasických "lock" přístupů `STM` naprosto vylučuje deadlock.

##### III.IV.VIII.VI. `RState`

`RState` je `State`, ve kterém se stav předává v obráceném pořadí:

    newtype RState s a = RState { runRState :: s -> (a,s) }

    instance Monad (RState s) where
        return a = RState (\s -> (a,s))
        RState m >>= f = RState $ \s1 ->
            let (a, s3) = m s2
                (b, s2) = runRState (f a) s1
            in  (b, s3)

##### III.IV.VIII.VII. "Free" monáda

    data Free f a
        = Pure a
        | Roll (f (Free f a))

    instance Functor f => Monad (Free f) where
        return = Pure
        Pure a >>= f = f a
        Roll m >>= f = Roll $ fmap (>>= f) m

### III.V. Parsování pomocí monád

Jeden velice zajímavý příklad využití monád je parsování. Na tuto část se můžete dívat jako na řešený příklad stavby vlastní monády. Ve standardní knihovně se takováto monáda nenachází, ale součástí knihoven, které se distribuují v rámci Haskell Platform, je knihovna parsec, která nabízí podobné rozhraní.

Náš parser bude vlastně funkce, která bere text, pokusí se něco naparsovat a buď neuspěje nebo vrátí hodnotu a zbytek nenaparsovaného textu:

    newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

Sekvenční parsování za nás bude řešit `do` notace, napišme tedy instanci třídy `Monad`:

    instance Monad Parser where

Pro `return a` požadujeme, aby náš parser vždy uspěl s hodnotou `a` a nezkonzumoval žádný text:

        return a = Parser $ \text -> Just (a, text)

`(>>=)` bude sekvenčně spojovat dva parsery: pokud se první parser nepodaří, tak selže celý kombinovaný parser, pokud se první podaří, tak zkusíme provést druhý parser:

        Parser m >>= f = Parser $ \text1 ->
            case m text1 of
                Nothing -> Nothing  -- první parser neuspěl
                Just (a, text2) -> runParser (f a) text2

Ověříme, že všechny axiomy skutečně platí:

    -- (1)
    m >>= return
    ==
    \text1 -> case m text1 of
        Nothing -> Nothing
        Just (a, text2) -> return a text2
    ==
    \text1 -> case m text1 of
        Nothing -> Nothing
        Just (a, text2) -> Just (a, text2)
    ==
    \text1 -> m text1
    == -- eta redukce
    m

    -- (2)
    return x >>= f
    ==
    (\t -> (x, t)) >>= f
    ==
    \text1 -> case (\t -> Just (x, t)) text1 of
        Nothing -> Nothing
        Just (a, text2) -> f a text2
    ==
    \text1 -> case Just (x, text1) of
        Nothing -> Nothing
        Just (a, text2) -> f a text2
    ==
    \text1 -> f x text1
    ==
    f x

    -- (3)
    (m >>= f) >>= g
    ==
    \text1 -> case (m >>= f) text1 of
        Nothing -> Nothing
        Just (b, text2) -> g b text2
    ==
    \text1 -> case (
            \text3 -> case m text3 of
                Nothing -> Nothing
                Just (a, text4) -> f a text4
          ) text1 of
        Nothing -> Nothing
        Just (b, text2) -> g b text2
    ==
    \text1 -> case (
            case m text1 of
                Nothing -> Nothing
                Just (a, text4) -> f a text4
          ) of
        Nothing -> Nothing
        Just (b, text2) -> g b text2

    -- výraz to každopádně není pěkný
    -- uvažme dvě alternativy, které mohou nastat

    -- 1. m text1 je Nothing
    -- potom se celý výraz redukuje na
    \text1 -> Nothing

    -- a
    m >>= (\x -> f x >>= g)
    -- je zjevně také Nothing, takže tady jsme v pořádku

    -- 2. m text1 je Just (x, t)
    \text1 -> case f x t of
        Nothing -> Nothing
        Just (b, text2) -> g b text2

    -- a co na druhé straně?
    -- po troše rozbalování definic dostaneme
    \text1 -> case m text1 of
        Nothing -> Nothing
        Just (a, text2) -> case f a text2 of
            Nothing -> Nothing
            Just (b, text3) -> g b text3

    -- nyní použijeme náš předpoklad, že m text1 == Just (x, t)
    \text1 -> case f x t of
        Nothing -> Nothing
        Just (b, text3) -> g b text3

    -- až na jména proměnných dostáváme stejný výraz, QED

Kromě sekvenční kompozice parserů potřebujeme výběr mezi několika parsery: jestli se první nepodaří, zkus druhý. A aby se nám s tím lépe pracovalo, přidáme také parser, který nikdy neuspěje.

    infixr 1 <|>

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser p1 <|> Parser p2 = Parser $ \t ->
        case p1 t of
            Just x -> Just x  -- první parser uspěl, nemusíme zkoušet druhý
            Nothing -> p2 t   -- první neuspěl, zkusme druhý

    fail' :: Parser a
    fail' = Parser $ \_ -> Nothing

Teď můžeme pomocí těchto funkcí napsat pár kombinátorů:

    -- výběr z několika parserů; použije první, který uspěje
    choice :: [Parser a] -> Parser a
    choice = foldr (<|>) fail'

    -- naparsuje nula a více a-ček
    many :: Parser a -> Parser [a]
    many p = some p <|> return []

    -- jeden a více
    some :: Parser a -> Parser [a]
    some p = do
        a  <- p
        as <- many p
        return (a:as)

    -- zkusí použít parser p; pokud ten selže, vrátí hodnotu a
    option :: a -> Parser a -> Parser a
    option a p = p <|> return a

A několik základních parserů:

    cond :: (Char -> Bool) -> Parser Char
    cond p = Parser $ \t -> case t of
        (x:xs) | p x -> Just (x, xs)
        _            -> Nothing

    char :: Char -> Parser Char
    char c = cond (== c)

    oneOf :: [Char] -> Parser Char
    oneOf l = cond (`elem` l)

    digit :: Parser Char
    digit = cond isDigit

    spaces :: Parser ()
    spaces = many (cond isSpace) >> return ()

Jako příklad si ukážeme parsování desetinných čísel. Pro jednoduchost používám na samotnou konverzi z řetězce na `Double` funkci `read`.

    number = some digit

    plus = do
        char '+'
        number

    minus = do
        char '-'
        n <- number
        return ('-':n)

    integer = plus <|> minus <|> number

    double = do
        str <- helper
        return (read str :: Double)
      where
        helper = do
            i <- integer
            d <- decimal
            e <- exponent
            return (i ++ d ++ e)

        decimal = option "" $ do
            char '.'
            n <- number
            return ('.':n)

        exponent = option "" $ do
            e <- oneOf "eE"
            n <- integer
            return (e:n)

Zkusme si to!

    ghci> runParser double "1"
    Just (1.0,"")

    ghci> runParser double "1      "
    Just (1.0,"      ")

    ghci> runParser double "1e5      "
    Just (100000.0,"      ")

    ghci> runParser double "-1e15      "
    Just (-1.0e15,"      ")

    ghci> runParser double "-1.47e15      "
    Just (-1.47e15,"      ")

    ghci> runParser double "-1.47e-1      "
    Just (-0.147,"      ")

    ghci> runParser double "-1.-47e-1      "
    Just (-1.0,".-47e-1      ")
    -- tady je vidět, že parser korektně naparsoval "-1" a zbytek řetězce
    -- nechal být

    ghci> runParser double "-a.47e-1      "
    Nothing

### III.VI. Pomocné funkce

Standardní knihovna definuje spoustu funkcí pro práci s monádami. Podívejme se na pár příkladů, které se dají celkem pěkně uplatnit:

Pro práci se seznamy máme funkce:

    sequence :: Monad m => [m a] -> m [a]
    sequence = foldr step (return [])
      where
        step m r = do
            a  <- m
            as <- r
            return (a:as)

    sequence_ :: Monad m => [m a] -> m ()
    sequence_ = foldr (>>) (return ())

`sequence` a `sequence_` fungují tak, že vezmou seznam monadických akcí, provedou každou akci, `sequence` pak výsledky uloží do seznamu. Pokud nás výsledky nezajímají, `sequence_` je efektivnější verze, která tyto výsledky ignoruje.

    -- definováno v std knihovně
    print :: Show a => a -> IO ()
    print = putStrLn . show

    ghci> sequence_ [print i | i <- [1..10]]
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10

    ghci> sequence (replicate 5 getLine)
    user input 1
    user input 2
    user input 3
    user input 4
    user input 5
    ["user input 1","user input 2","user input 3","user input 4","user input 5"]

Všimněte si, že `sequence` funguje pro libovolnou monádu:

    ghci> sequence [[1,2], [4,5,6]]
    [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6]]

Na `sequence` jsou založeny další funkce:

    mapM :: Monad m => (a -> m b) -> [a] -> m [b]
    mapM f = sequence . map f

    mapM_ :: Monad m => (a -> m b) -> [a] -> m [b]
    mapM_ f = sequence_ . map f

    forM :: Monad m => [a] -> (a -> m b) -> m [b]
    forM = flip mapM

    -- analogicky pro forM_

    action :: IO [String]
    action = forM [1..5] $ \i -> do
        putStrLn $ "Write number " ++ show i
        getLine

    ghci> action
    Write number 1
    14
    Write number 2
    2
    Write number 3
    7
    Write number 4
    42
    Write number 5
    3
    ["14","2","7","42","3"]

`forM` a `forM_` se nacházejí v modulu `Control.Monad`.

Dále máme kompozici (zmíněnou již dříve):

    (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
    f >=> g = \x -> f x >>= g

    (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
    (<=<) = flip (>=>)

Kromě `forM` cyklu máme i `while (true)`:

    forever :: Monad m => m a -> m b
    forever m = m >> forever m

    ghci> forever (getLine >>= putStrLn)
    Hello
    Hello
    x
    x
    -- ...

Pro práci s podmínkami existují funkce:

    when :: Monad m => Bool -> m () -> m ()
    when True  m = m
    when False _ = return ()

    unless :: Monad m => Bool -> m () -> m ()
    unless = when . not

IV. Závěrem
-----------

Uh, snad je to všechno. Pokud máte nějaké dotazy (jakože dotazy pravděpodobně budou), jsem k sehnání na vituscze@gmail.com
