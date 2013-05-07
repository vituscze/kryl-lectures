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
        putStrLn "What is your favourite color?"
        answer <- getLine
        return answer

Ale jednodušší a přehlednější je psát:

    ask = do
        putStrLn "What is your favourite color?"
        getLine
