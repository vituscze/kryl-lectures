
Pro úplnost zde uvádím pár informací navíc, které sice nejsou vázany na Krylovu přednášku, přesto se však mohou hodit.

I. Syntaxe funkcí, redux
------------------------

Jednou z věcí, kterou jsem zapomněl zmínit, ale asi se bez ní obejdete, je vztah mezi aplikací funkce, funkčí šipkou a případně curryfikací.

Prefixní aplikace funkcí (tj. juxtapozice) má maximální prioritu a levou asociativitu. Funkčí šipka má naproti tomu nejnižší prioritu a pravou asociativitu. Co to znamená?

    foldr f z list

je se ve skutečnosti parsuje jako

    ((foldr f) z) list

kde `foldr :: (a -> b -> b) -> b -> [a] -> b`.

Tohle je přesně v souladu s tím, že funkce v Haskellu berou pouze jeden argument. Nejprve aplikujeme `foldr` na funkci `f`, čímž dostaneme funkci, kterou pak zavoláme na `z`, atp. V C-čkové syntaxi by takovéto volání vypadalo poněkud podivně.

    foldr(f)(z)(list)

Aplikace funkcí na nějaký argument tedy z typové signatury odřízne vrchol (funkčí šipku) a jeho levý podstrom (první argument).


         foldr :: (->)
                  /  \
                 /    \
     (a -> b -> b)     (->)       ==>    foldr f :: (->)
                       /  \                         /  \
                      /    \                       /    \
                     b      (->)                  b      (->)
                            /  \                         /  \
                           /    \                       /    \
                         [a]     b                    [a]     b

A tohle funguje právě díky tomu, že prefixní aplikace se asociuje vlevo a má vysokou prioritu, zatímco funkčí šipka má pravou asociativitu a nízkou prioritu.

Poslední (poněkud mind-bending) příklad na aplikaci funkcí:

    -- identita funguje pro každý typ
    id :: a -> a
    id x = x

    id length [1..5]

Jak tohle vůbec funguje? Vždyť `id` dostane jeden argument a to je vše, jak ji můžeme dále volat na `[1..5]`? Pokud si uvědomíte, že

    id length [1..5]

je ve skutečnosti:

    (id length) [1..5]

tak najednou vše dává smysl. `id length` vrací zpět funkci `length`, kterou pak můžete aplikovat na seznam `[1..5]`. Jak tohle funguje? Odpověď je unifikace (na úrovni typů):

    id     :: (a         ) -> a
    length :: ([b] -> Int)

Zjevně můžeme použít `id length` tak, že `a` nahradíme za `[b] -> Int`. Proveďme tedy tuto substituci:

    id     :: ([b] -> Int) -> ([b] -> Int)
    length :: ([b] -> Int)

Nyní už všechno krásně pasuje a dostáváme `id length :: [b] -> Int`, což je další funkce, kterou poté aplikujeme na `[1..5]`.


II. Type inference
------------------

Haskell je schopný odvodit nejobecnější typ k jakékoliv funkci, tj. pokud např. napíšete:

    f x = map (*2) x

a poté se zeptáte na typ funkce `f` (což se dělá pomocí příkazu `:t výraz` uvnitř GHCi, viz další kapitola), dostanete odpověď:

    ghci> let f x = map (*2) x
    ghci> :t f
    f :: Num b => [b] -> [b]

Kryl sice na zkoušce bude vyžadovat všechny typy ručně napsané, ale pokud máte při ruce GHCi a nevíte, jaký typ napsat (ideálně tak, aby byl co nejobecnější), tak prostě typ vynechejte a pak se zeptejte přes GHCi. Existuje několik případů, kdy type inference nefunguje (a ani nemůže fungovat), ale nepotkáte se s nimi často.

***

### Pro zájemce

Jeden z takových případů je polymorfní rekurze, tj. v definici nějaké funkce `f` použijete rekurzivně `f`, ale na odlišný typ!

    stupidShow :: Show a => Int -> a -> String
    stupidShow 0 a = show a
    stupidShow n a = stupidShow (n - 1) [a]

    ghci> stupidShow 10 42
    "[[[[[[[[[[42]]]]]]]]]]"

Všimněte si, že funkci rekurzivně voláme na jednoprvkový seznam. Bez typové signatury vám GHC(i) vynadá, že se pokoušíte vytvořit nekonečný typ:

    Occurs check: cannot construct the infinite type: t0 = [t0]
    In the expression: a
    In the second argument of `stupidShow', namely `[a]'
    In the expression: stupidShow (n - 1) [a]

Další případy, kdy type inference není aktivní je při použití language extensionů `Rank2Types`, `RankNTypes`, `GADTs` a možná ještě více.

***


III. Práce s GHC a GHCi
-----------------------

Haskell Platform v sobě obsahuje GHC a GHCi. První z těchto dvou je kompilátor (Haskell je totiž primárně kompilovaný jazyk) a druhý je interaktivní interpret.

Pokud chcete zkompilovat váš program, stačí zavolat

    ghc source.hs

případně s optimalizacemi:

    ghc -O source.hs

Nicméně pro vytvoření binárky potřebujete mít definovanou hodnotu

    main :: IO ()

v modulu `Main`. K tomu se dostaneme na pozdějších přednáškách.

Dále je tu GHCi, kde strávíte většinu času. Pro pohodlnou práci se obvykle doporučuje vytvořit si nějaký soubor, dále jako source.hs a pomocí GHCi si ho načíst. To se dá provést několika způsoby:

    ghci source.hs (rovnou načte soubor)
    ghci           (pustí pouze GHCi)

      :l source.hs (uvnitř GHCi)

GHCi podporuje spoustu všemožných příkazů, zatím jsme viděli `:l`, `:t` a `:i`, pár dalších příkazů, které se vám mohou hodit:

    :m +Data.List       -- načte modul Data.List
    :cd dir             -- změní working directory
    :l source.hs        -- načte soubor
    :r                  -- reload souboru
    :t expr             -- určí typ výrazu
    :i jméno            -- zobrazuje informace o daném jméně, dá se použít na
                        -- typové třídy, operátory, funkce atp.
    :k typecon          -- oznámí kind typového konstruktoru, více (snad)
                        -- později
    :main args          -- spustí main s argumenty args
    :browse Data.List   -- vypíše všechny entity z modulu Data.List

Při načtení GHCi zkontroluje syntaxi a provede type checking, pokud je všechno v pořádku, GHCi odpoví něco ve smyslu:

    Ok, modules loaded: Something.

Pokud v původním souboru provedete změny, tak v GHCi stačí napsat `:r` a GHCi automaticky reloadne daný soubor. `:r` je vůbec příkaz, který se používá často.

Kromě toho, že v GHCi můžete vyhodnocovat různé výrazy, můžete také definovat vlastní funkce a hodnoty. Pozor, z historických důvodů se definice uvnitř GHCi píší poněkud zvláštním způsobem (proto doporučení psát si věci do souboru a pak ho načíst do GHCi). Např.:

    source.hs:

    fac :: Integer -> Integer
    fac n = product [1..n]

    -- bez typové signatury
    add x y = x + y

    GHCi:

    ghci> let fac :: Integer -> Integer; fac n = product [1..n]
    ghci> let add x y = x + y


    ghci> fac 40
    815915283247897734345611269596115894272000000000
    ghci> fac 10 + fac 30
    265252859812191058636308483628800
    ghci> :t add
    add :: Num a => a -> a -> a
    ghci> take 20 [1, 3 ..]
    [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39]


III. Další užitečné konstrukty
------------------------------

Kromě již na přednášce zmiňovaného let obsahuje Haskell ještě několik užitečných pomůcek pro zpřehlednění kódu:


###  III.I. `let`

Syntaxe let je následující:

    let lhs1 = rhs1
        lhs2 = rhs2
        ...
        lhsn = rhsn
    in expr

`let` a `in` jsou klíčová slova. Příklad:

    let l = length [1..5]
        f x = x * 3 + 1
    in f l
    -- výsledek: 16

`let` uvozuje layout, viz kapitola IV.

Sémantika je jasná, umožňuje pojmenovat podvýrazy a lokální funkce pro zefektivnění a zpřehlednění kódu. Narozdíl od Scheme je v Haskellu `let` defaultně rekurzivní, můžete tedy napsat např.:

    let len [] = 0
        len (_:xs) = 1 + len xs
    in len [1..5]
    -- výsledek: 5

Narozdíl od `where` (níže) je `let` sám o sobě výraz, můžete tedy např. napsat toto (ne, že bych to doporučoval):

    f x = 1 + let g y = y * 3 + 1 in g x

### III.II. `where`

Syntaxe `where` je následující:

    lhs = rhs
      where
        lhs1 = rhs1
        lhs2 = rhs2
        ...
        lhsn = rhsn

`where` je opět keyword, který uvozuje layout. Sémanticky je předchozí `where` ekvivalentní:

    lhs = let lhs1 = rhs1
              lhs2 = rhs2
              ...
              lhsn = rhsn
          in rhs

Plní tedy podobný účel jako `let`, ale vypadá o něco lépe.

    length :: [a] -> Int
    length l = go 0 l -- 'go' je časté jméno pro rekurzivní helper funkci
      where
        go acc []     = acc
        go acc (_:xs) = go (acc + 1) xs

### III.III. `case`

Syntaxe `case`:

    case expr of
      alt1 -> expr1
      alt2 -> expr2
      ...
      altn -> exprn

`where` a `of` jsou klíčová slova, `of` pak uvozuje layout. `case` funguje tak, že se podívá na výraz `expr` a zkusí, jestli první pattern `alt1` odpovídá, v tom případě je výsledkem `case` výraz `expr1`, pokud `alt1` neodpovídá, přesune se na `alt2`, atp.

    case [1,2,3] of
      []    -> 0
      [_]   -> 1
      [_,b] -> 2 + b
      (a:_) -> a * 2

První pattern neuspěje (`[1,2,3]` není prázdný seznam), druhý také ne (tento pattern matchuje pouze seznamy s právě jedním prvkem), třetí také ne, ale poslední ano.

`[1,2,3]` je syntaktická zkratka pro `1:2:3:[]`, tj. do `a` se dosadí 1 a výsledná hodnota `case` výrazu je 1 * 2, tedy 2.

### III.IV. `if`

V Haskellu je také obyčejné `if-then-else` s jednoduchou syntaxí:

    if cond then expr1 else expr2

`cond` je libovolný výraz typu `Bool`, `expr1` a `expr2` libovolné výrazy stejného typu. Pozor, `else` větev je povinná (v tomto smyslu se dá na `if-then-else` v Haskellu dívat jako na ternární operátor `?`). Sémantiku není třeba vysvětlovat.

    if length [] < 1 then "hello" else "whatever"

IV. Haskell is whitespace sensitive
------------------------------------

Některé to možná znechutí, ale Haskell má layout založený na odsazování. Narozdíl od jiných jazyků s podobnou "vadou" je layout v Haskellu celkem rozumný.

Část klíčových slov v Haskellu uvozuje layout, konkrétně se jedná o `let`, `where`, `of` a `do` (bude později). První non-whitespace znak, který není součástí komentáře, pak určuje odsazení bloku. Řádka s odsazením vyšším se počítá jako pokračování předchozí, řádka s odsazením stejným se počítá jako nová řádka a konečně řádka s nižším odsazením ukončuje layout. Například:

    f y = case y of        -- of je layout keyword
        [] -> 1 + product  -- [ určuje identaci +4 vůči 'f'
            [1..7]         -- indentace větší než +4, je to část předchozí řádky
        (x:_) -> x * x     -- indentace stejná, nová alternativa case

    g z = z                -- indentace menší, konec layoutu

Indentační úroveň celého modulu je 0. Tj. v tomto kódu

    f x = let a = 5
              b = 6
    in a + b + x

nepatří `in a + b + x` k funkci `f` a tudíž se jedná o parse error.

Ve většině případů tohle bývá přehledné a rozumné (kdyžtak se porozhlédněte v mém repozitáři `logic`, jak takový obyčejný kód v Haskellu vypadá). Pokud ale stejně trváte na tom, že nechcete mít s whitespace sensitive jazykem nic společného, tak Haskell nabízí explicitní layout pomocí složených závorek.

Pokud je první non-whitespace znak po layout keyword otevírací složená závorka, tak se layout pro tuto část vypíná a řádky se explicitně označují středníkem.

    f y = case y of { [] -> 1 + product
             [1..7]; (x:_)
    -> x * x}

  **Poznámka:** Pokud budete používat layout (což doporučuji), tak ve vašem oblíbeném editoru vypněte skutečné tabulátory. Standard totiž vyžaduje interpretovat `TAB` jako zarovnání na nejbližších dalších 8 sloupců. Jelikož tabulátory jsou obyčejně nastavované na 2 nebo 4 sloupce, můžete se dopustit spousty "neviditelných" chyb.

Odsazování řeště pouze pomocí mezer.


V. Case sensitivity
-------------------

Kromě toho, že jsou jména case sensitive, využívá Haskell rozlišení podle velikosti písmen pro několik důležitých věcí:

V typové signatuře označují jména začínající velkým písmenem konkrétní typy, tedy např. `Int`, `Integer`, `Char`, `String` atp. Jména začínající malým písmenem jsou pak typové proměnné. Tohle má příjemný důsledek, že kdykoliv se podíváte na signaturu, velice jednoduše určíte, co je "generické" a co je konkrétní:

    insert :: key -> value -> Map key value -> Map key value

Tj. `insert` funguje pro libovolné typy `key` a `value` a vkládá tyto dvě hodnoty do mapy, což je konkrétní typ označený jako `Map`.

Na úrovni výrazu je rozlišení podobné. Funkce, konstanty, argumenty, všechny začínají malým písmenem. Naopak konstruktory začínají písmenem velkým, tj.

    printError :: Either String Int -> String
    printError (Left error) = error
    printError (Right value) = show value

`Either` je následující datový typ:

    ghci> :i Either
    data Either a b = Left a | Right b      -- Defined in `Data.Either'

Zase je na první pohled vidět, co je konstruktor a co je proměnná. To je také důvod, proč nelze zkompilovat např.:

    Find :: Ord a => a -> Set a -> Bool

Správně je pouze `find`.

***

###  Pro zájemce

Haskell rozlišuje dva základní jmenné prostory: pro typy a pro hodnoty. Když deklarujete tento typ:

    data Pair a b = Pair a b

tak se nejedná o rekurzivní definici. `data Pair` definuje `Pair` v jmenném prostoru typů, zatímco `= Pair` definuje konstruktor, tedy jméno v jmenném prostoru hodnot. Bývá zvykem dávát typům, které mají pouze jeden konstruktor stejné jméno jak pro konstruktor tak pro typ (či typový konstruktor).

Typy a hodnoty jsou velice dobře odděleny, takže toto nevede k nejednoznačnosti.

Obyčejně se moduly importují do globálního jmenného prostoru, ale spousta modulů definuje funkce, jejichž jména kolidují s jmény v `Prelude`, např. `Data.Map` definuje funkci:

    map :: (a -> b) -> Map key a -> Map key b

Tyto moduly byly navrženy tak, abyste je importovaly pod jejich plným jménem, tedy:

    import Data.Map

    f = map

    <interactive>:9:9:
    Ambiguous occurrence `map'
    It could refer to either `Data.Map.map', imported from `Data.Map'
                          or `Prelude.map',
                             imported from `Prelude' (and originally defined in
    `GHC.Base')

versus:

    import qualified Data.Map

    f :: (a -> b) -> Data.Map.Map key a -> Data.Map.Map key b
    f = Data.Map.map

nebo:

    import qualified Data.Map as Map

    f :: (a -> b) -> Map.Map key a -> Map.Map key a
    f = Map.map


Tyto importy se dají různě kombinovat, např. jméno typu `Map` se v `Prelude` nevyskytuje, bylo by tedy otravné pořád vypisovat `Map.Map`:

    import qualified Data.Map as Map -- import všeho pod jménem Map
    import Data.Map (Map)            -- import nekvalifikovaného jména Map

    f :: (a -> b) -> Map key a -> Map key b -- ideální
    f = Map.map

Několik příkladů je opět k vidění v repozitáři `logic`.

***

VI. Závěrem
-----------

Pokud by byly nějaké dotazy, připomínky či korekce, tak mi můžete napsat na vituscze@gmail.com

Jsem ochoten odpovídat na libovolné otázky ohledně Haskellu a pokud to bude rozumné, tak se je pokusím sbalit do podobného dokumentu.
