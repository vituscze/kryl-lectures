Haskell a závorky
=================

Narozdíl od Lispu se Haskell pokouší šetřit závorkami. Pro tento účel existuje několik operátorů.

I. Operátor `($)`
-----------------

Spousta výrazů musí být uzávorkována kvůli tomu, že aplikace funkce má maximální prioritu. Občas by se nám však hodilo, aby aplikace měla prioritu malou. Např.:

    map (*2) ([1..5] ++ [6..10])

Pokud by aplikace mezi `(*2)` a `[1..5] ++ [6..10]` měla nízkou prioritu, mohli bychom napsat:

    map (*2) [1..5] ++ [6..10]

Přesně proto existuje v Haskellu operátor `$`, definovaný takto:

    ghci> :i $
    ($) :: (a -> b) -> a -> b       -- Defined in `GHC.Base'
    infixr 0 $

    -- a nedělá vůbec nic
    f $ x = f x

Náš příklad lze potom zapsat jako:

    map (*2) $ [1..5] ++ [6..10]

Díky nízké prioritě `$` se předchozí výraz naparsuje takto:

    (map (*2)) $ ([1..5] ++ [6..10])
    -- aplikujeme $ -->
    (map (*2)) ([1..5] ++ [6..10])
    -- což odpodívá původnímu výrazu

`($)` se také dá použít pro posloupnost funkcí:

    \list -> show $ take 20 $ map (^2) $ filter odd $ list

Nicméně, toto je přesně případ, kdy by se hodil jiný operátor:

II. Operátor `(.)`
------------------

V matematice se setkáte se skládáním funkcí, které je obyčejně definované takto:

    (f ∘ g)(x) = f(g(x))

Haskell kompozici funkcí umožňuje také, jen pod poněkud méně poetickým jménem `.`:

    infixr 9 .

    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    (f . g) x = f (g x)

Pokud se vám zalíbily pipy v shellu:

    cat file | sort -u | head -5 | tr 2X 4B | sed "s://.*::g"

Tak s kompozicí funkcí budete jako doma. Všimněte si, že narozdíl od shellu jsou funkce v obráceném pořadí. Poslední příklad v kapitole I. by tedy šel zapsat jako:

    show . take 20 . map (^2) . filter odd

Kromě toho, že můžeme elegatně vyjadřovat posloupnosti funkcí, můžeme také s pomocí `.` redukovat seznamy funkcí, iterovat jednu funkci, atp.

Iteraci jedné funkce v matematice definujeme takto:

    f⁰ = id
    fⁿ = f ∘ fⁿ⁻¹

V Haskellu pak (pro použití v kódu doporučuji vymyslet lepší jméno):

    -- tento operátor není nikde předdefinovaný
    (.^) :: (a -> a) -> Int -> (a -> a)
    f .^ 0 = id
    f .^ n
      | n > 0 = f . (f .^ (n - 1))

    -- alternativně
    f .^ n = (!! n) . iterate f

Pokud bychom měli příslušný eliminátor přirozených čísel, jako např.:

    natElim :: (a -> a) -> a -> Integer -> a
    natElim f z 0 = z
    natElim f z n
      | n > 0 = f $ natElim f z (n - 1)

    (.^) f = natElim (f .) id

Ale zde možnosti nekončí, zkusme vzít seznam funkcí a vytvořit z něj jedinou funkci, která provede všechny funkce v seznamu.

    compAll :: [a -> a] -> a -> a
    compAll = foldr (.) id

    -- compAll [(+1), (*2), (^2)] == (+1) . (*2) . (^2) . id

    f .^ n = compAll . replicate n f

    -- nebo pokud preferujete funkce bez argumentů
    (.^) = (compAll .) . flip replicate

Ačkoliv má operátor `(.)` velmi vysokou prioritu, aplikace funkcí má prioritu větší. Na to je třeba myslet, pokud přepisujete výrazy do této podoby:

    doSomething x = doThis (doThat (doThisTwice (doOtherStuff x)))

    doSomething x = doThis . doThat . doThisTwice . doOtherStuff x
    -- předchozí výraz se uzávorkuje takto:
    doSomething x = doThis . doThat . doThisTwice . (doOtherStuff x)
    -- což zjevně není ekvivalentní původní funkci

    -- správně je
    doSomething x = (doThis . doThat . doThisTwice . doOtherStuff) x

    -- zde se můžeme zbavit závorek
    doSomething x = doThis . doThat . doThisTwice . doOtherStuff $ x

    -- nebo dokonce zahodit x na obou stranách
    doSomething = doThis . doThat . doThisTwice . doOtherStuff

III. Závěrem
------------

Opět jsem k nalezení na adrese vituscze@gmail.com . Tento text obsahuje skrytou referenci na Red Dwarf, tři bludišťáky pro toho, kdo ji odhalí.
