--------------------------------------------------------------------------Aufgabenblatt 6 .hs--------------------------------------------------------------------------
module Mengen where
type Fehlermeldung = String
type MengeAlsZeichenreihe = String
newtype MT1 e = MT1 [e] 
data MT2 e = Nichts
             | VerlaengereUm e (MT2 e)
newtype MT3 e = MT3 (e -> Bool)


--Klasse Defaultable
--Definiert eine Typklasse für Typen, die einen Standardwert (eine Liste von Werten) liefern können.

class Defaultable a where
  defaultValue :: [a] --  liefert eine Liste von Standardwerten des Typs 'a'.

instance Defaultable Int where
  defaultValue = [(- 100)..100]

instance Defaultable Char where
  defaultValue = ['a'..'z'] ++ ['A'..'Z']

class Menge m where
  leereMenge :: m
  allMenge :: m
  istMenge :: m -> Bool
  vereinige :: m -> m -> m
  schneide :: m -> m -> m
  zieheab :: m -> m -> m
  komplementiere :: m -> m
  komplementiere = zieheab allMenge
  sindGleich :: m -> m -> Bool
  sindGleich m1 m2 = istTeilmenge m1 m2 && istTeilmenge m2 m1 --Zwei Mengen sind gleich, wenn sie Teilmengen voneinander sind
  sindUngleich :: m -> m -> Bool
  sindUngleich m1 = not . sindGleich m1
  istTeilmenge :: m -> m -> Bool
  istObermenge :: m -> m -> Bool
  istObermenge m1 m2 = istTeilmenge m2 m1
  istEchteTeilmenge :: m -> m -> Bool
  istEchteTeilmenge m1 m2 = istTeilmenge m1 m2 && not (sindGleich m1 m2) --Eine Menge ist echte Teilmenge einer Anderen, wenn sie Teilmenge aber nicht gleich ist
  istEchteObermenge :: m -> m -> Bool
  istEchteObermenge m1 m2 = istEchteTeilmenge m2 m1 --Wenn A (echte) Obermenge von B ist, ist dann B (echte) Teilmenge von A
  sindElementeFremd :: m -> m -> Bool
  sindElementeFremd m1 = sindGleich leereMenge . schneide m1 --Zwei Mengen sind elementefremd, wenn ihrer Schnitt die Leeremenge ist
  sindQuerUeberlappend :: m -> m -> Bool
  sindQuerUeberlappend m1 m2 =             --Zwei Mengen sind quer-> ueberlappend, wenn sie...
    not (sindElementeFremd m1 m2)          --  ... mindestens ein Element gemeinsam haben
    && not (istTeilmenge m1 m2)            --  ... jeweils keine Teilmenge voneinander sind
    && not (istTeilmenge m2 m1)
  istKeinGueltigerMengenwert :: Fehlermeldung -> m
  istKeinGueltigerMengenwert = error
  nichtImplementierbar :: Fehlermeldung -> m
  nichtImplementierbar = error
  zeige :: m -> MengeAlsZeichenreihe


instance Menge (MT1 Char) where
  leereMenge = MT1 []
  allMenge = MT1 defaultValue
  istMenge = istMengeMT1
  vereinige = vereinigeMT1
  schneide = schneideMT1
  zieheab = zieheabMT1
  istTeilmenge = istTeilmengeMT1
  zeige = zeigeMT1


instance Menge (MT1 Int) where
  leereMenge = MT1 []
  allMenge = MT1 defaultValue
  istMenge = istMengeMT1
  vereinige = vereinigeMT1
  schneide = schneideMT1
  zieheab = zieheabMT1
  istTeilmenge = istTeilmengeMT1
  zeige = zeigeMT1


--Allgemeine Funktionen für MT1

istMengeMT1 :: Eq e =>MT1 e -> Bool
istMengeMT1 (MT1     []) = True
istMengeMT1 (MT1 (e:es)) = all (/= e) es && (istMengeMT1 . MT1) es

vereinigeMT1 :: Eq e =>MT1 e -> MT1 e -> MT1 e
vereinigeMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 . nub $ list1 ++ list2
      | otherwise                        = fehlermeldung

schneideMT1 :: Eq e =>MT1 e -> MT1 e -> MT1 e
schneideMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <-  list1, e `elem` list2]
      | otherwise                        = fehlermeldung

zieheabMT1 :: Eq e =>MT1 e -> MT1 e -> MT1 e
zieheabMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <-  list1, e `notElem` list2]
      | otherwise                        = fehlermeldung


istTeilmengeMT1 :: Eq e =>MT1 e -> MT1 e -> Bool
istTeilmengeMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = all (`elem` list2) list1
      | otherwise                        = fehlermeldung

zeigeMT1 :: Show e =>MT1 e -> MengeAlsZeichenreihe
zeigeMT1 (MT1 elems) = "{" ++ formatElems elems ++ "}"


--Hilffunktionen für MT1.

--Fehlermeldung für wenn ein oder mehrere Argumente nicht Menge sind.

fehlermeldung :: a
fehlermeldung = error "Argument muss Menge sein (keine Duplikate)"

--Entferne Duplikate einer Liste.

nub :: Eq a =>[a] -> [a]
nub     [] = []
nub (e:es) = e : (nub $ filter (/= e) es)

--Formatiere Elemente, um sie auszudrucken.

formatElems :: Show a =>[a] -> String
formatElems []     = ""
formatElems [e]    = show e
formatElems (e:es) = show e ++ ", " ++ formatElems es

instance Menge (MT3 Char) where
  leereMenge = MT3 (\_ -> False)
  allMenge = MT3 (\_ -> True )
  istMenge = \_ -> True
  vereinige = vereinigeMT3
  schneide = schneideMT3
  zieheab = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige = zeigeMT3


instance Menge (MT3 Int) where
  leereMenge = MT3 (\_ -> False)
  allMenge = MT3 (\_ -> True )
  istMenge = \_ -> True
  vereinige = vereinigeMT3
  schneide = schneideMT3
  zieheab = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige = zeigeMT3


--Allgemeine Funktionen für MT3.

vereinigeMT3 :: Eq e =>MT3 e -> MT3 e -> MT3 e
vereinigeMT3 (MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem || f2 elem

schneideMT3 :: Eq e =>MT3 e -> MT3 e -> MT3 e
schneideMT3 (MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem && f2 elem

zieheabMT3 :: Eq e =>MT3 e -> MT3 e -> MT3 e
zieheabMT3(MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem && (not . f2) elem

istTeilmengeMT3 :: (Eq e, Defaultable e) =>MT3 e -> MT3 e -> Bool
istTeilmengeMT3 m1 (MT3 f) =
    let elems1 = toListMT3 m1
    in all f elems1

zeigeMT3 m = "{" ++ (formatElems . toListMT3) m ++ "}"


--Diese Funktion wandelt einen Wert des Typs MT3 in eine Liste vom Typ e um.
--Sie benoetigt die Typklassebeschraenkung (Defaultable e), um sicherzustellen, dass der Typ e eine defaultValue Funktion besitzt.
--Haette man nicht diese Beschaenkung, muesste man Bound nutzen. Es ist aber sehr gross bei Int und Char.

toListMT3 :: (Defaultable e) =>MT3 e -> [e]
toListMT3 (MT3 f) = filter f defaultValue


--Ueberpruefe ob ein Char Element in einer Menge ueber Chars ist.
--Man kann hier bei der Ueberpruefung, ob die Eingaben gueltig sind, nicht istKeinGueltigerMengenwert nutzen, weil der Rueckgabetyp dieser Funktion ein Bool ist, und von istKeinGueltigerMengenwert eine Menge.

istElement :: Menge m =>Char -> m -> Bool
istElement c m
    | not $ isDefaultChar c = error "Ungueltiger Charakter"
    | otherwise             = elem c $ zeige m

--Ueberpruefe ob ein Char kein Element in einer Menge ueber Chars ist.

istKeinElement :: Menge m =>Char -> m -> Bool
istKeinElement c = not . istElement c

isDefaultChar :: Char -> Bool
isDefaultChar = (`elem` defaultValue)
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--------------------------------------------------------------------------Aufgabenblatt 7 .hs--------------------------------------------------------------------------

---------------------------------------------------------------------------------A.1-----------------------------------------------------------------------------------
data Landeshauptstadt = B | E | G | I | K | L | P | S | W deriving (Show, Eq, Ord, Bounded, Enum) -- Show, Eq, Ord, Bounded und Enum sind sinnvoll für eine Aufzählung von Landeshauptstädten
-- Man darf nicht `deriving` fuer `type` Deklarationen
--  nutzen, weil es nur ein Typsynonym ist
type Staedtepaar = (Landeshauptstadt,Landeshauptstadt)
-- Show, Eq, Ord sind sinnvoll, da man Paare von Landeshauptstädten vergleichen und ausgeben kann. Bounded und Enum nicht anwendbar, da keine natürliche Ordnung aller Paare existiert.
data Staedtepaar1 = SP1 Landeshauptstadt Landeshauptstadt deriving (Show, Eq, Ord) 
newtype Staedtepaar2 = SP2 (Landeshauptstadt,Landeshauptstadt) deriving (Show, Eq, Ord) -- Ähnlich wie Staedtepaar1
-- Man kann nicht `deriving` fuer Typen nutzen,
--  die charakteristische Funktionen sind
data Staedtepaar3 = SP3 ((Landeshauptstadt,Landeshauptstadt) -> Bool) 
data Staedtepaar4 = SP4 (Landeshauptstadt -> Landeshauptstadt -> Bool) -- Ähnlich wie Staedtepaar3
---------------------------------------------------------------------------------A.2-----------------------------------------------------------------------------------
instance Defaultable Landeshauptstadt where
    defaultValue = [minBound .. maxBound]
-- (a)
instance Menge (MT1 Landeshauptstadt) where
    leereMenge   = MT1 []
    allMenge     = MT1 defaultValue
    istMenge     = istMengeMT1
    vereinige    = vereinigeMT1
    schneide     = schneideMT1
    zieheab      = zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige        = zeigeMT1

-- (b)
instance Menge (MT3 Landeshauptstadt) where
  leereMenge = MT3 (\_ -> False)
  allMenge = MT3 (\_ -> True )
  istMenge = \_ -> True
  vereinige = vereinigeMT3
  schneide = schneideMT3
  zieheab = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige = zeigeMT3

------------------------------------------------------------------------------ Tests ---------------------------------------------------------------------------------

main = do
  putStrLn "-----------------------------------------------------A.2-----------------------------------------------------"
  putStrLn ""
  putStrLn "----------(a)------------------------------------------------------------------------------------------------"
  putStrLn ""
  putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT1 Landeshauptstadt)
  putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT1 Landeshauptstadt)
  putStrLn ""
  putStrLn $ "istMenge     {}: " ++ (show $ istMenge (leereMenge :: MT1 Landeshauptstadt))
  putStrLn $ "istMenge {B, B}: " ++ (show $ istMenge $ MT1 [B, B])
  putStrLn $ "istMenge {B, E}: " ++ (show $ istMenge $ MT1 [B, E])
  putStrLn ""
  putStrLn $ "vereinige  {} {B}: " ++ (zeige . vereinige leereMenge $ MT1 [B])
  putStrLn $ "vereinige {B} {B}: " ++ (zeige $ vereinige (MT1 [B]) (MT1 [B]))
  putStrLn $ "vereinige {B} {E}: " ++ (zeige $ vereinige (MT1 [B]) (MT1 [E]))
  putStrLn ""
  putStrLn $ "schneide  {}    {B}: " ++ (zeige . schneide leereMenge $ MT1 [B])
  putStrLn $ "schneide {B}    {B}: " ++ (zeige $ schneide (MT1 [B]) (MT1    [B]))
  putStrLn $ "schneide {B} {B, E}: " ++ (zeige $ schneide (MT1 [B]) (MT1 [B, E]))
  putStrLn ""
  putStrLn $ "zieheab     {} {B}: " ++ (zeige . zieheab leereMenge $ MT1 [B])
  putStrLn $ "zieheab    {B} {B}: " ++ (zeige $ zieheab (MT1    [B]) (MT1 [B]))
  putStrLn $ "zieheab {B, E} {B}: " ++ (zeige $ zieheab (MT1 [B, E]) (MT1 [B]))
  putStrLn ""
  putStrLn $ "komplementiere . zieheab allMenge $ {B}: " ++ (zeige . komplementiere . zieheab allMenge $ MT1 [B])
  putStrLn ""
  putStrLn $ "sindGleich    {B}    {B}: " ++ (show $ sindGleich (MT1    [B]) (MT1    [B]))
  putStrLn $ "sindGleich    {B}    {E}: " ++ (show $ sindGleich (MT1    [B]) (MT1    [E]))
  putStrLn $ "sindGleich {B, E} {E, B}: " ++ (show $ sindGleich (MT1 [B, E]) (MT1 [E, B]))
  putStrLn ""
  putStrLn $ "sindUngleich    {B}    {B}: " ++ (show $ sindUngleich (MT1    [B]) (MT1    [B]))
  putStrLn $ "sindUngleich    {B}    {E}: " ++ (show $ sindUngleich (MT1    [B]) (MT1    [E]))
  putStrLn $ "sindUngleich {B, E} {E, B}: " ++ (show $ sindUngleich (MT1 [B, E]) (MT1 [E, B]))
  putStrLn ""
  putStrLn $ "istTeilmenge    {B}    {B}: " ++ (show $ istTeilmenge (MT1    [B]) (MT1    [B]))
  putStrLn $ "istTeilmenge {B, E} {E, B}: " ++ (show $ istTeilmenge (MT1 [B, E]) (MT1 [E, B]))
  putStrLn $ "istTeilmenge    {B} {B, E}: " ++ (show $ istTeilmenge (MT1    [B]) (MT1 [B, E]))
  putStrLn $ "istTeilmenge {B, E}    {B}: " ++ (show $ istTeilmenge (MT1 [B, E]) (MT1    [B]))
  putStrLn ""
  putStrLn $ "istEchteTeilmenge    {B}    {B}: " ++ (show $ istEchteTeilmenge (MT1    [B]) (MT1    [B]))
  putStrLn $ "istEchteTeilmenge {B, E} {E, B}: " ++ (show $ istEchteTeilmenge (MT1 [B, E]) (MT1 [E, B]))
  putStrLn $ "istEchteTeilmenge    {B} {B, E}: " ++ (show $ istEchteTeilmenge (MT1    [B]) (MT1 [B, E]))
  putStrLn $ "istEchteTeilmenge {B, E}    {B}: " ++ (show $ istEchteTeilmenge (MT1 [B, E]) (MT1    [B]))
  putStrLn ""
  putStrLn $ "istObermenge    {B}    {B}: " ++ (show $ istObermenge (MT1    [B]) (MT1    [B]))
  putStrLn $ "istObermenge {B, E} {E, B}: " ++ (show $ istObermenge (MT1 [B, E]) (MT1 [E, B]))
  putStrLn $ "istObermenge    {B} {B, E}: " ++ (show $ istObermenge (MT1    [B]) (MT1 [B, E]))
  putStrLn $ "istObermenge {B, E}    {B}: " ++ (show $ istObermenge (MT1 [B, E]) (MT1    [B]))
  putStrLn ""
  putStrLn $ "istEchteObermenge    {B}    {B}: " ++ (show $ istEchteObermenge (MT1    [B]) (MT1    [B]))
  putStrLn $ "istEchteObermenge {B, E} {E, B}: " ++ (show $ istEchteObermenge (MT1 [B, E]) (MT1 [E, B]))
  putStrLn $ "istEchteObermenge    {B} {B, E}: " ++ (show $ istEchteObermenge (MT1    [B]) (MT1 [B, E]))
  putStrLn $ "istEchteObermenge {B, E}    {B}: " ++ (show $ istEchteObermenge (MT1 [B, E]) (MT1    [B]))
  putStrLn ""
  putStrLn $ "sindElementeFremd {B}    {E}: " ++ (show $ sindElementeFremd (MT1 [B]) (MT1    [E]))
  putStrLn $ "sindElementeFremd {B} {B, E}: " ++ (show $ sindElementeFremd (MT1 [B]) (MT1 [B, E]))
  putStrLn ""
  putStrLn $ "sindQuerUeberlappend    {B}    {B}: " ++ (show $ sindQuerUeberlappend (MT1    [B]) (MT1    [B]))
  putStrLn $ "sindQuerUeberlappend    {B}    {E}: " ++ (show $ sindQuerUeberlappend (MT1    [B]) (MT1    [E]))
  putStrLn $ "sindQuerUeberlappend {B, G} {B, I}: " ++ (show $ sindQuerUeberlappend (MT1 [B, G]) (MT1 [B, I]))
  putStrLn $ "sindQuerUeberlappend    {B} {B, E}: " ++ (show $ sindQuerUeberlappend (MT1    [B]) (MT1 [B, E]))
  putStrLn $ "sindQuerUeberlappend {B, E}    {B}: " ++ (show $ sindQuerUeberlappend (MT1 [B, E]) (MT1    [B]))
  putStrLn ""
  putStrLn "----------(b)------------------------------------------------------------------------------------------------"
  putStrLn ""
  let b3  = \e -> if e == 'B'                          then True else False
      e3  = \e -> if e == 'E'                          then True else False
      be3 = \e -> if e == 'B' || e == 'E'              then True else False
      eb3 = \e -> if e == 'E' || e == 'B'              then True else False
      bg3 = \e -> if e == 'B' || e == 'G'              then True else False
      bi3 = \e -> if e == 'B' || e == 'I'              then True else False
      
  putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT3 Landeshauptstadt)
  putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT3 Landeshauptstadt)
  putStrLn ""
  putStrLn "istMenge ist die Protoimplementierung"
  putStrLn ""
  putStrLn $ "vereinige  {} {B}: " ++ (zeige . vereinige leereMenge $ MT3 b3)
  putStrLn $ "vereinige {B} {B}: " ++ (zeige $ vereinige (MT3 b3) (MT3 b3))
  putStrLn $ "vereinige {B} {E}: " ++ (zeige $ vereinige (MT3 b3) (MT3 e3))
  putStrLn ""
  putStrLn $ "schneide  {}    {B}: " ++ (zeige . schneide leereMenge $ MT3 b3)
  putStrLn $ "schneide {B}    {B}: " ++ (zeige $ schneide (MT3 b3) (MT3 b3))
  putStrLn $ "schneide {B} {B, E}: " ++ (zeige $ schneide (MT3 b3) (MT3 be3))
  putStrLn ""
  putStrLn $ "zieheab     {} {B}: " ++ (zeige . zieheab leereMenge $ MT3 b3)
  putStrLn $ "zieheab    {B} {B}: " ++ (zeige $ zieheab (MT3 b3) (MT3 b3))
  putStrLn $ "zieheab {B, E} {B}: " ++ (zeige $ zieheab (MT3 be3) (MT3 b3))
  putStrLn ""
  putStrLn $ "komplementiere . zieheab allMenge $ {B}: " ++ (zeige . komplementiere . zieheab allMenge $ MT3 b3)
  putStrLn ""
  putStrLn $ "sindGleich    {B}    {B}: " ++ (show $ sindGleich (MT3 b3) (MT3 b3))
  putStrLn $ "sindGleich    {B}    {E}: " ++ (show $ sindGleich (MT3 b3) (MT3 e3))
  putStrLn $ "sindGleich {B, E} {E, B}: " ++ (show $ sindGleich (MT3 be3) (MT3 eb3))
  putStrLn ""
  putStrLn $ "sindUngleich    {B}    {B}: " ++ (show $ sindUngleich (MT3 b3) (MT3 b3))
  putStrLn $ "sindUngleich    {B}    {E}: " ++ (show $ sindUngleich (MT3 b3) (MT3 e3))
  putStrLn $ "sindUngleich {B, E} {E, B}: " ++ (show $ sindUngleich (MT3 be3) (MT3 eb3))
  putStrLn ""
  putStrLn $ "istTeilmenge    {B}    {B}: " ++ (show $ istTeilmenge (MT3 b3) (MT3 b3))
  putStrLn $ "istTeilmenge {B, E} {E, B}: " ++ (show $ istTeilmenge (MT3 be3) (MT3 eb3))
  putStrLn $ "istTeilmenge    {B} {B, E}: " ++ (show $ istTeilmenge (MT3 b3) (MT3 be3))
  putStrLn $ "istTeilmenge {B, E}    {B}: " ++ (show $ istTeilmenge (MT3 be3) (MT3 b3))
  putStrLn ""
  putStrLn $ "istEchteTeilmenge    {B}    {B}: " ++ (show $ istEchteTeilmenge (MT3 b3) (MT3 b3))
  putStrLn $ "istEchteTeilmenge {B, E} {E, B}: " ++ (show $ istEchteTeilmenge (MT3 be3) (MT3 eb3))
  putStrLn $ "istEchteTeilmenge    {B} {B, E}: " ++ (show $ istEchteTeilmenge (MT3 b3) (MT3 be3))
  putStrLn $ "istEchteTeilmenge {B, E}    {B}: " ++ (show $ istEchteTeilmenge (MT3 be3) (MT3 b3))
  putStrLn ""
  putStrLn $ "istObermenge    {B}    {B}: " ++ (show $ istObermenge (MT3 b3) (MT3 b3))
  putStrLn $ "istObermenge {B, E} {E, B}: " ++ (show $ istObermenge (MT3 be3) (MT3 eb3))
  putStrLn $ "istObermenge    {B} {B, E}: " ++ (show $ istObermenge (MT3 b3) (MT3 be3))
  putStrLn $ "istObermenge {B, E}    {B}: " ++ (show $ istObermenge (MT3 be3) (MT3 b3))
  putStrLn ""
  putStrLn $ "istEchteObermenge    {B}    {B}: " ++ (show $ istEchteObermenge (MT3 b3) (MT3 b3))
  putStrLn $ "istEchteObermenge {B, E} {E, B}: " ++ (show $ istEchteObermenge (MT3 be3) (MT3 eb3))
  putStrLn $ "istEchteObermenge    {B} {B, E}: " ++ (show $ istEchteObermenge (MT3 b3) (MT3 be3))
  putStrLn $ "istEchteObermenge {B, E}    {B}: " ++ (show $ istEchteObermenge (MT3 be3) (MT3 b3))
  putStrLn ""
  putStrLn $ "sindElementeFremd {B}    {E}: " ++ (show $ sindElementeFremd (MT3 b3) (MT3 e3))
  putStrLn $ "sindElementeFremd {B} {B, E}: " ++ (show $ sindElementeFremd (MT3 b3) (MT3 be3))
  putStrLn ""
  putStrLn $ "sindQuerUeberlappend    {B}    {B}: " ++ (show $ sindQuerUeberlappend (MT3 b3) (MT3 b3))
  putStrLn $ "sindQuerUeberlappend    {B}    {E}: " ++ (show $ sindQuerUeberlappend (MT3 b3) (MT3 e3))
  putStrLn $ "sindQuerUeberlappend {B, G} {B, I}: " ++ (show $ sindQuerUeberlappend (MT3 bg3) (MT3 bi3))
  putStrLn $ "sindQuerUeberlappend    {B} {B, E}: " ++ (show $ sindQuerUeberlappend (MT3 b3) (MT3 be3))
  putStrLn $ "sindQuerUeberlappend {B, E}    {B}: " ++ (show $ sindQuerUeberlappend (MT3 be3) (MT3 b3))
  putStrLn ""
