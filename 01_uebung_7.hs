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
  leereMenge   = MT3 (\_ -> False)
  allMenge     = MT3 (\_ -> True )
  istMenge     = \_ -> True
  vereinige    = vereinigeMT3
  schneide     = schneideMT3
  zieheab      = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige        = zeigeMT3

---------------------------------------------------------------------------------A.3-----------------------------------------------------------------------------------
class Menge m => Relation m where
  istLeereRelation :: m -> Bool
  istLeereRelation = sindGleich leereMenge
  istAllRelation :: m -> Bool
  istAllRelation = sindGleich allMenge
  istLinkstotal :: m -> Bool
  istRechtstotal :: m -> Bool
  istReflexiv :: m -> Bool
  istSymmetrisch :: m -> Bool
  istTransitiv :: m -> Bool
  istQuasiOrdnung :: m -> Bool
  istQuasiOrdnung m = istReflexiv m && istTransitiv m
  istAequivalenzrelation :: m -> Bool
  istAequivalenzrelation m = istQuasiOrdnung m && istSymmetrisch m

---------------------------------------------------------------------------------A.4-----------------------------------------------------------------------------------
-- (a)
instance Defaultable Staedtepaar where
    defaultValue = allPairs
data IstPartnerstadtVon1 = IPV1 (MT1 (Landeshauptstadt, Landeshauptstadt))


instance Menge IstPartnerstadtVon1 where
  leereMenge = IPV1(MT1 [])
  allMenge = IPV1(MT1 defaultValue)
  istMenge (IPV1 m) = istMengeMT1 m
  vereinige (IPV1 m1) (IPV1 m2) = IPV1 $ vereinigeMT1 m1 m2
  schneide (IPV1 m1) (IPV1 m2) = IPV1 $ schneideMT1 m1 m2
  zieheab (IPV1 m1) (IPV1 m2) = IPV1 $ zieheabMT1 m1 m2
  istTeilmenge (IPV1 m1) (IPV1 m2) = istTeilmengeMT1 m1 m2
  zeige (IPV1 m1) = zeigeMT1 m1

instance Relation IstPartnerstadtVon1 where
  istLinkstotal  = istLinkstotalIPV1
  istRechtstotal = istRechtstotalIPV1
  istReflexiv = istReflexivIPV1
  istSymmetrisch = istSymmetrischIPV1
  istTransitiv (IPV1 m) = istTransitivIPV1 m m


-- Überprüft, ob für jede Landeshauptstadt mindestens ein Paar existiert, in dem sie der erste Eintrag ist.
istLinkstotalIPV1 :: Defaultable Landeshauptstadt => IstPartnerstadtVon1 -> Bool
istLinkstotalIPV1 (IPV1 (MT1 paare)) = all (\x -> any (\(a, _) -> a == x) paare) defaultValue

-- Überprüft, ob für jede Landeshauptstadt mindestens ein Paar existiert, in dem sie der zweite Eintrag ist.
istRechtstotalIPV1 :: Defaultable Landeshauptstadt => IstPartnerstadtVon1 -> Bool
istRechtstotalIPV1 (IPV1 (MT1 paare)) = all (\x -> any (\(_, a) -> a == x) paare) defaultValue

-- Überprüft, ob für jede Landeshauptstadt ein Paar (x,x) existiert.
istReflexivIPV1 :: Defaultable Landeshauptstadt => IstPartnerstadtVon1 -> Bool
istReflexivIPV1 (IPV1 (MT1 paare)) = all (\(x, y) -> x == y) paare

-- Überprüft, ob für jedes Paar (x,y) auch das Paar (y,x) existiert.
istSymmetrischIPV1 :: IstPartnerstadtVon1 -> Bool
istSymmetrischIPV1 (IPV1 (MT1 paare)) = all (\(x, y) -> elem (y, x) paare) paare

-- Überprüft, wenn es Paare (x, y) und (y, z) gibt, auch ein Paar (x, z) vorhanden sein muss
istTransitivIPV1 ::  MT1 Staedtepaar -> MT1 Staedtepaar -> Bool
istTransitivIPV1 _  (MT1 []) = True
istTransitivIPV1 (MT1 orig) (MT1 ((l, l'):ps)) =
  -- Schaue ob die Transitivitaet aller (l R l'')s gilt, dann schau weiter
  all (`elem` orig) l_l''s && (istTransitivIPV1 (MT1 orig) . MT1) ps
  where
    -- Alle vorgegebene (l' R x)s
    l'_l''s = filter (\pair -> fst pair == l') orig
    l''s    = map (snd) l'_l''s
    -- Alle zu pruefende Transitivitaeten aus l
    l_l''s  = [(l, l'') | l'' <- l''s]



allPairs :: [(Landeshauptstadt, Landeshauptstadt)]
allPairs = [(x, y) | x <- defaultValue, y <- defaultValue]


-- (b)  
data IstPartnerstadtVon3 = IPV3 (MT3 (Landeshauptstadt, Landeshauptstadt))

instance Menge IstPartnerstadtVon3 where
  leereMenge   = IPV3 (MT3 (\_ -> False))
  allMenge     = IPV3 (MT3 (\_ -> True ))
  istMenge     = \_ -> True
  vereinige (IPV3 m1) (IPV3 m2) = IPV3 $ vereinigeMT3 m1 m2
  schneide (IPV3 m1) (IPV3 m2) = IPV3 $ schneideMT3 m1 m2
  zieheab (IPV3 m1) (IPV3 m2) = IPV3 $ zieheabMT3 m1 m2
  istTeilmenge (IPV3 m1) (IPV3 m2) = istTeilmengeMT3 m1 m2
  zeige (IPV3 m1) = zeigeMT3 m1

instance Relation IstPartnerstadtVon3 where
  istLinkstotal  = istLinkstotalIPV3
--  istRechtstotal = istRechtstotalIPV3
--  istReflexiv = istReflexivIPV3
--  istSymmetrisch = istSymmetrischIPV3
-- istTransitiv 

istLinkstotalIPV3 :: Defaultable Landeshauptstadt => IstPartnerstadtVon3 -> Bool
istLinkstotalIPV3 (IPV3 (MT3 predicate)) = all (\x -> any (\(a, _) -> a == x) allPairs) defaultValue
  where allPairs = [(a, b) | a <- defaultValue, b <- defaultValue, predicate (a, b)]
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
  putStrLn "-----------------------------------------------------A.4-----------------------------------------------------"
  putStrLn ""
  putStrLn "----------(a)------------------------------------------------------------------------------------------------"
  let bIPV1 = IPV1 (MT1 [(B,B)])
      eIPV1 = IPV1 (MT1 [(E,E)])
      beIPV1 = IPV1 (MT1 [(B,B),(E,E)])
      ebIPV1 = IPV1 (MT1 [(E,E),(B,B)])
      bgIPV1 = IPV1 (MT1 [(B,B),(G,G)])
      biIPV1 = IPV1 (MT1 [(B,B),(I,I)])
      beispiel2 = IPV1 (MT1 [(B, E), (E, G)])
      linkstotal_true = IPV1 (MT1 [(B, B), (E, B), (G, B), (I, B), (K, B), (L, B), (P, B), (S, B), (W, B)])
      rechtstotal_true = IPV1 (MT1[(B, B), (B, E), (B, G), (B, I), (B, K), (B, L), (B, P), (B, S), (B, W)])
      symmetrisch_true = IPV1 (MT1 [(B, E), (E, B), (B, B)])
      reflexiv_true = IPV1 (MT1 [(B, B), (E, E)])
      transitiv_true = IPV1 (MT1 [(B, E), (E, G), (B, G)])
      

  putStrLn ""
  putStrLn $ "leereMenge: " ++ zeige (leereMenge :: IstPartnerstadtVon1)
  putStrLn $ "allMenge  : " ++ zeige (allMenge   :: IstPartnerstadtVon1)
  putStrLn ""
  putStrLn $ "istMenge     {}: " ++ (show $ istMenge (leereMenge :: IstPartnerstadtVon1))
  putStrLn $ "istMenge {(B,E), (B,E)}: " ++ (show $ istMenge $ IPV1 (MT1 [(B,E), (B,E)]))
  putStrLn $ "istMenge {(B,E), (E,B)}: " ++ (show $ istMenge $ IPV1 (MT1 [(B,E), (E,B)]))
  putStrLn ""
  putStrLn $ "vereinige  {} {(B,B)}: " ++ (zeige . vereinige leereMenge $ bIPV1)
  putStrLn $ "vereinige {(B,B)} {(B,B)}: " ++ (zeige $ vereinige (bIPV1) (bIPV1))
  putStrLn $ "vereinige {(B,B)} {(E,E)}: " ++ (zeige $ vereinige (bIPV1) (eIPV1))
  putStrLn ""
  putStrLn $ "schneide  {}    {(B,B)}: " ++ (zeige . schneide leereMenge $ bIPV1)
  putStrLn $ "schneide {(B,B)}    {(B,B)}: " ++ (zeige $ schneide (bIPV1) (bIPV1))
  putStrLn $ "schneide {(B,B)} {B, E}: " ++ (zeige $ schneide (bIPV1) (beIPV1))
  putStrLn ""
  putStrLn $ "zieheab     {} {(B,B)}: " ++ (zeige . zieheab leereMenge $ bIPV1)
  putStrLn $ "zieheab    {(B,B)} {(B,B)}: " ++ (zeige $ zieheab (bIPV1) (bIPV1))
  putStrLn $ "zieheab {(B,B),(E,E)} {(B,B)}: " ++ (zeige $ zieheab (beIPV1) (bIPV1))
  putStrLn ""
  putStrLn $ "komplementiere . zieheab allMenge $ {(B,B)}: " ++ (zeige . komplementiere . zieheab allMenge $ bIPV1)
  putStrLn ""
  putStrLn $ "sindGleich    {(B,B)}    {(B,B)}: " ++ (show $ sindGleich (bIPV1) (bIPV1))
  putStrLn $ "sindGleich    {(B,B)}    {(E,E)}: " ++ (show $ sindGleich (bIPV1) (eIPV1))
  putStrLn $ "sindGleich {(B,B),(E,E)} {E, B}: " ++ (show $ sindGleich (beIPV1) (ebIPV1))
  putStrLn ""
  putStrLn $ "sindUngleich    {(B,B)}    {(B,B)}: " ++ (show $ sindUngleich (bIPV1) (bIPV1))
  putStrLn $ "sindUngleich    {(B,B)}    {(E,E)}: " ++ (show $ sindUngleich (bIPV1) (eIPV1))
  putStrLn $ "sindUngleich {(B,B),(E,E)} {E, B}: " ++ (show $ sindUngleich (beIPV1) (ebIPV1))
  putStrLn ""
  putStrLn $ "istTeilmenge    {(B,B)}    {(B,B)}: " ++ (show $ istTeilmenge (bIPV1) (bIPV1))
  putStrLn $ "istTeilmenge {(B,B),(E,E)} {E, B}: " ++ (show $ istTeilmenge (beIPV1) (ebIPV1))
  putStrLn $ "istTeilmenge    {(B,B)} {(B,B),(E,E)}: " ++ (show $ istTeilmenge (bIPV1) (beIPV1))
  putStrLn $ "istTeilmenge {(B,B),(E,E)}    {(B,B)}: " ++ (show $ istTeilmenge (beIPV1) (bIPV1))
  putStrLn ""
  putStrLn $ "istEchteTeilmenge    {(B,B)}    {(B,B)}: " ++ (show $ istEchteTeilmenge (bIPV1) (bIPV1))
  putStrLn $ "istEchteTeilmenge {(B,B),(E,E)} {E, B}: " ++ (show $ istEchteTeilmenge (beIPV1) (ebIPV1))
  putStrLn $ "istEchteTeilmenge    {(B,B)} {(B,B),(E,E)}: " ++ (show $ istEchteTeilmenge (bIPV1) (beIPV1))
  putStrLn $ "istEchteTeilmenge {(B,B),(E,E)}    {(B,B)}: " ++ (show $ istEchteTeilmenge (beIPV1) (bIPV1))
  putStrLn ""
  putStrLn $ "istObermenge    {(B,B)}    {(B,B)}: " ++ (show $ istObermenge (bIPV1) (bIPV1))
  putStrLn $ "istObermenge {(B,B),(E,E)} {E, B}: " ++ (show $ istObermenge (beIPV1) (ebIPV1))
  putStrLn $ "istObermenge    {(B,B)} {(B,B),(E,E)}: " ++ (show $ istObermenge (bIPV1) (beIPV1))
  putStrLn $ "istObermenge {(B,B),(E,E)}    {(B,B)}: " ++ (show $ istObermenge (beIPV1) (bIPV1))
  putStrLn ""
  putStrLn $ "istEchteObermenge    {(B,B)}    {(B,B)}: " ++ (show $ istEchteObermenge (bIPV1) (bIPV1))
  putStrLn $ "istEchteObermenge {(B,B),(E,E)} {E, B}: " ++ (show $ istEchteObermenge (beIPV1) (ebIPV1))
  putStrLn $ "istEchteObermenge    {(B,B)} {(B,B),(E,E)}: " ++ (show $ istEchteObermenge (bIPV1) (beIPV1))
  putStrLn $ "istEchteObermenge {(B,B),(E,E)}    {(B,B)}: " ++ (show $ istEchteObermenge (beIPV1) (bIPV1))
  putStrLn ""
  putStrLn $ "sindElementeFremd {(B,B)}    {(E,E)}: " ++ (show $ sindElementeFremd (bIPV1) (eIPV1))
  putStrLn $ "sindElementeFremd {(B,B)} {(B,B),(E,E)}: " ++ (show $ sindElementeFremd (bIPV1) (beIPV1))
  putStrLn ""
  putStrLn $ "sindQuerUeberlappend    {(B,B)}    {(B,B)}: " ++ (show $ sindQuerUeberlappend (bIPV1) (bIPV1))
  putStrLn $ "sindQuerUeberlappend    {(B,B)}    {(E,E)}: " ++ (show $ sindQuerUeberlappend (bIPV1) (eIPV1))
  putStrLn $ "sindQuerUeberlappend {(B,B),(G,G)} {(B,B), (I,I)}: " ++ (show $ sindQuerUeberlappend (bgIPV1) (biIPV1))
  putStrLn $ "sindQuerUeberlappend    {(B,B)} {(B,B),(E,E)}: " ++ (show $ sindQuerUeberlappend (bIPV1) (beIPV1))
  putStrLn $ "sindQuerUeberlappend {(B,B),(E,E)}    {(B,B)}: " ++ (show $ sindQuerUeberlappend (beIPV1) (bIPV1))
  putStrLn ""
  putStrLn $ "istLinkstotal {(B,B), (E,B), (G,B), (I,B), (K,B), (L,B), (P,B), (S,B), (W,B)} : " ++ show (istLinkstotal linkstotal_true)
  putStrLn $ "istLinkstotal {(B,E), (E,G)}: " ++ show (istLinkstotal beispiel2)
  putStrLn ""
  putStrLn $ "istRechtstotal {(B,B), (B,E), (B,G), (B,I), (B,K), (B,L), (B,P), (B,S), (B,W)} : " ++ show (istRechtstotal rechtstotal_true)
  putStrLn $ "istRechtstotal {(B,E), (E,G)}: " ++ show (istRechtstotal beispiel2)
  putStrLn ""
  putStrLn $ "istReflexiv {(B,B), (E,E)}: " ++ show (istReflexiv reflexiv_true)
  putStrLn $ "istReflexiv {(B,E), (E,G)}: " ++ show (istReflexiv beispiel2)
  putStrLn ""
  putStrLn $ "istSymmetrisch {(B,E), (E,B), (B,B)}: " ++ show (istSymmetrisch symmetrisch_true)
  putStrLn $ "istSymmetrisch {(B,E), (E,G)}: " ++ show (istSymmetrisch beispiel2)
  putStrLn ""
  putStrLn $ "transitiv_true {(B,E), (E,G), (B,G)}: " ++ show (istTransitiv transitiv_true)
  putStrLn $ "istTransitiv {(B,E), (E,G)}: " ++ show (istTransitiv beispiel2)
  putStrLn ""
  putStrLn ""
  putStrLn "----------(b)------------------------------------------------------------------------------------------------"
  let bIPV3 = \e -> if e == (B,B)                                        then True else False
      eIPV3 = \e -> if e == (E,E)                                        then True else False
      beIPV3 = \e -> if e == (B,B) || e == (E,E)                         then True else False
      ebIPV3 = \e -> if e == (E,E) || e == (B,B)                         then True else False
      bgIPV3 = \e -> if e == (B,B) || e == (G,G)                         then True else False
      biIPV3 = \e -> if e == (B,B) || e == (I,I)                         then True else False

  putStrLn ""
  putStrLn $ "leereMenge: " ++ zeige (leereMenge :: IstPartnerstadtVon3)
  putStrLn $ "allMenge  : " ++ zeige (allMenge   :: IstPartnerstadtVon3)
  putStrLn ""
  putStrLn "istMenge ist die Protoimplementierung"
  putStrLn ""
  putStrLn $ "vereinige  {} {(B,B)}: " ++ (zeige . vereinige leereMenge $ (IPV3(MT3 bIPV3)))
  putStrLn $ "vereinige {(B,B)} {(B,B)}: " ++ (zeige $ vereinige (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "vereinige {(B,B)} {(E,E)}: " ++ (zeige $ vereinige (IPV3(MT3 bIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn ""
  putStrLn $ "schneide  {}    {(B,B)}: " ++ (zeige . schneide leereMenge $ IPV3(MT3 bIPV3))
  putStrLn $ "schneide {(B,B)}    {(B,B)}: " ++ (zeige $ schneide (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "schneide {(B,B)} {B, E}: " ++ (zeige $ schneide (IPV3(MT3 bIPV3)) (IPV3(MT3 beIPV3)))
  putStrLn ""
  putStrLn $ "zieheab     {} {(B,B)}: " ++ (zeige . zieheab leereMenge $ IPV3(MT3 bIPV3))
  putStrLn $ "zieheab    {(B,B)} {(B,B)}: " ++ (zeige $ zieheab (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "zieheab {(B,B),(E,E)} {(B,B)}: " ++ (zeige $ zieheab (IPV3(MT3 beIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn ""
  putStrLn $ "komplementiere . zieheab allMenge $ {(B,B)}: " ++ (zeige . komplementiere . zieheab allMenge $ IPV3(MT3 bIPV3))
  putStrLn ""
  putStrLn $ "sindGleich    {(B,B)}    {(B,B)}: " ++ (show $ sindGleich (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "sindGleich    {(B,B)}    {(E,E)}: " ++ (show $ sindGleich (IPV3(MT3 bIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn $ "sindGleich {(B,B),(E,E)} {E, B}: " ++ (show $ sindGleich (IPV3(MT3 beIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn ""
  putStrLn $ "sindUngleich    {(B,B)}    {(B,B)}: " ++ (show $ sindUngleich (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "sindUngleich    {(B,B)}    {(E,E)}: " ++ (show $ sindUngleich (IPV3(MT3 bIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn $ "sindUngleich {(B,B),(E,E)} {E, B}: " ++ (show $ sindUngleich (IPV3(MT3 beIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn ""
  putStrLn $ "istTeilmenge    {(B,B)}    {(B,B)}: " ++ (show $ istTeilmenge (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "istTeilmenge {(B,B),(E,E)} {E, B}: " ++ (show $ istTeilmenge (IPV3(MT3 beIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn $ "istTeilmenge    {(B,B)} {(B,B),(E,E)}: " ++ (show $ istTeilmenge (IPV3(MT3 bIPV3)) (IPV3(MT3 beIPV3)))
  putStrLn $ "istTeilmenge {(B,B),(E,E)}    {(B,B)}: " ++ (show $ istTeilmenge (IPV3(MT3 beIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn ""
  putStrLn $ "istEchteTeilmenge    {(B,B)}    {(B,B)}: " ++ (show $ istEchteTeilmenge (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "istEchteTeilmenge {(B,B),(E,E)} {E, B}: " ++ (show $ istEchteTeilmenge (IPV3(MT3 beIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn $ "istEchteTeilmenge    {(B,B)} {(B,B),(E,E)}: " ++ (show $ istEchteTeilmenge (IPV3(MT3 bIPV3)) (IPV3(MT3 beIPV3)))
  putStrLn $ "istEchteTeilmenge {(B,B),(E,E)}    {(B,B)}: " ++ (show $ istEchteTeilmenge (IPV3(MT3 beIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn ""
  putStrLn $ "istObermenge    {(B,B)}    {(B,B)}: " ++ (show $ istObermenge (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "istObermenge {(B,B),(E,E)} {E, B}: " ++ (show $ istObermenge (IPV3(MT3 beIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn $ "istObermenge    {(B,B)} {(B,B),(E,E)}: " ++ (show $ istObermenge (IPV3(MT3 bIPV3)) (IPV3(MT3 beIPV3)))
  putStrLn $ "istObermenge {(B,B),(E,E)}    {(B,B)}: " ++ (show $ istObermenge (IPV3(MT3 beIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn ""
  putStrLn $ "istEchteObermenge    {(B,B)}    {(B,B)}: " ++ (show $ istEchteObermenge (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "istEchteObermenge {(B,B),(E,E)} {E, B}: " ++ (show $ istEchteObermenge (IPV3(MT3 beIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn $ "istEchteObermenge    {(B,B)} {(B,B),(E,E)}: " ++ (show $ istEchteObermenge (IPV3(MT3 bIPV3)) (IPV3(MT3 beIPV3)))
  putStrLn $ "istEchteObermenge {(B,B),(E,E)}    {(B,B)}: " ++ (show $ istEchteObermenge (IPV3(MT3 beIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn ""
  putStrLn $ "sindElementeFremd {(B,B)}    {(E,E)}: " ++ (show $ sindElementeFremd (IPV3(MT3 bIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn $ "sindElementeFremd {(B,B)} {(B,B),(E,E)}: " ++ (show $ sindElementeFremd (IPV3(MT3 bIPV3)) (IPV3(MT3 beIPV3)))
  putStrLn ""
  putStrLn $ "sindQuerUeberlappend    {(B,B)}    {(B,B)}: " ++ (show $ sindQuerUeberlappend (IPV3(MT3 bIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn $ "sindQuerUeberlappend    {(B,B)}    {(E,E)}: " ++ (show $ sindQuerUeberlappend (IPV3(MT3 bIPV3)) (IPV3(MT3 eIPV3)))
  putStrLn $ "sindQuerUeberlappend {(B,B),(G,G)} {(B,B), (I,I)}: " ++ (show $ sindQuerUeberlappend (IPV3(MT3 bgIPV3)) (IPV3(MT3 biIPV3)))
  putStrLn $ "sindQuerUeberlappend    {(B,B)} {(B,B),(E,E)}: " ++ (show $ sindQuerUeberlappend (IPV3(MT3 bIPV3)) (IPV3(MT3 beIPV3)))
  putStrLn $ "sindQuerUeberlappend {(B,B),(E,E)}    {(B,B)}: " ++ (show $ sindQuerUeberlappend (IPV3(MT3 beIPV3)) (IPV3(MT3 bIPV3)))
  putStrLn ""