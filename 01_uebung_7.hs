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
  sindGleich :: m -> m -> Bool
  sindUngleich :: m -> m -> Bool
  istTeilmenge :: m -> m -> Bool
  istObermenge :: m -> m -> Bool
  istEchteTeilmenge :: m -> m -> Bool
  istEchteObermenge :: m -> m -> Bool
  sindElementeFremd :: m -> m -> Bool
  sindQuerUeberlappend :: m -> m -> Bool
  istKeinGueltigerMengenwert :: Fehlermeldung -> m
  nichtImplementierbar :: Fehlermeldung -> m
  zeige :: m -> MengeAlsZeichenreihe


--PROTOIMPLEMENTIERUNGEN

  komplementiere = zieheab allMenge

--Zwei Mengen sind gleich, wenn sie Teilmengen voneinander sind

  sindGleich m1 m2 = istTeilmenge m1 m2 && istTeilmenge m2 m1
  sindUngleich m1 = not . sindGleich m1

--Wenn A (echte) Obermenge von B ist, ist dann B (echte) Teilmenge von A

  istObermenge m1 m2 = istTeilmenge m2 m1
  istEchteObermenge m1 m2 = istEchteTeilmenge m2 m1 

--Eine Menge ist echte Teilmenge einer Anderen, wenn sie Teilmenge aber nicht gleich ist

  istEchteTeilmenge m1 m2 = istTeilmenge m1 m2 && not (sindGleich m1 m2)

--Zwei Mengen sind elementefremd, wenn ihrer Schnitt die Leeremenge ist

  sindElementeFremd m1 = sindGleich leereMenge . schneide m1

--Zwei Mengen sind quer-> ueberlappend, wenn sie...
--  ... mindestens ein Element gemeinsam haben
--  ... jeweils keine Teilmenge voneinander sind

  sindQuerUeberlappend m1 m2 =
      not (sindElementeFremd m1 m2)
      && not (istTeilmenge m1 m2)
      && not (istTeilmenge m2 m1)
  istKeinGueltigerMengenwert = error
  nichtImplementierbar = error


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


--Allgemeine Funktionen fuer MT1

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


--Hilffunktionen fuer MT1.

--Fehlermeldung fuer wenn ein oder mehrere Argumente nicht Menge sind.

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


instance Menge (MT2 Char) where
  leereMenge = Nichts
  allMenge = createMT2 defaultValue
  istMenge = istMengeMT2
  vereinige = vereinigeMT2
  schneide = schneideMT2
  zieheab = zieheabMT2
  istTeilmenge = istTeilmengeMT2
  zeige = zeigeMT2


instance Menge (MT2 Int) where
  leereMenge = Nichts
  allMenge = createMT2 defaultValue
  istMenge = istMengeMT2
  vereinige = vereinigeMT2
  schneide = schneideMT2
  zieheab = zieheabMT2
  istTeilmenge = istTeilmengeMT2
  zeige = zeigeMT2


--Allgemeine Funktionen fuer MT2.

istMengeMT2 :: Eq e =>MT2 e -> Bool
istMengeMT2              Nichts = True
istMengeMT2 (VerlaengereUm e m) = (not . isElem e) m && istMengeMT2 m

vereinigeMT2 :: Eq e =>MT2 e -> MT2 e -> MT2 e
vereinigeMT2 m1 m2
      | istMengeMT2 m1 && istMengeMT2 m2 = join m1 m2
      | otherwise                        = fehlermeldung

schneideMT2 :: Eq e =>MT2 e -> MT2 e -> MT2 e
schneideMT2 m1 m2
      | istMengeMT2 m1 && istMengeMT2 m2 = union m1 m2
      | otherwise                        = fehlermeldung

zieheabMT2 :: Eq e =>MT2 e -> MT2 e -> MT2 e
zieheabMT2 m1 m2
      | istMengeMT2 m1 && istMengeMT2 m2 = sub m1 m2
      | otherwise                        = fehlermeldung


istTeilmengeMT2 :: Eq e =>MT2 e -> MT2 e -> Bool
istTeilmengeMT2 m1 m2
      | istMengeMT2 m1 && istMengeMT2 m2 = isSubset m1 m2
      | otherwise                        = fehlermeldung

zeigeMT2 :: Show e =>MT2 e -> MengeAlsZeichenreihe
zeigeMT2 elems = "{" ++ (formatElems . toListMT2) elems ++ "}"


--Hilffunktionen fuer MT2.

--Wandle eine Liste von Dingen in einem MT2 um.

createMT2 :: [e] -> MT2 e
createMT2     [] = Nichts
createMT2 (x:xs) = VerlaengereUm x $ createMT2 xs

--Wandle ein MT2 in einer Liste seiner Elemente.

toListMT2 :: MT2 e -> [e]
toListMT2              Nichts = []
toListMT2 (VerlaengereUm e m) = e : (toListMT2 m)

--Ob ein Ding Element eines MT2s ist.

isElem :: Eq e =>e -> MT2 e -> Bool
isElem _ Nichts = False
isElem x (VerlaengereUm e m)
    | x == e    = True
    | otherwise = isElem x m

--Vereinige zwei MT2, wobei man akkumuliert das Ergebnis im ersten Argument.

join :: Eq e =>MT2 e -> MT2 e -> MT2 e
join      m Nichts = m
join m1 (VerlaengereUm e m2)
    --  wenn e in m1, dann ueberspringe ihn
    | isElem e m1 = join m1 m2
    | otherwise   = join (VerlaengereUm e m1) m2

--Schneide zwei MT2, wobei man akkumuliert das Ergebnis am Rueckweg der Rekursion.

union :: Eq e =>MT2 e -> MT2 e -> MT2 e
union Nichts      _ = Nichts
union (VerlaengereUm e m1) m2
    --  wenn e in m2, dann behalte es
    | isElem e m2 = VerlaengereUm e $ union m1 m2
    | otherwise   = union m1 m2

--Ziehe den zweiten MT2 vom Ersten ab, wobei man akkumuliert das Ergebnis am Rueckweg der Rekursion.

sub :: Eq e =>MT2 e -> MT2 e -> MT2 e
sub Nichts      _ = Nichts
sub (VerlaengereUm e m1) m2
    --  wenn e in m2, dann ueberspringe ihn
    | isElem e m2 = sub m1 m2
    | otherwise   = VerlaengereUm e $ sub m1 m2

--Ob der erste MT2 Teilmenge vom Zweiten ist.

isSubset :: Eq e =>MT2 e -> MT2 e -> Bool
isSubset Nichts _ = True
isSubset (VerlaengereUm e m1) m2
    --  wenn e in m2, pruefe m1 weiter
    | isElem e m2 = isSubset m1 m2
    | otherwise   = False


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


--Allgemeine Funktionen fuer MT3.

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



main = do
  putStrLn "-----------------------------------------------------A.1-----------------------------------------------------"
  putStrLn ""
  