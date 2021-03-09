--STOIAN MIRUNA MARIA
--325CB
module Query where

import UserInfo
import Rating
import Movie
import Data.List

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry] 

type ColSeparator = Char
type LnSeparator = Char

-- TODO 1
--IMPARTE STRINGUL
splitBy :: Char -> String -> [String]
splitBy c [] = [[]]
splitBy c (s : string) 
                    | s /= c = (s:head(splitBy c string)):(tail(splitBy c string))
                    |otherwise="":(splitBy c  string)

--RETURNEAZA TOATA LISTA MAI PUTIN CE ESTE NULL PE ULTIMA POZITIE
desp3 colSep lnSep x = init (desp1 colSep lnSep x)

--IMPARTE LINIILE DUPA \n
desp1::Char->Char->String->[String]
desp1 colSep lnSep  x = splitBy lnSep x

--IMPARTE LINIILE DUPA SEPARATORUL DE COLOANA
desp2::Char->Char->[String]->[[String]]
desp2 colSep lnSep [] = []
desp2 colSep lnSep (x:xs) = splitBy colSep x:(desp2 colSep lnSep xs)

--RETURNEAZA STRINGUL PARSAT
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table colSep lnSep var = Table (head (desp2 colSep lnSep (desp3 colSep lnSep var))) ((tail (desp2 colSep lnSep (desp3 colSep lnSep var))))

user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

-- TODO 2

--CALCULEAZA MAXIMUL PE COLOANE
calcMaxCol :: [Entry] -> [Integer]
calcMaxCol intrari = if length (head intrari) /= 0 then (calcCelMaiLungCuv (map head intrari) 0):(calcMaxCol (map tail intrari)) else []

--RETURNEAZA LUNGIMEA CELUI MAI LUNG CUVANT
calcCelMaiLungCuv::[String]->Integer->Integer
calcCelMaiLungCuv [] max = max
calcCelMaiLungCuv (x:xs) max
                    |(fromIntegral (length x)) > max = calcCelMaiLungCuv xs (fromIntegral (length x))
                    |otherwise = calcCelMaiLungCuv xs max

--CALCULEAZA SUMA LUNGIMILOR  CELOR MAI LUNGI CUVINTE
sumaInt :: [Integer] ->Integer 
sumaInt [] = 0
sumaInt (x:xs) = x + sumaInt xs

--AFISEAZA LINIUTELE TABELULUI
randLiniute ::[Integer] ->String
randLiniute lung = (take (fromIntegral ((sumaInt lung) + (fromIntegral (length lung)) + 1)) (repeat '-')) ++ "\n"

--SCRIE HEADERUL
afisareHeader :: [String]->[Integer] ->String
afisareHeader [] _ = "|" ++ "\n"
afisareHeader (field1:files) (lung : lungs) = "|" ++ field1 ++ (take (fromIntegral (lung - (fromIntegral (length field1)))) (repeat ' ')) 
                                ++ (afisareHeader files lungs)
--SCRIE VALORILE DIN TABEL
afisareEntries :: [[String]] -> [Integer] ->String
afisareEntries [] lungs = []
afisareEntries (entry : entrie) lungs = (afisareHeader entry lungs ) ++ (afisareEntries entrie lungs)

instance Show Table where
    show (Table header entries) =randLiniute (calcMaxCol (header:entries)) ++
                                (afisareHeader header (calcMaxCol (header:entries))) ++ 
                                randLiniute (calcMaxCol (header:entries)) ++
                                (afisareEntries entries (calcMaxCol (header:entries))) ++
                                randLiniute (calcMaxCol (header:entries))

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

--TRANSPUSA ENTRIES
concatCol ::Table ->[[String]]
concatCol (Table header entries)
                        |length (head entries) > 0 = (map head entries):(concatCol (Table header (map tail entries)))
                        |otherwise = []

--GASESTE COLOANELE SPECIFICE FIECARUI HEADER
findAllCol :: [String] -> Table -> [[String]]
findAllCol [] _ = []
findAllCol (col:xs) (Table header entries) = (findCol col header (concatCol (Table header entries))):(findAllCol xs (Table header entries)) 

findCol :: String -> [String] -> [[String]] -> [String] 
findCol str [] _ = []
findCol str (x:xs) (y:ys) = if str == x then y else (findCol str xs ys)

-- TODO 3

getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter= undefined

eliminaJust (Nothing) = -1
eliminaJust (Just x) = x

--GASESTE PE A CATA POZITIE IN LISTA SE AFLA VALOAREA DORITA
count::Int->String->Table->Int
count number field (Table [] entries) = (-1)
count number field (Table header entries) = if field == (head header) then number else (count (number +1) field (Table (tail header) entries))

--CONDITII DE VERIFICARE A VALORILOR IN FUNCTIE DE FIECARE FILTER
verif1 ::Integer ->Int->[String]->Bool
verif1  val poz lista
                |val > (read (lista !! poz)::Integer) = True
                |otherwise = False
verif2 ::String ->Int->[String]->Bool
verif2  val poz lista
                |val == (lista !! poz) = True
                |otherwise = False

verif3 :: [String] ->Int->[String] ->Bool
verif3 str indiceField lista
                    |eliminaJust(elemIndex (lista !! indiceField) str) /= -1 =True
                    |otherwise = False

verif4::[String]->Int->[String]->Bool
verif4 str indiceField lista
                    |eliminaJust(elemIndex (lista !! indiceField) str) /= -1 =False
                    |otherwise = True

verif5 :: Integer ->Int->[String] ->Bool
verif5  val poz lista
                |val <= (read (lista !! poz)::Integer) =True
                |otherwise = False

verif6 :: String ->Int ->[String] ->Bool
verif6  val poz lista
                |val == (lista !! poz) =False
                |otherwise = True

--CAUTA VALORILE PORTIVITE FIECAREI CONDITII 

cauta1 :: Integer ->Int ->[[String]]->[[String]]
cauta1 valoare pozitie [] = []
cauta1 valoare pozitie (x:xs) = if (verif1 valoare pozitie x) == True 
					then x:(cauta1 valoare pozitie xs) else (cauta1 valoare pozitie xs)

cauta2 :: String ->Int->[[String]] ->[[String]]
cauta2 valoare pozitie [] = []
cauta2 valoare pozitie (x:xs) = if (verif2 valoare pozitie x) == True 
					then x:(cauta2 valoare pozitie xs) else (cauta2 valoare pozitie xs)

cauta3 :: [String] ->Int->[[String]] ->[[String]]
cauta3 lista indiceField [] = []
cauta3 lista indiceField (x:xs) = if (verif3 lista indiceField x) == True 
					then x:(cauta3 lista indiceField xs) else (cauta3 lista indiceField xs)

cauta4 :: [String] -> Int -> [[String]] -> [[String]]
cauta4 lista indiceField [] = []
cauta4 lista indiceField (x:xs) = if (verif4 lista indiceField x) == True 
					then x:(cauta4 lista indiceField xs) else (cauta4 lista indiceField xs)

cauta5 :: Integer ->Int -> [[String]] -> [[String]]
cauta5 valoare pozitie [] = []
cauta5 valoare pozitie (x:xs) = if (verif5 valoare pozitie x) == True 
					then x:(cauta5 valoare pozitie xs) else (cauta5 valoare pozitie xs)

cauta6 :: String -> Int ->[[String]] -> [[String]]
cauta6 valoare pozitie [] = []
cauta6 valoare pozitie (x:xs) = if (verif6 valoare pozitie x) == True 
					then x:(cauta6 valoare pozitie xs) else (cauta6 valoare pozitie xs)

--PRIMESTE TABELUL ORIGINAL SI INTOARCE TABELUL 
--CE CONTINE VALORILE CE RESPECTA CONDITIILE 
cautaInTabel1 ::String->Integer->Table->Table
cautaInTabel1 field number (Table header entries) = 
				(Table header (cauta1 number (count 0 field (Table header entries)) entries))

cautaInTabel2 ::String->String->Table->Table
cautaInTabel2 field number (Table header entries) = 
				(Table header (cauta2 number (count 0 field (Table header entries)) entries))

cautaInTabel3::String->[String]->Table->Table
cautaInTabel3 field lista (Table header entries) = 
				(Table header (cauta3 lista (count 0 field (Table header entries)) entries))

cautaInTabel4::String->[String]->Table->Table
cautaInTabel4 field lista (Table header entries) = 
				(Table header (cauta4 lista (count 0 field (Table header entries)) entries))

cautaInTabel5::String->Integer->Table->Table
cautaInTabel5 field number (Table header entries) = 
				(Table header (cauta5 number (count 0 field (Table header entries)) entries))

cautaInTabel6::String->String->Table->Table
cautaInTabel6 field number (Table header entries) = 
				(Table header (cauta6 number (count 0 field (Table header entries)) entries))

--CONCATENARE A DOUA TABELE
concatenare::Table->Table->Table
concatenare (Table header1 entries1) (Table header2 entries2) = 
				Table header1 (entries1 ++ entries2)

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom t) = t
eval (Select list t) =  Table list (transpose (findAllCol list (eval t)))
eval (SelectLimit list integ t) = Table list (take (fromIntegral integ) 
								(transpose (findAllCol list (eval t))))
eval (Cosine (Atom t)) = t
eval ((Atom t) :|| (Atom t')) = t
eval (Filter (Lt field number) t) = cautaInTabel1 field number (eval t)
eval (Filter (Eq field string) t) = cautaInTabel2 field string (eval t)
eval (Filter (In field string) t) = cautaInTabel3 field string (eval t)
eval (Filter (Not (In field lista)) t) = cautaInTabel4 field lista (eval t)
eval (Filter (Not (Lt field number)) t) = cautaInTabel5 field number (eval t)
eval (Filter (Not (Eq field number)) t) = cautaInTabel6 field number (eval t)
eval ( t1 :|| t2 ) = concatenare (eval t1) (eval t2)


-- TODO 5

f ::Table ->String
f (Table header entries) = head (head entries)

same_zone :: String -> Query
same_zone string = 	Select ["user_id", "occupation"] (Filter (Not (Eq "user_id" string)) 
					(Filter ( Eq "zone" ( f (eval (Select  ["zone"] (Filter (Eq "user_id" string) 
					(Atom user_info))))))  (Atom user_info)))


male_within_age :: Integer -> Integer -> Query
male_within_age x y = Select ["occupation", "zone"] $ Filter (Lt "age" y) $ 
                    Filter (Not (Lt "age" (x+1))) $ Filter (Eq "sex" "M") $ Atom user_info

mixed :: [String] -> [String] -> Int -> Query
mixed str1 str2 varsta = Select ["user_id"] $ Filter (Lt "age" (fromIntegral(varsta))) $
						 Filter (In "zone" str1) $ Filter (In "occupation" str2) $ Atom user_info 