module Tokipona exposing
    ( translate
    , translatePona
    )

{-| A Tokipone translator for Elm

#The exposes

I tried to use elm/parser but it is not very convenient for this case

@docs translate

-}

import Dict exposing (Dict, fromList, get)
import List exposing (head, tail)



-- import Parser exposing ((|.), (|=), Parser, keyword, map, oneOf, run, spaces, succeed)


type alias TOKIPONA =
    String


type NOUN
    = NOUN String
    | AS_NOUN


type VERB
    = VERB String
    | BE


type ADJECTIVE
    = ADJECTIVE String
    | ADJ


type alias GRAMMAR =
    String


type WORD
    = WORD TOKIPONA WORD_KIND NOUN VERB ADJECTIVE
    | PARTICLE TOKIPONA GRAMMAR
    | ERROR String


type WORD_KIND
    = NOUN_KIND
    | VERB_KIND
    | ADJECTIVE_KIND
    | GRAMMAR_KIND
    | UNKNOW_KIND


tokiponaWords : Dict String WORD
tokiponaWords =
    fromList
        [ ( "jan", WORD "jan" NOUN_KIND (NOUN "people; human being, person, somebody") BE ADJ )
        , ( "mi", WORD "mi" NOUN_KIND (NOUN "I; me, we, us") BE ADJ )
        , ( "moku", WORD "moku" VERB_KIND (NOUN "food") (VERB "to ingest; to eat, drink, consume, swallow") ADJ )
        , ( "sina", WORD "sina" NOUN_KIND (NOUN "you") BE ADJ )
        , ( "suli", WORD "suli" ADJECTIVE_KIND AS_NOUN BE (ADJECTIVE "big; heavy, large, long, tall; important; adult") )
        , ( "suno", WORD "suno" NOUN_KIND (NOUN "sun; light, brightness, glow, radiance, shine; light source") BE ADJ )
        , ( "telo", WORD "telo" NOUN_KIND (NOUN "water; liquid, fluid, wet substance; beverage") BE ADJ )
        , ( "pona", WORD "suli" ADJECTIVE_KIND AS_NOUN BE (ADJECTIVE "good; positive, useful; friendly, peaceful; simple") )
        , ( "li", PARTICLE "li" "separates some subjects (especially third-person) from the verb" )
        ]


getRawTokipona : String -> WORD
getRawTokipona word =
    let
        tp =
            get word tokiponaWords
    in
    case tp of
        Just w ->
            w

        Nothing ->
            ERROR word


word_mi =
    getRawTokipona "mi"


word_sina =
    getRawTokipona "sina"


word_li =
    getRawTokipona "li"


doPona : Bool -> String -> String
doPona pona str =
    if pona then
        let
            ( begin, end ) =
                cutList (\c -> c == ';') False (String.toList str)
        in
        if not (List.isEmpty begin) then
            String.fromList begin

        else
            String.fromList end

    else
        str


getDefaultKind : Bool -> WORD -> String
getDefaultKind pona w =
    let
        result =
            case w of
                ERROR s ->
                    "[ERROR: '" ++ s ++ "' is not a valid word]"

                PARTICLE tokipona grammar ->
                    grammar

                WORD tokipona defaultKind noun verb adjective ->
                    case defaultKind of
                        NOUN_KIND ->
                            case noun of
                                NOUN s ->
                                    s

                                AS_NOUN ->
                                    "[ERROR: default kind is NOUN but there is no definition]"

                        VERB_KIND ->
                            case verb of
                                VERB v ->
                                    v

                                BE ->
                                    "[ERROR: default kind is VERB but there is no definition]"

                        ADJECTIVE_KIND ->
                            case adjective of
                                ADJECTIVE a ->
                                    a

                                ADJ ->
                                    "[ERROR: default kind is ADJECTIVE but there is no definition]"

                        GRAMMAR_KIND ->
                            "[ERROR: " ++ tokipona ++ " is not a grammar particle]"

                        UNKNOW_KIND ->
                            "[ERROR: " ++ tokipona ++ " is not unknown]"
    in
    doPona pona result


rawTokiponaToString : Bool -> WORD_KIND -> WORD -> String
rawTokiponaToString pona kind w =
    case w of
        ERROR s ->
            "[ERROR: '" ++ s ++ "' is not a valid word]"

        PARTICLE tokipona grammar ->
            grammar

        WORD tokipona defaultKind noun verb adjective ->
            case kind of
                NOUN_KIND ->
                    case noun of
                        NOUN s ->
                            doPona pona s

                        AS_NOUN ->
                            "('" ++ getDefaultKind pona w ++ "' as a noun)"

                VERB_KIND ->
                    case verb of
                        VERB v ->
                            doPona pona v

                        BE ->
                            "('" ++ getDefaultKind pona w ++ "' as a verb or being...)"

                ADJECTIVE_KIND ->
                    case adjective of
                        ADJECTIVE a ->
                            doPona pona a

                        ADJ ->
                            "('" ++ getDefaultKind pona w ++ "' as an adjective)"

                GRAMMAR_KIND ->
                    "[ERROR: " ++ tokipona ++ " is not a grammar particle]"

                UNKNOW_KIND ->
                    "[ERROR: " ++ tokipona ++ " is not unknown]"


getKind : WORD -> WORD_KIND
getKind w =
    case w of
        ERROR s ->
            UNKNOW_KIND

        PARTICLE tokipona grammar ->
            GRAMMAR_KIND

        WORD tokipona defaultKind noun verb adjective ->
            defaultKind



{-

   current version
-}


translate : String -> String
translate phrase =
    internalTranslate phrase False


translatePona : String -> String
translatePona phrase =
    internalTranslate phrase True


internalTranslate : String -> Bool -> String
internalTranslate phrase pona =
    case String.length phrase of
        0 ->
            ""

        _ ->
            String.words phrase
                |> List.map getRawTokipona
                --|> Just
                |> translateTravel
                |> List.map (itemToString pona)
                |> String.join " / "


type Item
    = ItemWord WORD WORD_KIND


itemToString : Bool -> Item -> String
itemToString pona item =
    case item of
        ItemWord word wordKind ->
            rawTokiponaToString pona wordKind word


type alias Group =
    { subject : List Item
    , verb : List Item
    , words : List WORD
    }


translateTravel : List WORD -> List Item
translateTravel words =
    case List.length words of
        1 ->
            singletonAnalyse <| head words

        _ ->
            words
                |> listWord2Group
                |> findSubject
                |> findVerb
                |> getItems


singletonAnalyse : Maybe WORD -> List Item
singletonAnalyse word =
    case word of
        Just w ->
            ItemWord w (getKind w) :: []

        Nothing ->
            []


listWord2Group : List WORD -> Group
listWord2Group words =
    Group [] [] words


cutList : (a -> Bool) -> Bool -> List a -> ( List a, List a )
cutList f keep l =
    let
        ( idx, pos ) =
            List.foldl
                (\item ( i, p ) ->
                    if f item && (i == -1) then
                        ( p, p + 1 )

                    else
                        ( i, p + 1 )
                )
                ( -1, 1 )
                l
    in
    if idx > 0 then
        if keep then
            ( List.take idx l, List.drop idx l )

        else
            ( List.take (idx - 1) l, List.drop idx l )

    else
        ( [], l )


convertWords2Subject : List WORD -> List Item
convertWords2Subject words =
    List.map (\w -> ItemWord w NOUN_KIND) words


findSubject : Group -> Group
findSubject g =
    let
        ( subject_mi, others_mi ) =
            cutList (\w -> w == word_mi) True g.words

        ( subject_sina, others_sina ) =
            cutList (\w -> w == word_sina) True g.words

        ( subject_li, others_li ) =
            cutList (\w -> w == word_li) False g.words
    in
    if not (List.isEmpty subject_sina) then
        Group (convertWords2Subject subject_sina) g.verb others_sina

    else if not (List.isEmpty subject_mi) then
        Group (convertWords2Subject subject_mi) g.verb others_mi

    else if not (List.isEmpty subject_li) then
        Group (convertWords2Subject subject_li) g.verb others_li

    else
        Group [] g.verb g.words


convertWords2Verb : List WORD -> List Item
convertWords2Verb words =
    List.map (\w -> ItemWord w VERB_KIND) words


findVerb : Group -> Group
findVerb g =
    Group g.subject (convertWords2Verb g.words) []


getItems : Group -> List Item
getItems g =
    List.append g.subject g.verb



{-
   current version
-}
{-
   translateTravel : Maybe (List WORD) -> List ITEM
   translateTravel words =
       let
           l =
               case words of
                   Just aList ->
                       aList

                   Nothing ->
                       []

           w =
               head l
       in
       case w of
           Just word ->
               ITEM word (getKind word) :: (translateTravel <| tail l)

           Nothing ->
               []
-}
{-
   myDisplayDeadEnd : List Parser.DeadEnd -> String
   myDisplayDeadEnd lde =
       let
           displayDeadEnd pb =
               case pb of
                   Parser.Problem str ->
                       str

                   _ ->
                       "?"
       in
       lde
           |> List.map (\de -> "(" ++ String.fromInt de.row ++ ", " ++ String.fromInt de.col ++ ") " ++ displayDeadEnd de.problem)
           |> String.join ", "


   translate : String -> String
   translate str =
       if String.length str == 0 then
           ""

       else
           case run phraseParser str of
               Ok listItem ->
                   listItem
                       |> List.map itemToString
                       |> String.join " / "

               Err errors ->
                   myDisplayDeadEnd errors


   phraseParser : Parser (List ITEM)
   phraseParser =
       succeed identity
           |. spaces
           |= oneOf
               [ succeed (::)
                   |= map (\w -> ITEM (getRawTokipona "mi") (getKind (getRawTokipona "mi"))) (keyword "mi")
                   |= map (\w -> ITEM w VERB_KIND :: []) wordParser
               , -- single word with default kind
                 map (\w -> ITEM w (getKind w) :: []) wordParser
               ]


   wordParser : Parser WORD
   wordParser =
       succeed identity
           |. spaces
           |= oneOf
               [ map (\_ -> getRawTokipona "jan") (keyword "jan")
               , map (\_ -> getRawTokipona "mi") (keyword "mi")
               , map (\_ -> getRawTokipona "moku") (keyword "moku")
               , map (\_ -> getRawTokipona "sina") (keyword "sina")
               , map (\_ -> getRawTokipona "suli") (keyword "suli")
               , map (\_ -> getRawTokipona "suno") (keyword "suno")
               , map (\_ -> getRawTokipona "telo") (keyword "telo")
               , map (\_ -> getRawTokipona "pona") (keyword "pona")
               , map (\_ -> getRawTokipona "li") (keyword "li")

               --, map (\w -> getRawTokipona w) Parser.ignore
               ]
-}
