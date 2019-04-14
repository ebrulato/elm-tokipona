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


type PRE_VERB
    = PRE_VERB String
    | NO_PRE_VERB


type VERB
    = VERB String
    | BE


type VERB_TRANSITIVE
    = VERB_TRANSITIVE String
    | NO_VERB_TRANSITIVE


type ADJECTIVE
    = ADJECTIVE String
    | ADJ


type ADVERB
    = ADVERB String
    | NO_ADVERB
    | ADV


type alias GRAMMAR =
    String


type WORD
    = WORD TOKIPONA WORD_KIND NOUN PRE_VERB VERB VERB_TRANSITIVE ADJECTIVE ADVERB
    | PARTICLE TOKIPONA GRAMMAR
    | ERROR String


type WORD_KIND
    = NOUN_KIND
    | PRE_VERB_KIND
    | VERB_KIND
    | VERB_TRANSITIVE_KIND
    | ADJECTIVE_KIND
    | ADVERB_KIND
    | GRAMMAR_KIND
    | UNKNOW_KIND


tokiponaWords : Dict String WORD
tokiponaWords =
    fromList
        [ -- lesson #3
          ( "jan", WORD "jan" NOUN_KIND (NOUN "people; human, being, person, somebody, anybody") NO_PRE_VERB BE (VERB_TRANSITIVE "to personify; to humanize, to personalize") (ADJECTIVE "human, somebody’s, personal, of people") (ADVERB "human, somebody’s, personal, of people") )
        , ( "mi", WORD "mi" NOUN_KIND (NOUN "I; me, we, us") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "my; our") ADV )
        , ( "moku", WORD "moku" VERB_KIND (NOUN "food; meal") NO_PRE_VERB (VERB "to ingest; to eat, to drink, to consume, to swallow") (VERB_TRANSITIVE "to ingest; to eat, to drink, to consume, to swallow") (ADJECTIVE "eating") (ADVERB "eating") )
        , ( "sina", WORD "sina" NOUN_KIND (NOUN "you") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "yours") ADV )
        , ( "suli", WORD "suli" ADJECTIVE_KIND (NOUN "size") NO_PRE_VERB BE (VERB_TRANSITIVE "to enlarge, to lengthen") (ADJECTIVE "big; heavy, large, long, tall; important; adult") (ADVERB "big; tall, long, adult, important") )
        , ( "suno", WORD "suno" NOUN_KIND (NOUN "sun; light, brightness, glow, radiance, shine; light source") NO_PRE_VERB BE (VERB_TRANSITIVE "to light; to illumine") (ADJECTIVE "sunny, sunnily") (ADVERB "sunny, sunnily") )
        , ( "telo", WORD "telo" NOUN_KIND (NOUN "water; liquid, fluid, wet substance; beverage; juice, sauce") NO_PRE_VERB BE (VERB_TRANSITIVE "to wash with water; to water,to put water to, to melt, to liquify") (ADJECTIVE "wett; slobbery, moist, damp, humid, sticky, sweaty, dewy, drizzly") (ADVERB "wett; slobbery, moist, damp, humid, sticky, sweaty, dewy, drizzly") )

        -- TODO interjection "pona!" conditional "pona la"
        , ( "pona", WORD "suli" ADJECTIVE_KIND AS_NOUN NO_PRE_VERB BE (VERB_TRANSITIVE "to fix; to improve, to repair, to make good") (ADJECTIVE "good; simple, positive, nice, correct, right, useful; friendly, peaceful") (ADVERB "good; simple, positive, nice, correct, right") )
        , ( "li", PARTICLE "li" "separates some subjects (especially third-person) from the verb" )

        -- lesson #4
        , ( "ilo", WORD "ilo" NOUN_KIND (NOUN "tool; implement, machine, device; thing used for a specific purpose") NO_PRE_VERB BE (VERB_TRANSITIVE "{to hack; to transform something as a tool}") (ADJECTIVE "useful") (ADVERB "usefully") )
        , ( "kili", WORD "kili" NOUN_KIND (NOUN "fruit; vegetable, mushroom") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "fruity") (ADVERB "fruity") )
        , ( "ni", WORD "ni" NOUN_KIND (NOUN "this, that") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "this, that") NO_ADVERB )
        , ( "ona", WORD "ona" NOUN_KIND (NOUN "he, she, it") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "his, her, its") NO_ADVERB )
        , ( "pipi", WORD "pipi" NOUN_KIND (NOUN "bug; insect, ant, spider") NO_PRE_VERB BE NO_VERB_TRANSITIVE ADJ ADV )
        , ( "ma", WORD "ma" NOUN_KIND (NOUN "area; earth, land; outdoor area; world; region, country, territory; soil") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "countrified; outdoor, alfresco, open-air") NO_ADVERB )
        , ( "ijo", WORD "ijo" NOUN_KIND (NOUN "something; anything, stuff, thing, object") NO_PRE_VERB BE (VERB_TRANSITIVE "to objectify") (ADJECTIVE "of something") (ADVERB "of something") )
        , ( "jo", WORD "jo" VERB_TRANSITIVE_KIND (NOUN "possession; having, content") NO_PRE_VERB BE (VERB_TRANSITIVE "to have; to carry, contain, hold") (ADJECTIVE "private; personal") (ADVERB "private; personal") )
        , ( "lukin", WORD "lukin" VERB_KIND (NOUN "view; look, glance, sight, gaze, glimpse, seeing, vision") (PRE_VERB "to seek to; look for, try to") (VERB "to look; to pay attention, to examine, to observe, to read, to watch") (VERB_TRANSITIVE "to see; to look at, to watch, to read") (ADJECTIVE "visual(ly)") (ADVERB "visual(ly)") )
        , ( "oko", WORD "oko" NOUN_KIND (NOUN "eye") NO_PRE_VERB (VERB "to see") (VERB_TRANSITIVE "to see") (ADJECTIVE "optical; eye-") NO_ADVERB )

        -- TODO interjection "pakala!"
        , ( "pakala", WORD "pakala" VERB_KIND (NOUN "accident; blunder, mistake, destruction, damage, breaking") NO_PRE_VERB (VERB "to screw up; to fall apart, to break") (VERB_TRANSITIVE "to screw up; to ruin, to break, to hurt, to injure, to damage, to destroy") (ADJECTIVE "destroyed; ruined, demolished, shattered, wrecked") (ADVERB "destroyed; ruined, demolished, shattered, wrecked") )
        , ( "unpa", WORD "unpa" VERB_KIND (NOUN "sex; sexuality") NO_PRE_VERB (VERB "to have sex") (VERB_TRANSITIVE "to have sex with; to sleep with, to fuck") (ADJECTIVE "erotic, sexual") (ADVERB "erotic, sexual") )
        , ( "wile", WORD "wile" VERB_KIND (NOUN "desire; need, will") (PRE_VERB "want; need, wish, have to, must, will, should") (VERB "to want; need, wish, have to, must, will, should") (VERB_TRANSITIVE "to want; need, wish, have to, must, will, should") ADJ ADV )
        , ( "e", PARTICLE "e" "introduces direct object" )
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


word_e =
    getRawTokipona "e"


doPona : Bool -> String -> String
doPona pona str =
    if pona then
        let
            ( begin, end, idx ) =
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

                WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb ->
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

                        VERB_TRANSITIVE_KIND ->
                            case verbTransive of
                                VERB_TRANSITIVE vt ->
                                    vt

                                NO_VERB_TRANSITIVE ->
                                    "[ERROR : '" ++ tokipona ++ "' is not a transisive verb]"

                        PRE_VERB_KIND ->
                            case preVerb of
                                PRE_VERB pv ->
                                    pv

                                NO_PRE_VERB ->
                                    "[ERROR : '" ++ tokipona ++ "' is not a prev-verb]"

                        ADVERB_KIND ->
                            case adverb of
                                ADVERB a ->
                                    a

                                NO_ADVERB ->
                                    "[ERROR : '" ++ tokipona ++ "' is not an adverb]"

                                ADV ->
                                    "[ERROR: default kind is ADVERB but there is no definition]"

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

        WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb ->
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

                VERB_TRANSITIVE_KIND ->
                    case verbTransive of
                        VERB_TRANSITIVE vt ->
                            doPona pona vt

                        NO_VERB_TRANSITIVE ->
                            "[ERROR : '" ++ tokipona ++ "' is not a transisive verb]"

                PRE_VERB_KIND ->
                    case preVerb of
                        PRE_VERB pv ->
                            doPona pona pv

                        NO_PRE_VERB ->
                            "[ERROR : '" ++ tokipona ++ "' is not a prev-verb]"

                ADVERB_KIND ->
                    case adverb of
                        ADVERB a ->
                            doPona pona a

                        NO_ADVERB ->
                            "[ERROR : '" ++ tokipona ++ "' is not an adverb]"

                        ADV ->
                            "('" ++ getDefaultKind pona w ++ "' as a adverb)"

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

        WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb ->
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
    , cod : List Item
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
                |> findCOD
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
    Group [] [] [] words


cutList : (a -> Bool) -> Bool -> List a -> ( List a, List a, Int )
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
            ( List.take idx l, List.drop idx l, idx )

        else
            ( List.take (idx - 1) l, List.drop idx l, idx )

    else
        ( [], l, 0 )


convertWords2Noun : List WORD -> List Item
convertWords2Noun words =
    List.map (\w -> ItemWord w NOUN_KIND) words


findSubject : Group -> Group
findSubject g =
    let
        subject =
            [ cutList (\w -> w == word_mi) True g.words
            , cutList (\w -> w == word_sina) True g.words
            , cutList (\w -> w == word_li) False g.words
            ]
                |> List.filter (\( _, _, idx ) -> idx > 0)
                |> List.sortBy (\( _, _, idx ) -> idx)
                |> List.head
    in
    case subject of
        Just ( s, o, idx ) ->
            Group (convertWords2Noun s) g.verb g.cod o

        Nothing ->
            Group (convertWords2Noun g.words) g.verb g.cod []



{-
   verb
   verb_transitive
   pre_verb verb
   pre_verb verb_transitive
   verb adverb*
   verb_transitive adverb*
   pre_verb verb adverb*
   pre_verb verb_transitive adverb*
-}


convertWords2Verb : Bool -> Bool -> Bool -> List WORD -> List Item
convertWords2Verb firstCall mainVerbFounded transitive words =
    case List.head words of
        Just w ->
            if firstCall then
                case w of
                    WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb ->
                        case preVerb of
                            PRE_VERB s ->
                                case List.tail words of
                                    Just l ->
                                        if List.length l > 0 then
                                            ItemWord w PRE_VERB_KIND :: convertWords2Verb False False transitive l

                                        else
                                            convertWords2Verb False False transitive words

                                    Nothing ->
                                        convertWords2Verb False False transitive words

                            NO_PRE_VERB ->
                                convertWords2Verb False False transitive words

                    _ ->
                        -- TODO : impossible case
                        convertWords2Verb False False transitive words

            else if not mainVerbFounded then
                if transitive then
                    case List.tail words of
                        Just l ->
                            ItemWord w VERB_TRANSITIVE_KIND :: convertWords2Verb False True transitive l

                        Nothing ->
                            ItemWord w VERB_TRANSITIVE_KIND :: []

                else
                    case List.tail words of
                        Just l ->
                            ItemWord w VERB_KIND :: convertWords2Verb False True transitive l

                        Nothing ->
                            ItemWord w VERB_KIND :: []

            else
                List.map (\word -> ItemWord word ADVERB_KIND) words

        Nothing ->
            []


findVerb : Group -> Group
findVerb g =
    let
        ( verb_e, others_e, idx_e ) =
            cutList (\w -> w == word_e) False g.words
    in
    if not (List.isEmpty verb_e) then
        Group g.subject (convertWords2Verb True False True verb_e) [] others_e

    else
        Group g.subject (convertWords2Verb True False False g.words) [] []


findCOD : Group -> Group
findCOD g =
    Group g.subject g.verb (convertWords2Noun g.words) []


getItems : Group -> List Item
getItems g =
    g.cod
        |> List.append g.verb
        |> List.append g.subject



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
