module Tokipona exposing
    ( translate
    , translateNimi, translateNimiPona, translatePona
    )

{-| A Tokipone translator for Elm

#The exposes

I tried to use elm/parser but it is not very convenient for this case

@docs translate

-}

import Dict exposing (get)
import Lipu exposing (..)
import List exposing (head, tail)
import Tools exposing (cutList, firstPos)



{-

   current version
-}


translate : String -> String
translate phrase =
    internalTranslate phrase False False


translatePona : String -> String
translatePona phrase =
    internalTranslate phrase True False


translateNimi : String -> String
translateNimi phrase =
    internalTranslate phrase False True


translateNimiPona : String -> String
translateNimiPona phrase =
    internalTranslate phrase True True


internalTranslate : String -> Bool -> Bool -> String
internalTranslate phrases pona nimi =
    case String.length (String.trim phrases) of
        0 ->
            ""

        _ ->
            if String.contains "." phrases then
                ponctuationSupport '.' phrases pona nimi

            else if String.contains ":" phrases then
                ponctuationSupport ':' phrases pona nimi

            else if String.contains "?" phrases then
                ponctuationSupport '?' phrases pona nimi

            else
                String.trim phrases
                    |> String.words
                    |> reduceWithLipu nimi
                    |> List.map getRawTokipona
                    |> List.map fixSimple
                    |> List.map fixForeign
                    |> translateTravel
                    |> List.map (itemToString pona)
                    |> String.join " / "


fixSimple word =
    case word of
        SIMPLE tokipona str ->
            convertSimple2Word (SIMPLE tokipona str)

        _ ->
            word


fixForeign word =
    case word of
        FOREIGN tokipona str ->
            convertForeign2Word (FOREIGN tokipona str)

        _ ->
            word


ponctuationSupport p phrases pona nimi =
    case cutList (\v -> v == p) False False (String.toList phrases) of
        ( firstPhrase, otherPhrases, _ ) ->
            String.trim <| internalTranslate (String.fromList firstPhrase) pona nimi ++ String.fromList [ p ] ++ " " ++ internalTranslate (String.fromList otherPhrases) pona nimi


reduceWithLipu : Bool -> List String -> List String
reduceWithLipu nimi words =
    if nimi then
        reduce2 [] <| reduce3 [] words

    else
        words


reduce3 : List String -> List String -> List String
reduce3 listHead listTail =
    case listTail of
        w1 :: w2 :: w3 :: t ->
            case Dict.get (w1 ++ " " ++ w2 ++ " " ++ w3) tokiponaLipu of
                Just wFounded ->
                    reduce3 (List.append listHead (getTokipona wFounded :: [])) t

                Nothing ->
                    reduce3 (List.append listHead (w1 :: [])) <| List.drop 1 listTail

        _ ->
            List.append listHead listTail


reduce2 : List String -> List String -> List String
reduce2 listHead listTail =
    case listTail of
        w1 :: w2 :: t ->
            case Dict.get (w1 ++ " " ++ w2) tokiponaLipu of
                Just wFounded ->
                    reduce2 (List.append listHead (getTokipona wFounded :: [])) t

                Nothing ->
                    reduce2 (List.append listHead (w1 :: [])) <| List.drop 1 listTail

        _ ->
            List.append listHead listTail


type Item
    = ItemWord WORD WORD_KIND
    | And_li


itemToString : Bool -> Item -> String
itemToString pona item =
    case item of
        ItemWord word wordKind ->
            rawTokiponaToString pona wordKind word

        And_li ->
            "and"


translateTravel : List WORD -> List Item
translateTravel words =
    case List.length words of
        1 ->
            singletonAnalyse <| head words

        _ ->
            findSubject words


singletonAnalyse : Maybe WORD -> List Item
singletonAnalyse word =
    case word of
        Just w ->
            ItemWord w (getKind w) :: []

        Nothing ->
            []


convertWords2Noun : Bool -> List WORD -> List Item
convertWords2Noun firstCall words =
    case List.head words of
        Just w ->
            if firstCall then
                case List.tail words of
                    Just l ->
                        ItemWord w NOUN_KIND :: convertWords2Noun (getKind w == LOC_KIND) l

                    Nothing ->
                        ItemWord w NOUN_KIND :: []

            else
                List.map (\word -> ItemWord word ADJECTIVE_KIND) words

        Nothing ->
            []


convertWords2Basic : List WORD -> List Item
convertWords2Basic words =
    case List.head words of
        Just w ->
            case List.tail words of
                Just l ->
                    let
                        kind =
                            getKind w
                    in
                    if (kind == VERB_KIND) || kind == VERB_TRANSITIVE_KIND then
                        findVerbs words True

                    else
                        convertWords2Noun True words

                Nothing ->
                    singletonAnalyse (Just w)

        Nothing ->
            []


findSubject : List WORD -> List Item
findSubject words =
    let
        subject =
            [ cutList (\w -> w == word_mi) True False words
            , cutList (\w -> w == word_sina) True False words
            , cutList (\w -> w == word_li) False False words
            ]
                |> List.filter (\( _, _, idx ) -> idx > 0)
                |> List.sortBy (\( _, _, idx ) -> idx)
                |> List.head
    in
    case subject of
        Just ( s, o, idx ) ->
            if is_mi_mute_or_lili_li_form words then
                case cutList (\w -> w == word_li) False False words of
                    ( s2, o2, idx2 ) ->
                        List.append (convertWords2Noun True s2) (findVerbs o2 False)

            else
                List.append (convertWords2Noun True s) (findVerbs o False)

        Nothing ->
            convertWords2Basic words


is_mi_mute_or_lili_li_form words =
    let
        pos_mi =
            firstPos word_mi words

        pos_sina =
            firstPos word_sina words

        pos_mute =
            firstPos word_mute words

        pos_lili =
            firstPos word_lili words

        pos_li =
            firstPos word_li words
    in
    (pos_mi > 0 && pos_mute > pos_mi && pos_li > pos_mute)
        || (pos_mi > 0 && pos_lili > pos_mi && pos_li > pos_lili)
        || (pos_sina > 0 && pos_mute > pos_sina && pos_li > pos_mute)
        || (pos_sina > 0 && pos_lili > pos_sina && pos_li > pos_lili)



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


convertWords2Verb : Bool -> Bool -> List WORD -> List Item
convertWords2Verb firstCall transitive words =
    case List.head words of
        Just w ->
            if firstCall then
                case w of
                    WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num prep ->
                        case preVerb of
                            PRE_VERB s ->
                                let
                                    nexts =
                                        List.drop 1 words
                                in
                                if
                                    isForm_lukin_with_preposition nexts w
                                        || isAlaQuestionForm w (Just nexts)
                                        --|| isForm_preVerb_and_mute_or_lili_or_sewi nexts
                                        || isAlaFormWithNoVerbAfter nexts
                                        || not (List.length nexts > 0)
                                then
                                    doMainVerbFounded (whichKind transitive) False False w (List.tail words)
                                    {-
                                       convertWords2Verb False transitive words

                                        else if nextWordIsAla nexts then
                                          ItemWord w PRE_VERB_KIND
                                              :: ItemWord word_ala ADVERB_KIND
                                              :: convertWords2Verb False transitive (isForm_wile_or_kama_with_preposition nexts w) (List.drop 1 nexts)
                                    -}

                                else
                                    doMainVerbFounded (whichKind transitive) True False w (List.tail words)

                            --ItemWord w PRE_VERB_KIND :: convertWords2Verb False transitive (isForm_wile_or_kama_with_preposition nexts w) nexts
                            NO_PRE_VERB ->
                                doMainVerbFounded (whichKind transitive) False False w (List.tail words)

                    --convertWords2Verb False transitive words
                    _ ->
                        []
                -- convertWords2Verb False transitive words

            else
                doMainVerbFounded (whichKind transitive) False False w (List.tail words)

        Nothing ->
            []


isAlaFormWithNoVerbAfter nexts =
    nextWordIsAla nexts && List.length nexts == 1


whichKind transitive =
    if transitive then
        VERB_TRANSITIVE_KIND

    else
        VERB_KIND



{-
   preverb + kepeken + x -> prever + verb + noun
   lukin + kepeken + x -> verb + preposition + noun
-}


doMainVerbFounded kind isPrevVerb afterPreVerb w words =
    case words of
        Just l ->
            if isForm_preVerb_and_mute_or_lili_or_sewi l then
                case List.head l of
                    Just wn ->
                        ItemWord w kind
                            :: adverbsOrNominalGroup False l

                    --:: doMainVerbFounded kind False False wn (Just (List.drop 1 l))
                    _ ->
                        ItemWord w VERB_KIND :: []

            else if isForm_wile_or_kama_with_preposition_or_wile_kama l w then
                case List.head l of
                    Just wn ->
                        ItemWord w PRE_VERB_KIND
                            :: ItemWord wn kind
                            :: adverbsOrNominalGroup True (List.drop 1 l)

                    _ ->
                        ItemWord w VERB_KIND :: []

            else if isForm_preVerb_kepeken l w then
                case List.head l of
                    Just wn ->
                        ItemWord w kind
                            :: ItemWord wn PREPOSITION_KIND
                            :: adverbsOrNominalGroup True (List.drop 1 l)

                    _ ->
                        ItemWord w VERB_KIND :: []

            else if isPrevVerb then
                case List.head l of
                    Just wn ->
                        if wn == word_ala then
                            let
                                tl =
                                    List.drop 1 l
                            in
                            case List.head tl of
                                Just wn2 ->
                                    ItemWord w PRE_VERB_KIND
                                        :: ItemWord word_ala ADVERB_KIND
                                        :: doMainVerbFounded kind False True wn2 (Just (List.drop 1 tl))

                                _ ->
                                    ItemWord w VERB_KIND
                                        :: ItemWord word_ala ADVERB_KIND
                                        :: []

                        else
                            ItemWord w PRE_VERB_KIND :: doMainVerbFounded kind False True wn (List.tail l)

                    _ ->
                        ItemWord w VERB_KIND :: []

            else if isAlaQuestionForm w words then
                ItemWord w kind
                    :: ItemWord word_ala ADVERB_KIND
                    :: ItemWord word_anu GRAMMAR_KIND
                    :: ItemWord w kind
                    :: managePreVerb w kind afterPreVerb (List.drop 2 l)

            else if nextWordIsAla l then
                ItemWord w kind
                    :: ItemWord word_ala ADVERB_KIND
                    :: managePreVerb w kind afterPreVerb (List.drop 1 l)

            else
                ItemWord w kind :: managePreVerb w kind afterPreVerb l

        Nothing ->
            ItemWord w kind :: []



{-
   prevVerb + (word\verbe) => preVerb + verbe
   prevVerb + (preVerb) => preVerb + verbe
   prevVerb + (preposition) = verb + preposition
-}


managePreVerb w kind afterPreVerb words =
    if supportPreVerb w then
        case List.head words of
            Just rw ->
                case ( supportPreVerb rw, wordSupportPreposition rw ) of
                    ( True, _ ) ->
                        ItemWord rw VERB_KIND :: adverbsOrNominalGroup (afterPreVerb || kind == VERB_TRANSITIVE_KIND) (List.drop 1 words)

                    ( False, True ) ->
                        ItemWord rw PREPOSITION_KIND :: adverbsOrNominalGroup True (List.drop 1 words)

                    ( False, False ) ->
                        ItemWord rw VERB_KIND :: adverbsOrNominalGroup (afterPreVerb || kind == VERB_TRANSITIVE_KIND) (List.drop 1 words)

            _ ->
                []

    else
        adverbsOrNominalGroup (wordSupportPreposition w) words


supportPreVerb w =
    case w of
        WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num prep ->
            case preVerb of
                PRE_VERB _ ->
                    True

                _ ->
                    False

        _ ->
            False


adverbsOrNominalGroup isNominal words =
    let
        ( adverbsOrNominal, positions ) =
            fetchPosition False words
    in
    if isNominal then
        List.append (convertWords2Noun True adverbsOrNominal) positions

    else
        List.append
            (List.map
                (\word ->
                    if getKind word == FOREIGN_KIND then
                        ItemWord word NOUN_KIND

                    else
                        ItemWord word ADVERB_KIND
                )
                adverbsOrNominal
            )
            positions


isAlaQuestionForm w words =
    case words of
        Just l ->
            (List.length l > 1)
                && (case ( List.head l, List.head (List.drop 1 l) ) of
                        ( Just rw1, Just rw2 ) ->
                            w == rw2 && rw1 == word_ala

                        _ ->
                            False
                   )

        Nothing ->
            False


nextWordIsAla words =
    firstPos word_ala words == 1


isForm_preVerb_and_mute_or_lili_or_sewi words =
    (firstPos word_lili words == 1)
        || (firstPos word_mute words == 1)
        || (firstPos word_sewi words == 1)


isForm_wile_or_kama_with_preposition_or_wile_kama words w =
    ((w == word_wile)
        || (w == word_kama)
    )
        && (firstSupportPreposition words
                && (List.length words > 1)
           )
        || (firstPos word_kama words == 1)
        && (w == word_wile)


isForm_lukin_with_preposition words w =
    (w == word_lukin)
        && (firstSupportPreposition words
                && (List.length words > 1)
           )


isForm_preVerb_kepeken words w =
    supportPreVerb w
        && (List.length words > 1)
        && (case List.head words of
                Just wn ->
                    wn == word_kepeken

                _ ->
                    False
           )


findVerbs : List WORD -> Bool -> List Item
findVerbs words noSubject =
    let
        ( verb_li, others_li, idx_li ) =
            cutList (\w -> w == word_li) False False words
    in
    if not (List.isEmpty verb_li) then
        List.append (findVerb verb_li noSubject) (And_li :: findVerbs others_li noSubject)

    else
        findVerb others_li noSubject


findVerb : List WORD -> Bool -> List Item
findVerb words noSubject =
    let
        ( verb_e, others_e, idx_e ) =
            cutList (\w -> w == word_e) False False words
    in
    if not (List.isEmpty verb_e) then
        List.append (convertWords2Verb (not noSubject) True verb_e) (findCODs others_e)
        {-
           else if firstSupportPreposition words then
               case List.head words of
                   Just w ->
                       if isAlaQuestionForm w (List.tail words) then
                           ItemWord w VERB_KIND
                               :: ItemWord word_ala ADVERB_KIND
                               :: ItemWord word_anu GRAMMAR_KIND
                               :: ItemWord w VERB_KIND
                               :: convertWords2Verb False True False True False (List.drop 3 words)

                       else if nextWordIsAla (List.drop 1 words) then
                           ItemWord w VERB_KIND
                               :: ItemWord word_ala ADVERB_KIND
                               :: convertWords2Verb False True False True False (List.drop 2 words)

                       else
                           convert2Position words VERB_KIND

                   _ ->
                       []
        -}

    else
        convertWords2Verb (not noSubject) False words


fetchPosition : Bool -> List WORD -> ( List WORD, List Item )
fetchPosition firstCall words =
    let
        ( noun, position, idx ) =
            cutList wordSupportPreposition True True words
    in
    case ( List.length position > 1, List.isEmpty noun, idx > 0 ) of
        ( True, False, True ) ->
            ( noun, convert2Position position PREPOSITION_KIND )

        ( True, True, True ) ->
            if firstCall then
                ( [], convert2Position position NOUN_KIND )

            else
                ( [], convert2Position position PREPOSITION_KIND )

        ( _, _, _ ) ->
            ( words, [] )


convert2Position : List WORD -> WORD_KIND -> List Item
convert2Position words kind =
    let
        verbPreposition =
            List.head words

        position =
            List.tail words
    in
    case verbPreposition of
        Just w ->
            case position of
                Just l ->
                    let
                        ( noun, position2 ) =
                            fetchPosition False l
                    in
                    ItemWord w kind :: List.append (convertWords2Noun True noun) position2

                Nothing ->
                    ItemWord w kind :: []

        Nothing ->
            []


firstSupportPreposition : List WORD -> Bool
firstSupportPreposition words =
    case List.head words of
        Nothing ->
            False

        Just w ->
            wordSupportPreposition w


wordSupportPreposition : WORD -> Bool
wordSupportPreposition word =
    case word of
        WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num prep ->
            prep /= NO_PREP

        _ ->
            False


findCODs : List WORD -> List Item
findCODs words =
    let
        ( cod_e, others_e, idx_e ) =
            cutList (\w -> w == word_e) False False words
    in
    if not (List.isEmpty cod_e) then
        List.append (convertWords2Noun True cod_e) (And_li :: findCODs others_e)

    else
        let
            ( noun, positions ) =
                fetchPosition True words
        in
        case ( List.length positions > 1, List.isEmpty noun ) of
            ( True, False ) ->
                List.append (convertWords2Noun True noun) positions

            ( True, True ) ->
                positions

            ( _, _ ) ->
                convertWords2Noun True noun
