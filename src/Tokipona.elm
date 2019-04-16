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
    case String.length phrases of
        0 ->
            ""

        _ ->
            if String.contains "." phrases then
                case cutList (\v -> v == '.') False (String.toList phrases) of
                    ( firstPhrase, otherPhrases, _ ) ->
                        String.trim <| internalTranslate (String.fromList firstPhrase) pona nimi ++ ". " ++ internalTranslate (String.fromList otherPhrases) pona nimi

            else
                String.words phrases
                    |> reduce2WithLipu nimi
                    |> List.map getRawTokipona
                    --|> Just
                    |> translateTravel
                    |> List.map (itemToString pona)
                    |> String.join " / "


reduce2WithLipu : Bool -> List String -> List String
reduce2WithLipu nimi words =
    if nimi then
        let
            w =
                List.head words
        in
        case w of
            Just v ->
                reduce2 [] v (List.drop 1 words)

            Nothing ->
                words

    else
        words


reduce2 : List String -> String -> List String -> List String
reduce2 listHead lastWord listTail =
    let
        w =
            List.head listTail
    in
    case w of
        Just v ->
            let
                tmp =
                    lastWord ++ " " ++ v

                nextT =
                    List.drop 1 listTail

                nextW =
                    List.head nextT
            in
            if Dict.member tmp tokiponaLipu then
                case nextW of
                    Just nextWv ->
                        reduce2 (List.append listHead (tmp :: [])) nextWv (List.drop 1 nextT)

                    Nothing ->
                        List.append listHead (tmp :: [])

            else
                reduce2 (List.append listHead (lastWord :: [])) v nextT

        Nothing ->
            List.append listHead (lastWord :: [])


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
                        ItemWord w NOUN_KIND :: convertWords2Noun False l

                    Nothing ->
                        ItemWord w NOUN_KIND :: []

            else
                List.map (\word -> ItemWord word ADJECTIVE_KIND) words

        Nothing ->
            []


findSubject : List WORD -> List Item
findSubject words =
    let
        subject =
            [ cutList (\w -> w == word_mi) True words
            , cutList (\w -> w == word_sina) True words
            , cutList (\w -> w == word_li) False words
            ]
                |> List.filter (\( _, _, idx ) -> idx > 0)
                |> List.sortBy (\( _, _, idx ) -> idx)
                |> List.head
    in
    case subject of
        Just ( s, o, idx ) ->
            if is_mi_mute_or_lili_li_form words then
                case cutList (\w -> w == word_li) False words of
                    ( s2, o2, idx2 ) ->
                        List.append (convertWords2Noun True s2) (findVerbs o2)

            else
                List.append (convertWords2Noun True s) (findVerbs o)

        Nothing ->
            convertWords2Noun True words



--is_mi_mute_or_lili_li_form : List WORD -> Bool


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


convertWords2Verb : Bool -> Bool -> Bool -> List WORD -> List Item
convertWords2Verb firstCall mainVerbFounded transitive words =
    case List.head words of
        Just w ->
            if firstCall then
                case w of
                    WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num ->
                        case preVerb of
                            PRE_VERB s ->
                                let
                                    nexts =
                                        List.drop 1 words
                                in
                                if isForm_preVerb_mute_or_lili_or_sewi nexts || not (List.length nexts > 0) then
                                    convertWords2Verb False False transitive words

                                else
                                    ItemWord w PRE_VERB_KIND :: convertWords2Verb False False transitive nexts

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


isForm_preVerb_mute_or_lili_or_sewi words =
    (firstPos word_lili words == 1)
        || (firstPos word_mute words == 1)
        || (firstPos word_sewi words == 1)


findVerbs : List WORD -> List Item
findVerbs words =
    let
        ( verb_li, others_li, idx_li ) =
            cutList (\w -> w == word_li) False words
    in
    if not (List.isEmpty verb_li) then
        List.append (findVerb verb_li) (And_li :: findVerbs others_li)

    else
        findVerb others_li


findVerb : List WORD -> List Item
findVerb words =
    let
        ( verb_e, others_e, idx_e ) =
            cutList (\w -> w == word_e) False words
    in
    if not (List.isEmpty verb_e) then
        List.append (convertWords2Verb True False True verb_e) (findCODs others_e)

    else
        convertWords2Verb True False False others_e


findCODs : List WORD -> List Item
findCODs words =
    let
        ( cod_e, others_e, idx_e ) =
            cutList (\w -> w == word_e) False words
    in
    if not (List.isEmpty cod_e) then
        List.append (convertWords2Noun True cod_e) (And_li :: findCODs others_e)

    else
        convertWords2Noun True words
