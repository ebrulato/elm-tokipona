module Nimi exposing (nimiPona)

import Char exposing (isLower, isUpper)
import Dict exposing (Dict, fromList, get, member)
import String exposing (contains, toList, toLower, uncons)


nimiPona : String -> Bool
nimiPona word =
    let
        lword =
            toLower word
    in
    case isFirstCapitalized word of
        ( True, others ) ->
            areOthersLowerCase others
                && noForbiddenSyllabes lword
                && (validStructure 1 False False <| toList <| lword)

        ( False, _ ) ->
            False



--  ji, ti, wo, and wu are forbidden


noForbiddenSyllabes w =
    not
        (contains "ji" w
            || contains "ti" w
            || contains "wo" w
            || contains "wu" w
        )



-- see http://tokipona.net/tp/janpije/okamasona2.php
-- Toki Pona's syllables all follow a pattern: consonant + vowel + optional n.
-- If a syllable is at the beginning of a word, the consonant is optional.


validStructure : Int -> Bool -> Bool -> List Char -> Bool
validStructure pos previousIsN previousIsVoyell lletters =
    case lletters of
        [] ->
            if (pos == 2) && not previousIsVoyell then
                -- c KO
                False

            else
                True

        letter :: letters ->
            case ( isVowel letter, pos ) of
                ( True, 1 ) ->
                    -- v
                    validStructure 2 False True letters

                ( False, 1 ) ->
                    if previousIsN && (isN letter || isM letter) then
                        -- The optional n is forbidden if the next syllable begins with m or n.
                        False

                    else
                        -- c
                        validStructure 2 (isN letter) False letters

                ( True, 2 ) ->
                    if previousIsVoyell then
                        -- vv KO
                        False

                    else
                        -- cv
                        validStructure 3 False True letters

                ( False, 2 ) ->
                    if previousIsVoyell then
                        -- vc OK
                        validStructure 1 (isN letter) False letters

                    else
                        -- cc KO
                        False

                ( True, 3 ) ->
                    -- cvv
                    False

                ( False, 3 ) ->
                    -- cvc
                    validStructure 1 (isN letter) False letters

                ( _, _ ) ->
                    False


isFirstCapitalized : String -> ( Bool, String )
isFirstCapitalized word =
    case uncons word of
        Just ( c, others ) ->
            ( isUpper c, others )

        Nothing ->
            ( False, "" )


areOthersLowerCase : String -> Bool
areOthersLowerCase word =
    case uncons word of
        Nothing ->
            True

        Just ( c, others ) ->
            if not (isLower c) then
                False

            else
                areOthersLowerCase others


isValidChar c =
    member c tokiponaChar


isConsonant c =
    case get c tokiponaChar of
        Nothing ->
            False

        Just v ->
            v


isN c =
    c == 'n'


isM c =
    c == 'm'


isVowel c =
    case get c tokiponaChar of
        Nothing ->
            False

        Just v ->
            not v


tokiponaChar : Dict Char Bool
tokiponaChar =
    fromList
        [ ( 'j', True )
        , ( 'k', True )
        , ( 'l', True )
        , ( 'm', True )
        , ( 'n', True )
        , ( 'p', True )
        , ( 's', True )
        , ( 't', True )
        , ( 'w', True )
        , ( 'a', False )
        , ( 'e', False )
        , ( 'i', False )
        , ( 'o', False )
        , ( 'u', False )
        ]
