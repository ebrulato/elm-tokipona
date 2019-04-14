module Example exposing (suite)

import Expect exposing (Expectation, equal)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tokipona exposing (translate, translatePona)


suite : Test
suite =
    describe "Tokipona test suite"
        [ describe "limit"
            [ test "toto" <| \_ -> translate "toto" |> equal "[ERROR: 'toto' is not a valid word]"
            , test "support spaces" <| \_ -> translate "  mi " |> equal "I; me, we, us"
            , test "empty phrase" <| \_ -> translate "" |> equal ""
            ]
        , describe
            "lesson #3 http://tokipona.net/tp/janpije/okamasona3.php"
            [ test "jan" <| \_ -> translate "jan" |> equal "people; human being, person, somebody"
            , test "mi" <| \_ -> translate "mi" |> equal "I; me, we, us"
            , test "moku" <| \_ -> translate "moku" |> equal "to ingest; to eat, drink, consume, swallow"
            , test "sina" <| \_ -> translate "sina" |> equal "you"
            , test "suli" <| \_ -> translate "suli" |> equal "big; heavy, large, long, tall; important; adult"
            , test "suno" <| \_ -> translate "suno" |> equal "sun; light, brightness, glow, radiance, shine; light source"
            , test "telo" <| \_ -> translate "telo" |> equal "water; liquid, fluid, wet substance; beverage"
            , test "pona" <| \_ -> translate "pona" |> equal "good; positive, useful; friendly, peaceful; simple"
            , test "li" <| \_ -> translate "li" |> equal "separates some subjects (especially third-person) from the verb"
            , test "mi pona" <| \_ -> translate "mi pona" |> equal "I; me, we, us / ('good; positive, useful; friendly, peaceful; simple' as a verb or being...)"
            , test "sina suli" <| \_ -> translate "sina suli" |> equal "you / ('big; heavy, large, long, tall; important; adult' as a verb or being...)"
            , test "mi moku" <| \_ -> translate "mi moku" |> equal "I; me, we, us / to ingest; to eat, drink, consume, swallow"
            , test "sina pona" <| \_ -> translate "sina pona" |> equal "you / ('good; positive, useful; friendly, peaceful; simple' as a verb or being...)"
            , test "telo li pona" <| \_ -> translate "telo li pona" |> equal "water; liquid, fluid, wet substance; beverage / ('good; positive, useful; friendly, peaceful; simple' as a verb or being...)"
            , test "suno li suli" <| \_ -> translate "suno li suli" |> equal "sun; light, brightness, glow, radiance, shine; light source / ('big; heavy, large, long, tall; important; adult' as a verb or being...)"
            , test "moku li pona" <| \_ -> translate "moku li pona" |> equal "food / ('good; positive, useful; friendly, peaceful; simple' as a verb or being...)"
            ]
        , describe
            "lesson #3 Simple http://tokipona.net/tp/janpije/okamasona3.php"
            [ test "jan" <| \_ -> translatePona "jan" |> equal "people"
            , test "mi" <| \_ -> translatePona "mi" |> equal "I"
            , test "moku" <| \_ -> translatePona "moku" |> equal "to ingest"
            , test "sina" <| \_ -> translatePona "sina" |> equal "you"
            , test "suli" <| \_ -> translatePona "suli" |> equal "big"
            , test "suno" <| \_ -> translatePona "suno" |> equal "sun"
            , test "telo" <| \_ -> translatePona "telo" |> equal "water"
            , test "pona" <| \_ -> translatePona "pona" |> equal "good"
            , test "li" <| \_ -> translatePona "li" |> equal "separates some subjects (especially third-person) from the verb"
            , test "mi pona" <| \_ -> translatePona "mi pona" |> equal "I / ('good' as a verb or being...)"
            , test "sina suli" <| \_ -> translatePona "sina suli" |> equal "you / ('big' as a verb or being...)"
            , test "mi moku" <| \_ -> translatePona "mi moku" |> equal "I / to ingest"
            , test "sina pona" <| \_ -> translatePona "sina pona" |> equal "you / ('good' as a verb or being...)"
            , test "telo li pona" <| \_ -> translatePona "telo li pona" |> equal "water / ('good' as a verb or being...)"
            , test "suno li suli" <| \_ -> translatePona "suno li suli" |> equal "sun / ('big' as a verb or being...)"
            , test "moku li pona" <| \_ -> translatePona "moku li pona" |> equal "food / ('good' as a verb or being...)"
            , test "jan li pona" <| \_ -> translatePona "jan li pona" |> equal "people / ('good' as a verb or being...)"
            , test "mi suli" <| \_ -> translatePona "mi suli" |> equal "I / ('big' as a verb or being...)"
            , test "telo li suli" <| \_ -> translatePona "telo li suli" |> equal "water / ('big' as a verb or being...)"
            , test "jan li moku" <| \_ -> translatePona "jan li moku" |> equal "people / to ingest"
            ]
        ]
