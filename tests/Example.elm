module Example exposing (suite)

import Expect exposing (Expectation, equal)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tokipona exposing (translate, translateNimi, translateNimiPona, translatePona)


suite : Test
suite =
    describe "Tokipona test suite"
        [ describe "limit"
            [ test "toto" <| \_ -> translate "toto" |> equal "[ERROR: 'toto' is not a valid word]"
            , test "support spaces" <| \_ -> translate "  mi " |> equal "I; me, we, us"
            , test "support spaces simple" <| \_ -> translatePona "  mi " |> equal "I"
            , test "empty phrase" <| \_ -> translate "" |> equal ""
            , test "empty phrase simple" <| \_ -> translatePona "" |> equal ""
            ]
        , describe
            "lesson #3 http://tokipona.net/tp/janpije/okamasona3.php"
            [ test "jan" <| \_ -> translate "jan" |> equal "people; human, being, person, somebody, anybody"
            , test "mi" <| \_ -> translate "mi" |> equal "I; me, we, us"
            , test "moku" <| \_ -> translate "moku" |> equal "to ingest; to eat, to drink, to consume, to swallow"
            , test "sina" <| \_ -> translate "sina" |> equal "you"
            , test "suli" <| \_ -> translate "suli" |> equal "big; heavy, large, long, tall; important; adult"
            , test "suno" <| \_ -> translate "suno" |> equal "sun; light, brightness, glow, radiance, shine; light source"
            , test "telo" <| \_ -> translate "telo" |> equal "water; liquid, fluid, wet substance; beverage; juice, sauce"
            , test "pona" <| \_ -> translate "pona" |> equal "good; simple, positive, nice, correct, right, useful; friendly, peaceful"
            , test "li" <| \_ -> translate "li" |> equal "separates some subjects (especially third-person) from the verb"
            , test "mi pona" <| \_ -> translate "mi pona" |> equal "I; me, we, us / to be good; to be simple, positive, nice, correct, right, useful; to be friendly, peaceful"
            , test "sina suli" <| \_ -> translate "sina suli" |> equal "you / ('big; heavy, large, long, tall; important; adult' as a verb or being...)"
            , test "mi moku" <| \_ -> translate "mi moku" |> equal "I; me, we, us / to ingest; to eat, to drink, to consume, to swallow"
            , test "sina pona" <| \_ -> translate "sina pona" |> equal "you / to be good; to be simple, positive, nice, correct, right, useful; to be friendly, peaceful"
            , test "telo li pona" <| \_ -> translate "telo li pona" |> equal "water; liquid, fluid, wet substance; beverage; juice, sauce / to be good; to be simple, positive, nice, correct, right, useful; to be friendly, peaceful"
            , test "suno li suli" <| \_ -> translate "suno li suli" |> equal "sun; light, brightness, glow, radiance, shine; light source / ('big; heavy, large, long, tall; important; adult' as a verb or being...)"
            , test "moku li pona" <| \_ -> translate "moku li pona" |> equal "food; meal / to be good; to be simple, positive, nice, correct, right, useful; to be friendly, peaceful"
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
            , test "mi pona" <| \_ -> translatePona "mi pona" |> equal "I / to be good"
            , test "sina suli" <| \_ -> translatePona "sina suli" |> equal "you / ('big' as a verb or being...)"
            , test "mi moku" <| \_ -> translatePona "mi moku" |> equal "I / to ingest"
            , test "sina pona" <| \_ -> translatePona "sina pona" |> equal "you / to be good"
            , test "telo li pona" <| \_ -> translatePona "telo li pona" |> equal "water / to be good"
            , test "suno li suli" <| \_ -> translatePona "suno li suli" |> equal "sun / ('big' as a verb or being...)"
            , test "moku li pona" <| \_ -> translatePona "moku li pona" |> equal "food / to be good"
            , test "jan li pona" <| \_ -> translatePona "jan li pona" |> equal "people / to be good"
            , test "mi suli" <| \_ -> translatePona "mi suli" |> equal "I / ('big' as a verb or being...)"
            , test "telo li suli" <| \_ -> translatePona "telo li suli" |> equal "water / ('big' as a verb or being...)"
            , test "jan li moku" <| \_ -> translatePona "jan li moku" |> equal "people / to ingest"
            ]
        , describe
            "lesson #4 http://tokipona.net/tp/janpije/okamasona4.php"
            [ test "ilo" <| \_ -> translate "ilo" |> equal "tool; implement, machine, device; thing used for a specific purpose"
            , test "kili" <| \_ -> translate "kili" |> equal "fruit; vegetable, mushroom"
            , test "ni" <| \_ -> translate "ni" |> equal "this, that"
            , test "ona" <| \_ -> translate "ona" |> equal "he, she, it"
            , test "pipi" <| \_ -> translate "pipi" |> equal "bug; insect, ant, spider"
            , test "ma" <| \_ -> translate "ma" |> equal "area; earth, land; outdoor area; world; region, country, territory; soil"
            , test "ijo" <| \_ -> translate "ijo" |> equal "something; anything, stuff, thing, object"
            , test "jo" <| \_ -> translate "jo" |> equal "to have; to carry, contain, hold"
            , test "lukin" <| \_ -> translate "lukin" |> equal "to look; to pay attention, to examine, to observe, to read, to watch"
            , test "oko" <| \_ -> translate "oko" |> equal "eye"
            , test "pakala" <| \_ -> translate "pakala" |> equal "to screw up; to fall apart, to break"
            , test "unpa" <| \_ -> translate "unpa" |> equal "to have sex"
            , test "wile" <| \_ -> translate "wile" |> equal "to want; need, wish, have to, must, will, should"
            , test "e" <| \_ -> translate "e" |> equal "introduces direct object"
            , test "mi moku e kili" <| \_ -> translate "mi moku e kili" |> equal "I; me, we, us / to ingest; to eat, to drink, to consume, to swallow / fruit; vegetable, mushroom"
            , test "ona li lukin e pipi" <| \_ -> translate "ona li lukin e pipi" |> equal "he, she, it / to see; to look at, to watch, to read / bug; insect, ant, spider"
            , test "ona li pona e ilo" <| \_ -> translate "ona li pona e ilo" |> equal "he, she, it / to fix; to improve, to repair, to make good / tool; implement, machine, device; thing used for a specific purpose"
            , test "mi pona e ijo" <| \_ -> translate "mi pona e ijo" |> equal "I; me, we, us / to fix; to improve, to repair, to make good / something; anything, stuff, thing, object"
            , test "mi wile lukin e ma" <| \_ -> translate "mi wile lukin e ma" |> equal "I; me, we, us / want; need, wish, have to, must, will, should / to see; to look at, to watch, to read / area; earth, land; outdoor area; world; region, country, territory; soil"
            , test "mi wile pakala e sina" <| \_ -> translate "mi wile pakala e sina" |> equal "I; me, we, us / want; need, wish, have to, must, will, should / to screw up; to ruin, to break, to hurt, to injure, to damage, to destroy / you"
            , test "pipi li lukin li unpa" <| \_ -> translate "pipi li lukin li unpa" |> equal "bug; insect, ant, spider / to look; to pay attention, to examine, to observe, to read, to watch / and / to have sex"
            , test "pipi li lukin e mi li unpa e sina" <| \_ -> translate "pipi li lukin e mi li unpa e sina" |> equal "bug; insect, ant, spider / to see; to look at, to watch, to read / I; me, we, us / and / to have sex with; to sleep with, to fuck / you"
            , test "pipi li wile lukin e mi li wile unpa e sina" <| \_ -> translate "pipi li wile lukin e mi li wile unpa e sina" |> equal "bug; insect, ant, spider / want; need, wish, have to, must, will, should / to see; to look at, to watch, to read / I; me, we, us / and / want; need, wish, have to, must, will, should / to have sex with; to sleep with, to fuck / you"
            , test "mi moku li pakala" <| \_ -> translate "mi moku li pakala" |> equal "I; me, we, us / to ingest; to eat, to drink, to consume, to swallow / and / to screw up; to fall apart, to break"
            , test "pipi li wile lukin e mi e sina li wile unpa e sina e mi" <| \_ -> translate "pipi li wile lukin e mi e sina li wile unpa e sina e mi" |> equal "bug; insect, ant, spider / want; need, wish, have to, must, will, should / to see; to look at, to watch, to read / I; me, we, us / and / you / and / want; need, wish, have to, must, will, should / to have sex with; to sleep with, to fuck / you / and / I; me, we, us"
            ]
        , describe
            "lesson #4 Simple http://tokipona.net/tp/janpije/okamasona4.php"
            [ test "ilo" <| \_ -> translatePona "ilo" |> equal "tool"
            , test "kili" <| \_ -> translatePona "kili" |> equal "fruit"
            , test "ni" <| \_ -> translatePona "ni" |> equal "this, that"
            , test "ona" <| \_ -> translatePona "ona" |> equal "he, she, it"
            , test "pipi" <| \_ -> translatePona "pipi" |> equal "bug"
            , test "ma" <| \_ -> translatePona "ma" |> equal "area"
            , test "ijo" <| \_ -> translatePona "ijo" |> equal "something"
            , test "jo" <| \_ -> translatePona "jo" |> equal "to have"
            , test "lukin" <| \_ -> translatePona "lukin" |> equal "to look"
            , test "oko" <| \_ -> translatePona "oko" |> equal "eye"
            , test "pakala" <| \_ -> translatePona "pakala" |> equal "to screw up"
            , test "unpa" <| \_ -> translatePona "unpa" |> equal "to have sex"
            , test "wile" <| \_ -> translatePona "wile" |> equal "to want"
            , test "e" <| \_ -> translatePona "e" |> equal "introduces direct object"
            , test "mi moku e kili" <| \_ -> translatePona "mi moku e kili" |> equal "I / to ingest / fruit"
            , test "ona li lukin e pipi" <| \_ -> translatePona "ona li lukin e pipi" |> equal "he, she, it / to see / bug"
            , test "ona li pona e ilo" <| \_ -> translatePona "ona li pona e ilo" |> equal "he, she, it / to fix / tool"
            , test "mi pona e ijo" <| \_ -> translatePona "mi pona e ijo" |> equal "I / to fix / something"
            , test "mi wile lukin e ma" <| \_ -> translatePona "mi wile lukin e ma" |> equal "I / want / to see / area"
            , test "mi wile pakala e sina" <| \_ -> translatePona "mi wile pakala e sina" |> equal "I / want / to screw up / you"
            , test "pipi li lukin li unpa" <| \_ -> translatePona "pipi li lukin li unpa" |> equal "bug / to look / and / to have sex"
            , test "pipi li lukin e mi li unpa e sina" <| \_ -> translatePona "pipi li lukin e mi li unpa e sina" |> equal "bug / to see / I / and / to have sex with / you"
            , test "pipi li wile lukin e mi li wile unpa e sina" <| \_ -> translatePona "pipi li wile lukin e mi li wile unpa e sina" |> equal "bug / want / to see / I / and / want / to have sex with / you"
            , test "mi moku li pakala" <| \_ -> translatePona "mi moku li pakala" |> equal "I / to ingest / and / to screw up"
            , test "mi moku e kili e telo" <| \_ -> translatePona "mi moku e kili e telo" |> equal "I / to ingest / fruit / and / water"
            , test "pipi li wile lukin e mi e sina li wile unpa e sina e mi" <| \_ -> translatePona "pipi li wile lukin e mi e sina li wile unpa e sina e mi" |> equal "bug / want / to see / I / and / you / and / want / to have sex with / you / and / I"
            , test "mi lukin e ni" <| \_ -> translatePona "mi lukin e ni" |> equal "I / to see / this, that"
            , test "mi wile unpa e ona" <| \_ -> translatePona "mi wile unpa e ona" |> equal "I / want / to have sex with / he, she, it"
            , test "mi jan li suli" <| \_ -> translatePona "mi jan li suli" |> equal "I / ('people' as a verb or being...) / and / ('big' as a verb or being...)"
            ]
        , describe
            "lesson #5 http://tokipona.net/tp/janpije/okamasona5.php"
            [ test "ike" <| \_ -> translatePona "ike" |> equal "bad"
            , test "jaki" <| \_ -> translatePona "jaki" |> equal "dirty"
            , test "lawa" <| \_ -> translatePona "lawa" |> equal "main"
            , test "len" <| \_ -> translatePona "len" |> equal "clothing"
            , test "lili" <| \_ -> translatePona "lili" |> equal "small"
            , test "mute" <| \_ -> translatePona "mute" |> equal "many"
            , test "nasa" <| \_ -> translatePona "nasa" |> equal "crazy"
            , test "seli" <| \_ -> translatePona "seli" |> equal "fire"
            , test "sewi" <| \_ -> translatePona "sewi" |> equal "elevated"
            , test "tomo" <| \_ -> translatePona "tomo" |> equal "house"
            , test "utala" <| \_ -> translatePona "utala" |> equal "conflict"
            , test "jan pona" <| \_ -> translatePona "jan pona" |> equal "people / good"
            , test "jan pona (nimi)" <| \_ -> translateNimiPona "jan pona" |> equal "friend"
            , test "jan pakala" <| \_ -> translatePona "jan pakala" |> equal "people / destroyed"
            , test "ilo moku" <| \_ -> translatePona "ilo moku" |> equal "tool / eating"
            , test "ni li ilo moku" <| \_ -> translatePona "ni li ilo moku" |> equal "this, that / ('tool' as a verb or being...) / eating"
            , test "jan utala (nimi)" <| \_ -> translateNimiPona "jan utala" |> equal "soldier"
            , test "jan utala pona (nimi)" <| \_ -> translateNimiPona "jan utala pona" |> equal "soldier / good"
            , test "jan utala pona mute (nimi)" <| \_ -> translateNimiPona "jan utala pona mute" |> equal "soldier / good / many"
            , test "jan utala pona ni (nimi)" <| \_ -> translateNimiPona "jan utala pona ni" |> equal "soldier / good / this, that"
            , test "jan pona utala (nimi)" <| \_ -> translateNimiPona "jan pona utala" |> equal "friend / fighting"
            , test "jan pona li pona lukin (nimi)" <| \_ -> translateNimiPona "jan pona li pona lukin" |> equal "friend / to be beautiful"
            , test "mi ike lukin (nimi)" <| \_ -> translateNimiPona "mi ike lukin" |> equal "I / to be ugly"
            , test "mi ike lukin " <| \_ -> translateNimi "mi ike lukin" |> equal "I; me, we, us / to be ugly"
            , test "mi ike lukin (bis)" <| \_ -> translatePona "mi ike lukin" |> equal "I / to be bad / visual(ly)"
            , test "jan ike (nimi)" <| \_ -> translateNimiPona "jan ike" |> equal "enemy"
            , test "jan ike lukin (nimi)" <| \_ -> translateNimiPona "jan ike lukin" |> equal "enemy / visual(ly)"
            , test "jan li ike lukin (nimi)" <| \_ -> translateNimiPona "jan li ike lukin" |> equal "people / to be ugly"
            , test "jan lawa (nimi)" <| \_ -> translateNimiPona "jan lawa" |> equal "leader"
            , test "lawa (nimi)" <| \_ -> translateNimiPona "lawa" |> equal "main"
            , test "lawa mi li pona (nimi)" <| \_ -> translateNimiPona "lawa mi li pona" |> equal "head / my / to be good"
            , test "jan lili (nimi)" <| \_ -> translateNimiPona "jan lili" |> equal "child"
            , test "jan sewi (nimi)" <| \_ -> translateNimiPona "jan sewi" |> equal "clergyman"
            , test "jan suli (nimi)" <| \_ -> translateNimiPona "jan suli" |> equal "adult"
            , test "jan unpa (nimi)" <| \_ -> translateNimiPona "jan unpa" |> equal "lover"
            , test "ma telo (nimi)" <| \_ -> translateNimiPona "ma telo" |> equal "lake"
            , test "ma tomo (nimi)" <| \_ -> translateNimiPona "ma tomo" |> equal "town"
            , test "mi mute (nimi)" <| \_ -> translateNimiPona "mi mute" |> equal "we"
            , test "ona mute (nimi)" <| \_ -> translateNimiPona "ona mute" |> equal "they"
            , test "telo nasa (nimi)" <| \_ -> translateNimiPona "telo nasa" |> equal "alcohol"
            , test "tomo telo (nimi)" <| \_ -> translateNimiPona "tomo telo" |> equal "restroom"
            , test "len jan (nimi)" <| \_ -> translateNimiPona "len jan" |> equal "clothing / somebody’s"
            , test "tomo mi (nimi)" <| \_ -> translateNimiPona "tomo mi" |> equal "house / my"
            , test "ma sina (nimi)" <| \_ -> translateNimiPona "ma sina" |> equal "area / your"
            , test "telo ona (nimi)" <| \_ -> translateNimiPona "telo ona" |> equal "water / his, her, its"

            -- Note "sun's fire" is not supported because suno acts as an adjective. use: "suno li jo e seli ni"
            , test "seli suno (nimi)" <| \_ -> translateNimiPona "seli suno" |> equal "fire / sunny"
            , test "suno li jo e seli ni (nimi)" <| \_ -> translateNimiPona "suno li jo e seli ni" |> equal "sun / to have / fire / this, that"
            , test "mi lawa pona e jan (nimi)" <| \_ -> translateNimiPona "mi lawa pona e jan" |> equal "I / to lead / well / people"
            , test "mi utala ike (nimi)" <| \_ -> translateNimiPona "mi utala ike" |> equal "I / to hit / badly"

            -- ambiguous : lukin or wile can act as a pre verb, but here we want sewi as an adverb... I added a special rule for lili, mute et sewi
            , test "sina lukin sewi e suno (nimi)" <| \_ -> translateNimiPona "sina lukin sewi e suno" |> equal "you / to see / up / sun"
            , test "ona li wile mute e ni (nimi)" <| \_ -> translateNimiPona "ona li wile mute e ni" |> equal "he, she, it / to want / many / this, that"
            , test "mi mute li lukin lili e ona (nimi)" <| \_ -> translateNimiPona "mi mute li lukin lili e ona" |> equal "we / to see / barely / he, she, it"
            , test "mi mute li lukin lili e ona (pona only)" <| \_ -> translatePona "mi mute li lukin lili e ona" |> equal "I / many / to see / barely / he, she, it"
            , test "mi jo e kili. ona li pona li lili. mi moku lili e kili lili." <| \_ -> translatePona "mi jo e kili. ona li pona li lili. mi moku lili e kili lili." |> equal "I / to have / fruit. he, she, it / to be good / and / to be small. I / to ingest / barely / fruit / small."
            , test "mi jo e kili. ona li pona li lili. mi moku lili e kili lili. (nimi)" <| \_ -> translateNimiPona "mi jo e kili. ona li pona li lili. mi moku lili e kili lili." |> equal "I / to have / fruit. he, she, it / to be good / and / to be small. I / to nibble / fruit / small."
            , test "mi lukin sewi e tomo suli." <| \_ -> translateNimiPona "mi lukin sewi e tomo suli." |> equal "I / to see / up / house / big."
            , test "seli suno li seli e tomo mi." <| \_ -> translateNimiPona "seli suno li seli e tomo mi." |> equal "fire / sunny / to heat / house / my."
            , test "jan lili li wile e telo kili." <| \_ -> translateNimiPona "jan lili li wile e telo kili." |> equal "child / to want / fruit juice."
            , test "ona mute li nasa e jan suli." <| \_ -> translateNimiPona "ona mute li nasa e jan suli." |> equal "they / to drive crazy / adult."
            ]
        , describe
            "lesson #6 http://tokipona.net/tp/janpije/okamasona6.php"
            [ test "ike" <| \_ -> translatePona "ike" |> equal "bad"
            , test "jaki" <| \_ -> translatePona "jaki" |> equal "dirty"
            ]
        ]
