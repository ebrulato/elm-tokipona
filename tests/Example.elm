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
            , test "pipi" <| \_ -> translate "pipi" |> equal "insect; bug, ant, spider"
            , test "ma" <| \_ -> translate "ma" |> equal "land; earth, area; outdoor area; world; region, country, territory; soil"
            , test "ijo" <| \_ -> translate "ijo" |> equal "something; anything, stuff, thing, object"
            , test "jo" <| \_ -> translate "jo" |> equal "to have; to carry, contain, hold"
            , test "lukin" <| \_ -> translate "lukin" |> equal "to look; to pay attention, to examine, to observe, to read, to watch"
            , test "oko" <| \_ -> translate "oko" |> equal "eye"
            , test "pakala" <| \_ -> translate "pakala" |> equal "to screw up; to fall apart, to break"
            , test "unpa" <| \_ -> translate "unpa" |> equal "to have sex"
            , test "wile" <| \_ -> translate "wile" |> equal "to want; need, wish, have to, must, will, should"
            , test "e" <| \_ -> translate "e" |> equal "introduces direct object"
            , test "mi moku e kili" <| \_ -> translate "mi moku e kili" |> equal "I; me, we, us / to ingest; to eat, to drink, to consume, to swallow / fruit; vegetable, mushroom"
            , test "ona li lukin e pipi" <| \_ -> translate "ona li lukin e pipi" |> equal "he, she, it / to see; to look at, to watch, to read / insect; bug, ant, spider"
            , test "ona li pona e ilo" <| \_ -> translate "ona li pona e ilo" |> equal "he, she, it / to fix; to improve, to repair, to make good / tool; implement, machine, device; thing used for a specific purpose"
            , test "mi pona e ijo" <| \_ -> translate "mi pona e ijo" |> equal "I; me, we, us / to fix; to improve, to repair, to make good / something; anything, stuff, thing, object"
            , test "mi wile lukin e ma" <| \_ -> translate "mi wile lukin e ma" |> equal "I; me, we, us / want; need, wish, have to, must, will, should / to see; to look at, to watch, to read / land; earth, area; outdoor area; world; region, country, territory; soil"
            , test "mi wile pakala e sina" <| \_ -> translate "mi wile pakala e sina" |> equal "I; me, we, us / want; need, wish, have to, must, will, should / to screw up; to ruin, to break, to hurt, to injure, to damage, to destroy / you"
            , test "pipi li lukin li unpa" <| \_ -> translate "pipi li lukin li unpa" |> equal "insect; bug, ant, spider / to look; to pay attention, to examine, to observe, to read, to watch / and / to have sex"
            , test "pipi li lukin e mi li unpa e sina" <| \_ -> translate "pipi li lukin e mi li unpa e sina" |> equal "insect; bug, ant, spider / to see; to look at, to watch, to read / I; me, we, us / and / to have sex with; to sleep with, to fuck / you"
            , test "pipi li wile lukin e mi li wile unpa e sina" <| \_ -> translate "pipi li wile lukin e mi li wile unpa e sina" |> equal "insect; bug, ant, spider / want; need, wish, have to, must, will, should / to see; to look at, to watch, to read / I; me, we, us / and / want; need, wish, have to, must, will, should / to have sex with; to sleep with, to fuck / you"
            , test "mi moku li pakala" <| \_ -> translate "mi moku li pakala" |> equal "I; me, we, us / to ingest; to eat, to drink, to consume, to swallow / and / to screw up; to fall apart, to break"
            , test "pipi li wile lukin e mi e sina li wile unpa e sina e mi" <| \_ -> translate "pipi li wile lukin e mi e sina li wile unpa e sina e mi" |> equal "insect; bug, ant, spider / want; need, wish, have to, must, will, should / to see; to look at, to watch, to read / I; me, we, us / and / you / and / want; need, wish, have to, must, will, should / to have sex with; to sleep with, to fuck / you / and / I; me, we, us"
            ]
        , describe
            "lesson #4 Simple http://tokipona.net/tp/janpije/okamasona4.php"
            [ test "ilo" <| \_ -> translatePona "ilo" |> equal "tool"
            , test "kili" <| \_ -> translatePona "kili" |> equal "fruit"
            , test "ni" <| \_ -> translatePona "ni" |> equal "this, that"
            , test "ona" <| \_ -> translatePona "ona" |> equal "he, she, it"
            , test "pipi" <| \_ -> translatePona "pipi" |> equal "insect"
            , test "ma" <| \_ -> translatePona "ma" |> equal "land"
            , test "ijo" <| \_ -> translatePona "ijo" |> equal "something"
            , test "jo" <| \_ -> translatePona "jo" |> equal "to have"
            , test "lukin" <| \_ -> translatePona "lukin" |> equal "to look"
            , test "oko" <| \_ -> translatePona "oko" |> equal "eye"
            , test "pakala" <| \_ -> translatePona "pakala" |> equal "to screw up"
            , test "unpa" <| \_ -> translatePona "unpa" |> equal "to have sex"
            , test "wile" <| \_ -> translatePona "wile" |> equal "to want"
            , test "e" <| \_ -> translatePona "e" |> equal "introduces direct object"
            , test "mi moku e kili" <| \_ -> translatePona "mi moku e kili" |> equal "I / to ingest / fruit"
            , test "ona li lukin e pipi" <| \_ -> translatePona "ona li lukin e pipi" |> equal "he, she, it / to see / insect"
            , test "ona li pona e ilo" <| \_ -> translatePona "ona li pona e ilo" |> equal "he, she, it / to fix / tool"
            , test "mi pona e ijo" <| \_ -> translatePona "mi pona e ijo" |> equal "I / to fix / something"
            , test "mi wile lukin e ma" <| \_ -> translatePona "mi wile lukin e ma" |> equal "I / want / to see / land"
            , test "mi wile pakala e sina" <| \_ -> translatePona "mi wile pakala e sina" |> equal "I / want / to screw up / you"
            , test "pipi li lukin li unpa" <| \_ -> translatePona "pipi li lukin li unpa" |> equal "insect / to look / and / to have sex"
            , test "pipi li lukin e mi li unpa e sina" <| \_ -> translatePona "pipi li lukin e mi li unpa e sina" |> equal "insect / to see / I / and / to have sex with / you"
            , test "pipi li wile lukin e mi li wile unpa e sina" <| \_ -> translatePona "pipi li wile lukin e mi li wile unpa e sina" |> equal "insect / want / to see / I / and / want / to have sex with / you"
            , test "mi moku li pakala" <| \_ -> translatePona "mi moku li pakala" |> equal "I / to ingest / and / to screw up"
            , test "mi moku e kili e telo" <| \_ -> translatePona "mi moku e kili e telo" |> equal "I / to ingest / fruit / and / water"
            , test "pipi li wile lukin e mi e sina li wile unpa e sina e mi" <| \_ -> translatePona "pipi li wile lukin e mi e sina li wile unpa e sina e mi" |> equal "insect / want / to see / I / and / you / and / want / to have sex with / you / and / I"
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
            , test "ma sina (nimi)" <| \_ -> translateNimiPona "ma sina" |> equal "land / your"
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
            [ test "kama" <| \_ -> translatePona "kama" |> equal "to come"
            , test "kepeken" <| \_ -> translatePona "kepeken" |> equal "to use"
            , test "kiwen" <| \_ -> translatePona "kiwen" |> equal "stone|rock"
            , test "kon" <| \_ -> translatePona "kon" |> equal "air"
            , test "lon" <| \_ -> translatePona "lon" |> equal "to be in|at|on"
            , test "pana" <| \_ -> translatePona "pana" |> equal "to give"
            , test "poki" <| \_ -> translatePona "poki" |> equal "container"
            , test "toki" <| \_ -> translatePona "toki" |> equal "language"
            , test "tawa" <| \_ -> translatePona "tawa" |> equal "to go to"
            , test "mi lon tomo" <| \_ -> translateNimiPona "mi lon tomo" |> equal "I / to be in|at|on / house"
            , test "mi moku lon" <| \_ -> translateNimiPona "mi moku lon" |> equal "I / to ingest / truely"
            , test "mi moku lon e pipi" <| \_ -> translateNimiPona "mi moku lon e pipi" |> equal "I / to ingest / truely / insect"
            , test "mi moku lon e pipi lon" <| \_ -> translateNimiPona "mi moku lon e pipi lon" |> equal "I / to ingest / truely / insect / true"
            , test "mi moku lon e pipi lon tomo" <| \_ -> translateNimiPona "mi moku lon e pipi lon tomo" |> equal "I / to ingest / truely / insect / in|at|on / house"
            , test "mi moku lon tomo" <| \_ -> translateNimiPona "mi moku lon tomo" |> equal "I / to ingest / in|at|on / house"
            , test "mi moku lon tomo e pipi" <| \_ -> translateNimiPona "mi moku lon tomo e pipi" |> equal "I / to ingest / in|at|on / house / insect"
            , test "suno li lon sewi" <| \_ -> translateNimiPona "suno li lon sewi" |> equal "sun / to be above"
            , test "mi telo e mi lon tomo telo" <| \_ -> translateNimiPona "mi telo e mi lon tomo telo" |> equal "I / to wash with water / I / in|at|on / restroom"
            , test "kili li lon poki" <| \_ -> translateNimiPona "kili li lon poki" |> equal "fruit / to be in|at|on / container"
            , test "sina lon e wile sina" <| \_ -> translateNimiPona "sina lon e wile sina" |> equal "you / to make real|aware|conscious|awake / desire / your"
            , test "sina wile lon e ona" <| \_ -> translateNimiPona "sina wile lon e ona" |> equal "you / want / to make real|aware|conscious|awake / he, she, it"
            , test "mi kepeken e ilo" <| \_ -> translateNimiPona "mi kepeken e ilo" |> equal "I / to use / tool"
            , test "sina wile kepeken e ilo" <| \_ -> translateNimiPona "sina wile kepeken e ilo" |> equal "you / want / to use / tool"
            , test "mi kepeken e poki ni" <| \_ -> translateNimiPona "mi kepeken e poki ni" |> equal "I / to use / container / this, that"
            , test "mi moku kepeken ilo moku" <| \_ -> translateNimiPona "mi moku kepeken ilo moku" |> equal "I / to ingest / with / spoon|fork|knife"
            , test "mi lukin kepeken" <| \_ -> translateNimiPona "mi lukin kepeken" |> equal "I / to seek to / to use"
            , test "mi lukin kepeken ilo suno" <| \_ -> translateNimiPona "mi lukin kepeken ilo suno" |> equal "I / to look / with / flashlight"
            , test "mi lukin e ni kepeken ilo suno" <| \_ -> translateNimiPona "mi lukin e ni kepeken ilo suno" |> equal "I / to see / this, that / with / flashlight"
            , test "mi tawa tomo mi" <| \_ -> translateNimiPona "mi tawa tomo mi" |> equal "I / to go to / house / my"
            , test "ona li tawa utala." <| \_ -> translateNimiPona "ona li tawa utala." |> equal "he, she, it / to go to / conflict."

            -- PRE_VERB case : lukin, wile, kama, kepeken
            -- PREPOSITION : lon, kepeken, tawa
            -- (wile or kama) (preposition) => wile or kama must be a pre verb and the preposition is nearly a standard verb, where the adverbs are some nouns
            -- (lukin) (preposition) => lukin must be a verb and the preposition is a preposition
            , test "mi lukin lon tomo." <| \_ -> translateNimiPona "mi lukin lon tomo." |> equal "I / to look / in|at|on / house."
            , test "mi lukin kepeken ilo." <| \_ -> translateNimiPona "mi lukin kepeken ilo." |> equal "I / to look / with / tool."
            , test "mi lukin tawa sina." <| \_ -> translateNimiPona "mi lukin tawa sina." |> equal "I / to look / to|for / you."
            , test "mi wile lon tomo." <| \_ -> translateNimiPona "mi wile lon tomo." |> equal "I / want / to be in|at|on / house."
            , test "mi wile kepeken ilo." <| \_ -> translateNimiPona "mi wile kepeken ilo." |> equal "I / want / to use / tool."
            , test "mi wile tawa sina." <| \_ -> translateNimiPona "mi wile tawa sina." |> equal "I / want / to go to / you."
            , test "mi kama lon tomo." <| \_ -> translateNimiPona "mi kama lon tomo." |> equal "I / going to / to be in|at|on / house."
            , test "mi kama kepeken ilo." <| \_ -> translateNimiPona "mi kama kepeken ilo." |> equal "I / going to / to use / tool."
            , test "mi kama tawa sina." <| \_ -> translateNimiPona "mi kama tawa sina." |> equal "I / going to / to go to / you."

            -- these phrases have no valid meaning
            --, test "mi kepeken lon tomo." <| \_ -> translateNimiPona "mi kepeken lon tomo." |> equal ""
            --, test "mi kepeken kepeken ilo." <| \_ -> translateNimiPona "mi kepeken kepeken ilo." |> equal ""
            --, test "mi kepeken tawa sina." <| \_ -> translateNimiPona "mi kepeken tawa sina." |> equal ""
            , test "sina wile tawa telo suli." <| \_ -> translateNimiPona "sina wile tawa telo suli." |> equal "you / want / to go to / ocean."
            , test "sina wile tawa telo suli. 2" <| \_ -> translatePona "sina wile tawa telo suli." |> equal "you / want / to go to / water / big."
            , test "sina wile kepeken ilo." <| \_ -> translateNimiPona "sina wile kepeken ilo." |> equal "you / want / to use / tool."
            , test "ona li tawa sewi kiwen." <| \_ -> translateNimiPona "ona li tawa sewi kiwen." |> equal "he, she, it / to go to / top of the rock."
            , test "ona li sewi kiwen." <| \_ -> translateNimiPona "ona li sewi kiwen." |> equal "he, she, it / ('top of the rock' as a verb or being...)."
            , test "ona li sewi kiwen. 2" <| \_ -> translatePona "ona li sewi kiwen." |> equal "he, she, it / to get up / hardly."
            , test "ona li sewi e kiwen. 2" <| \_ -> translatePona "ona li sewi e kiwen." |> equal "he, she, it / to lift / stone|rock."
            , test "ona li tawa sewi e kiwen. 2" <| \_ -> translatePona "ona li tawa sewi e kiwen." |> equal "he, she, it / to move / up / stone|rock."
            , test "mi tawa e kiwen." <| \_ -> translateNimiPona "mi tawa e kiwen." |> equal "I / to move / stone|rock."
            , test "mi tawa e ilo moku." <| \_ -> translateNimiPona "mi tawa e ilo moku." |> equal "I / to move / spoon|fork|knife."
            , test "ona li tawa e len mi." <| \_ -> translateNimiPona "ona li tawa e len mi." |> equal "he, she, it / to move / clothing / my."
            , test "mi toki tawa sina" <| \_ -> translateNimiPona "mi toki tawa sina" |> equal "I / to talk / to|for / you"
            , test "ona li lawa e jan tawa ma pona" <| \_ -> translateNimiPona "ona li lawa e jan tawa ma pona" |> equal "he, she, it / to lead / people / to|for / land / good"
            , test "ona li kama tawa ma mi" <| \_ -> translateNimiPona "ona li kama tawa ma mi" |> equal "he, she, it / going to / to go to / land / my"
            , test "ni li pona tawa mi" <| \_ -> translateNimiPona "ni li pona tawa mi" |> equal "this, that / to be good / to|for / I"
            , test "ni li ike tawa mi" <| \_ -> translateNimiPona "ni li ike tawa mi" |> equal "this, that / to be bad / to|for / I"
            , test "kili li pona tawa mi" <| \_ -> translateNimiPona "kili li pona tawa mi" |> equal "fruit / to be good / to|for / I"
            , test "toki li pona tawa mi" <| \_ -> translateNimiPona "toki li pona tawa mi" |> equal "language / to be good / to|for / I"
            , test "tomo li ike tawa mi" <| \_ -> translateNimiPona "tomo li ike tawa mi" |> equal "house / to be bad / to|for / I"
            , test "telo suli li ike tawa mi" <| \_ -> translateNimiPona "telo suli li ike tawa mi" |> equal "ocean / to be bad / to|for / I"
            , test "mi lukin e ma. ni li pona tawa mi." <| \_ -> translateNimiPona "mi lukin e ma. ni li pona tawa mi." |> equal "I / to see / land. this, that / to be good / to|for / I."
            , test "ma li pona lukin" <| \_ -> translateNimiPona "ma li pona lukin" |> equal "land / to be beautiful"

            -- as tomo tawa is a usual word, I prefer to respect the pi rule in this case.
            -- but now it is not supported unitl the 11th lesson
            -- TODO, test "mi pona e tomo pi jan pakala" <| \_ -> translateNimiPona "mi pona e tomo pi jan pakala" |> equal "I / to fix / house / to|for / people / destroyed"
            , test "tomo tawa" <| \_ -> translateNimiPona "tomo tawa" |> equal "car"
            , test "mi tomo tawa e ni" <| \_ -> translateNimiPona "mi tomo tawa e ni" |> equal "I / to transform as a vehicle / this, that"

            -- no ambiguity as we consider that "tomo tawa" is a vehicule
            , test "mi pana e tomo tawa sina" <| \_ -> translateNimiPona "mi pana e tomo tawa sina" |> equal "I / to give / car / your"

            -- but here we consider tawa as a preposition... because the preposition rule is prioritary
            , test "mi pana e tomo tawa sina 2" <| \_ -> translatePona "mi pana e tomo tawa sina" |> equal "I / to give / house / to|for / you"
            , test "mi tawa kepeken tomo tawa tawa tomo sina" <| \_ -> translateNimiPona "mi tawa kepeken tomo tawa tawa tomo sina" |> equal "I / to go to / with / car / to|for / house / your"
            , test "mi tawa kepeken tomo tawa kon tawa tomo sina" <| \_ -> translateNimiPona "mi tawa kepeken tomo tawa kon tawa tomo sina" |> equal "I / to go to / with / airplane / to|for / house / your"
            , test "mi tawa kepeken tomo tawa telo tawa tomo sina" <| \_ -> translateNimiPona "mi tawa kepeken tomo tawa telo tawa tomo sina" |> equal "I / to go to / with / boat / to|for / house / your"
            , test "sina toki e ni tawa mi: sina moku... ... " <| \_ -> translateNimiPona "sina toki e ni tawa mi: sina moku... ... " |> equal "you / to speak / this, that / to|for / I: you / to ingest. . . . . ."
            , test "ona li kama tawa tomo mi." <| \_ -> translateNimiPona "ona li kama tawa tomo mi." |> equal "he, she, it / going to / to go to / house / my."
            , test "mi kama e pakala." <| \_ -> translateNimiPona "mi kama e pakala." |> equal "I / to cause / accident."
            , test "sina kama e ni: mi wile moku" <| \_ -> translateNimiPona "sina kama e ni: mi wile moku" |> equal "you / to cause / this, that: I / want / to ingest"
            , test "mi kama jo e telo" <| \_ -> translateNimiPona "mi kama jo e telo" |> equal "I / going to / to have / water"
            , test "mi pona e ilo suno kepeken ilo lili" <| \_ -> translateNimiPona "mi pona e ilo suno kepeken ilo lili" |> equal "I / to fix / flashlight / with / tool / small"
            , test "toki pona li pona tawa mi" <| \_ -> translateNimiPona "toki pona li pona tawa mi" |> equal "language / good / to be good / to|for / I"
            , test "mi mute li pana e moku tawa ona mute" <| \_ -> translateNimiPona "mi mute li pana e moku tawa ona mute" |> equal "we / to give / food / to|for / they"
            , test "ni li tawa jan pona mi" <| \_ -> translateNimiPona "ni li tawa jan pona mi" |> equal "this, that / to go to / friend / my"
            , test "ilo li lon poki" <| \_ -> translateNimiPona "ilo li lon poki" |> equal "tool / to be in|at|on / container"
            , test "poki ni li lon jaki" <| \_ -> translateNimiPona "poki ni li lon jaki" |> equal "container / this, that / to be in|at|on / dirt"
            , test "mi wile tawa tomo ona kepeken tomo tawa mi" <| \_ -> translateNimiPona "mi wile tawa tomo ona kepeken tomo tawa mi" |> equal "I / want / to go to / house / his, her, its / with / car / my"
            , test "ona mute li utala toki" <| \_ -> translateNimiPona "ona mute li utala toki" |> equal "they / to argue"
            , test "sina wile kama tawa tomo toki." <| \_ -> translateNimiPona "sina wile kama tawa tomo toki." |> equal "you / want / to come / to|for / chat room."
            , test "jan li toki kepeken toki pona lon tomo toki." <| \_ -> translateNimiPona "jan li toki kepeken toki pona lon tomo toki." |> equal "people / to talk / with / language / good / in|at|on / chat room."
            , test "mi tawa tomo toki. ona li pona tawa mi." <| \_ -> translateNimiPona "mi tawa tomo toki. ona li pona tawa mi." |> equal "I / to go to / chat room. he, she, it / to be good / to|for / I."
            , test "sina kama jo e jan pona lon ni." <| \_ -> translateNimiPona "sina kama jo e jan pona lon ni." |> equal "you / going to / to have / friend / in|at|on / this, that."
            ]
        , describe
            "lesson #7 http://tokipona.net/tp/janpije/okamasona7.php"
            [ test "anpa" <| \_ -> translateNimiPona "anpa" |> equal "bottom"
            , test "insa" <| \_ -> translateNimiPona "insa" |> equal "inside"
            , test "monsi" <| \_ -> translateNimiPona "monsi" |> equal "back"
            , test "sama" <| \_ -> translateNimiPona "sama" |> equal "like"
            , test "tan" <| \_ -> translateNimiPona "tan" |> equal "because of"
            , test "poka" <| \_ -> translateNimiPona "poka" |> equal "with"
            , test "ona li lon sewi mi." <| \_ -> translateNimiPona "ona li lon sewi mi." |> equal "he, she, it / to be above / I."
            , test "pipi li lon anpa mi." <| \_ -> translateNimiPona "pipi li lon anpa mi." |> equal "insect / to be underneath / I."
            , test "moku li lon insa mi" <| \_ -> translateNimiPona "moku li lon insa mi" |> equal "food / to be inside / I"
            , test "len li lon monsi mi" <| \_ -> translateNimiPona "len li lon monsi mi" |> equal "clothing / to be behind / I"
            , test "mi moku lon poka sina" <| \_ -> translateNimiPona "mi moku lon poka sina" |> equal "I / to ingest / beside / you"
            , test "sina lukin e ona lon poka mi" <| \_ -> translateNimiPona "sina lukin e ona lon poka mi" |> equal "you / to see / he, she, it / beside / I"
            , test "mi anpa e jan utala" <| \_ -> translateNimiPona "mi anpa e jan utala" |> equal "I / to defeat / soldier"
            , test "jan ni li sama mi" <| \_ -> translateNimiPona "jan ni li sama mi" |> equal "people / this, that / to be like / I"
            , test "ona li lukin sama pipi" <| \_ -> translateNimiPona "ona li lukin sama pipi" |> equal "he, she, it / to look / like / insect"
            , test "sama li ike" <| \_ -> translateNimiPona "sama li ike" |> equal "equality / to be bad"
            , test "jan sama" <| \_ -> translateNimiPona "jan sama" |> equal "sibling"
            , test "mi moku tan ni: mi wile moku" <| \_ -> translateNimiPona "mi moku tan ni: mi wile moku" |> equal "I / to ingest / because of / this, that: I / want / to ingest"
            , test "mi tan ma ike" <| \_ -> translateNimiPona "mi tan ma ike" |> equal "I / to come from / land / bad"
            , test "jan pona mi li lon poka mi" <| \_ -> translateNimiPona "jan pona mi li lon poka mi" |> equal "friend / my / to be nearby / I"
            , test "suno li lon sewi mi" <| \_ -> translateNimiPona "suno li lon sewi mi" |> equal "sun / to be above / I"
            , test "ma li lon anpa mi" <| \_ -> translateNimiPona "ma li lon anpa mi" |> equal "land / to be underneath / I"
            , test "ijo ike li lon monsi mi" <| \_ -> translateNimiPona "ijo ike li lon monsi mi" |> equal "something / bad / to be behind / I"
            , test "mi pona tan ni: mi lon" <| \_ -> translateNimiPona "mi pona tan ni: mi lon" |> equal "I / to be good / because of / this, that: I / to be in|at|on"
            , test "mi lukin e ma lon poka tomo" <| \_ -> translateNimiPona "mi lukin e ma lon poka tomo" |> equal "I / to see / land / beside / house"
            , test "jan li lukin sama pipi" <| \_ -> translateNimiPona "jan li lukin sama pipi" |> equal "people / to look / like / insect"
            , test "poka mi li pakala." <| \_ -> translateNimiPona "poka mi li pakala." |> equal "side / my / to screw up."
            , test "mi kepeken e poki e ilo moku." <| \_ -> translateNimiPona "mi kepeken e poki e ilo moku." |> equal "I / to use / container / and / spoon|fork|knife."
            , test "mi kepeken e poki moku e ilo moku." <| \_ -> translateNimiPona "mi kepeken e poki moku e ilo moku." |> equal "I / to use / bowl|cup / and / spoon|fork|knife."
            , test "jan li lon insa tomo." <| \_ -> translateNimiPona "jan li lon insa tomo." |> equal "people / to be inside / house."
            ]
        , describe
            "sample"
            [ test "s1" <| \_ -> translateNimiPona "" |> equal ""
            , test "s2" <| \_ -> translateNimiPona "" |> equal ""
            ]
        ]
