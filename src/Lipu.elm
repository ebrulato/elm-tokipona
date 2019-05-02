module Lipu exposing
    ( ADJECTIVE(..)
    , ADJECTIVE_NUMERAL(..)
    , ADVERB(..)
    , GRAMMAR
    , NOUN(..)
    , PREPOSITION(..)
    , PRE_VERB(..)
    , TOKIPONA
    , VERB(..)
    , VERB_TRANSITIVE(..)
    , WORD(..)
    , WORD_KIND(..)
    , getKind
    , getRawTokipona
    , rawTokiponaToString
    , tokiponaLipu
    , tokiponaWords
    , word_e
    , word_kama
    , word_li
    , word_lili
    , word_lukin
    , word_mi
    , word_mute
    , word_sewi
    , word_sina
    , word_wile
    )

import Dict exposing (Dict, fromList, get)
import Tools exposing (cutList)


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


type ADJECTIVE_NUMERAL
    = ADJECTIVE_NUMERAL Int
    | NO_NUMERAL


type alias GRAMMAR =
    String


type PREPOSITION
    = PREPOSITION String
    | NO_PREP


type WORD
    = WORD TOKIPONA WORD_KIND NOUN PRE_VERB VERB VERB_TRANSITIVE ADJECTIVE ADVERB ADJECTIVE_NUMERAL PREPOSITION
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
    | PREPOSITION_KIND


tokiponaWords : Dict String WORD
tokiponaWords =
    fromList
        [ -- lesson #3
          ( "jan", WORD "jan" NOUN_KIND (NOUN "people; human, being, person, somebody, anybody") NO_PRE_VERB BE (VERB_TRANSITIVE "to personify; to humanize, to personalize") (ADJECTIVE "somebody’s; human, personal, of people") (ADVERB "human, somebody’s, personal, of people") NO_NUMERAL NO_PREP )
        , ( "mi", WORD "mi" NOUN_KIND (NOUN "I; me, we, us") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "my; our") ADV NO_NUMERAL NO_PREP )
        , ( "moku", WORD "moku" VERB_KIND (NOUN "food; meal") NO_PRE_VERB (VERB "to ingest; to eat, to drink, to consume, to swallow") (VERB_TRANSITIVE "to ingest; to eat, to drink, to consume, to swallow") (ADJECTIVE "eating") (ADVERB "eating") NO_NUMERAL NO_PREP )
        , ( "sina", WORD "sina" NOUN_KIND (NOUN "you") NO_PRE_VERB (VERB "to be you") NO_VERB_TRANSITIVE (ADJECTIVE "your") ADV NO_NUMERAL NO_PREP )
        , ( "suli", WORD "suli" ADJECTIVE_KIND (NOUN "size") NO_PRE_VERB BE (VERB_TRANSITIVE "to enlarge, to lengthen") (ADJECTIVE "big; heavy, large, long, tall; important; adult") (ADVERB "big; tall, long, adult, important") NO_NUMERAL NO_PREP )
        , ( "suno", WORD "suno" NOUN_KIND (NOUN "sun; light, brightness, glow, radiance, shine; light source") NO_PRE_VERB BE (VERB_TRANSITIVE "to light; to illumine") (ADJECTIVE "sunny; sunnily") (ADVERB "sunny; sunnily") NO_NUMERAL NO_PREP )
        , ( "telo", WORD "telo" NOUN_KIND (NOUN "water; liquid, fluid, wet substance; beverage; juice, sauce") NO_PRE_VERB BE (VERB_TRANSITIVE "to wash with water; to water,to put water to, to melt, to liquify") (ADJECTIVE "wett; slobbery, moist, damp, humid, sticky, sweaty, dewy, drizzly") (ADVERB "wett; slobbery, moist, damp, humid, sticky, sweaty, dewy, drizzly") NO_NUMERAL NO_PREP )

        -- TODO interjection "pona!" conditional "pona la"
        , ( "pona", WORD "pona" ADJECTIVE_KIND AS_NOUN NO_PRE_VERB (VERB "to be good; to be simple, positive, nice, correct, right, useful; to be friendly, peaceful") (VERB_TRANSITIVE "to fix; to improve, to repair, to make good") (ADJECTIVE "good; simple, positive, nice, correct, right, useful; friendly, peaceful") (ADVERB "well; good, simple, positive, nice, correct, right") NO_NUMERAL NO_PREP )
        , ( "li", PARTICLE "li" "separates some subjects (especially third-person) from the verb" )

        -- lesson #4
        , ( "ilo", WORD "ilo" NOUN_KIND (NOUN "tool; implement, machine, device; thing used for a specific purpose") NO_PRE_VERB BE (VERB_TRANSITIVE "{to hack; to transform something as a tool}") (ADJECTIVE "useful") (ADVERB "usefully") NO_NUMERAL NO_PREP )
        , ( "kili", WORD "kili" NOUN_KIND (NOUN "fruit; vegetable, mushroom") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "fruity") (ADVERB "fruity") NO_NUMERAL NO_PREP )
        , ( "ni", WORD "ni" NOUN_KIND (NOUN "this, that") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "this, that") NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "ona", WORD "ona" NOUN_KIND (NOUN "he, she, it") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "his, her, its") NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "pipi", WORD "pipi" NOUN_KIND (NOUN "insect; bug, ant, spider") NO_PRE_VERB BE NO_VERB_TRANSITIVE ADJ ADV NO_NUMERAL NO_PREP )
        , ( "ma", WORD "ma" NOUN_KIND (NOUN "area; earth, land; outdoor area; world; region, country, territory; soil") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "countrified; outdoor, alfresco, open-air") NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "ijo", WORD "ijo" NOUN_KIND (NOUN "something; anything, stuff, thing, object") NO_PRE_VERB BE (VERB_TRANSITIVE "to objectify") (ADJECTIVE "of something") (ADVERB "of something") NO_NUMERAL NO_PREP )
        , ( "jo", WORD "jo" VERB_TRANSITIVE_KIND (NOUN "possession; having, content") NO_PRE_VERB BE (VERB_TRANSITIVE "to have; to carry, contain, hold") (ADJECTIVE "private; personal") (ADVERB "private; personal") NO_NUMERAL NO_PREP )
        , ( "lukin", WORD "lukin" VERB_KIND (NOUN "view; look, glance, sight, gaze, glimpse, seeing, vision") (PRE_VERB "to seek to; look for, try to") (VERB "to look; to pay attention, to examine, to observe, to read, to watch") (VERB_TRANSITIVE "to see; to look at, to watch, to read") (ADJECTIVE "visual(ly)") (ADVERB "visual(ly)") NO_NUMERAL NO_PREP )
        , ( "oko", WORD "oko" NOUN_KIND (NOUN "eye") NO_PRE_VERB (VERB "to see") (VERB_TRANSITIVE "to see") (ADJECTIVE "optical; eye-") NO_ADVERB NO_NUMERAL NO_PREP )

        -- TODO interjection "pakala!"
        , ( "pakala", WORD "pakala" VERB_KIND (NOUN "accident; blunder, mistake, destruction, damage, breaking") NO_PRE_VERB (VERB "to screw up; to fall apart, to break") (VERB_TRANSITIVE "to screw up; to ruin, to break, to hurt, to injure, to damage, to destroy") (ADJECTIVE "destroyed; ruined, demolished, shattered, wrecked") (ADVERB "destroyed; ruined, demolished, shattered, wrecked") NO_NUMERAL NO_PREP )
        , ( "unpa", WORD "unpa" VERB_KIND (NOUN "sex; sexuality") NO_PRE_VERB (VERB "to have sex") (VERB_TRANSITIVE "to have sex with; to sleep with, to fuck") (ADJECTIVE "erotic, sexual") (ADVERB "erotic, sexual") NO_NUMERAL NO_PREP )
        , ( "wile", WORD "wile" VERB_KIND (NOUN "desire; need, will") (PRE_VERB "want; need, wish, have to, must, will, should") (VERB "to want; need, wish, have to, must, will, should") (VERB_TRANSITIVE "to want; need, wish, have to, must, will, should") ADJ ADV NO_NUMERAL NO_PREP )
        , ( "e", PARTICLE "e" "introduces direct object" )

        -- lesson #5
        -- TODO interjection "ike!" & "ike la"
        , ( "ike", WORD "ike" ADJECTIVE_KIND (NOUN "negativity; badness, evil") NO_PRE_VERB (VERB "to be bad; to suck") (VERB_TRANSITIVE "to make bad; to worsen") (ADJECTIVE "bad; negative, wrong, evil, overly complex") (ADVERB "badly; negatively, wrongly, evily, intricately") NO_NUMERAL NO_PREP )

        -- TODO interjection "jaki!"
        , ( "jaki", WORD "jaki" ADJECTIVE_KIND (NOUN "dirt; pollution, garbage, filth, feces") NO_PRE_VERB BE (VERB_TRANSITIVE "to pollute; to dirty") (ADJECTIVE "dirty; gross, filthy, obscene") (ADVERB "dirty; gross, filthy") NO_NUMERAL NO_PREP )
        , ( "lawa", WORD "lawa" ADJECTIVE_KIND (NOUN "head; mind") NO_PRE_VERB BE (VERB_TRANSITIVE "to lead; to control, to rule, to steer") (ADJECTIVE "main; leading, in charge") (ADVERB "main; leading, in charge") NO_NUMERAL NO_PREP )
        , ( "len", WORD "len" NOUN_KIND (NOUN "clothing; cloth, fabric, network, internet") NO_PRE_VERB BE (VERB_TRANSITIVE "to wear; to be dressed, to dress") (ADJECTIVE "dressed; clothed, costumed, dressed up") NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "lili", WORD "lili" ADJECTIVE_KIND (NOUN "smallness; youth, immaturity") NO_PRE_VERB (VERB "to be small; little, young, short") (VERB_TRANSITIVE "to reduce; to shorten, to shrink, to lessen") (ADJECTIVE "small; little, young, a bit, short, few, less") (ADVERB "barely; small little, young, a bit, short, few, less") NO_NUMERAL NO_PREP )
        , ( "mute", WORD "mute" ADJECTIVE_KIND (NOUN "amount, quantity") NO_PRE_VERB BE (VERB_TRANSITIVE "to make many or much") (ADJECTIVE "many; very, much, several, a lot, abundant, numerous, more") (ADVERB "many; very, much, several, a lot, abundant, numerous, more") (ADJECTIVE_NUMERAL 20) NO_PREP )
        , ( "nasa", WORD "nasa" ADJECTIVE_KIND (NOUN "stupidity; foolishness, silliness, nonsense, idiocy, obtuseness, muddler") NO_PRE_VERB BE (VERB_TRANSITIVE "to drive crazy; to make weird") (ADJECTIVE "crazy; silly, foolish, drunk, strange, stupid, weird") (ADVERB "silly; crazy, foolish, drunk, strange, stupid, weird") NO_NUMERAL NO_PREP )
        , ( "seli", WORD "seli" NOUN_KIND (NOUN "fire; warmth, heat") NO_PRE_VERB BE (VERB_TRANSITIVE "to heat; to warm up, to cook") (ADJECTIVE "hot; warm, cooked") (ADVERB "hot; warm, cooked") NO_NUMERAL NO_PREP )
        , ( "sewi", WORD "sewi" ADJECTIVE_KIND (NOUN "high; up, above, top, over, on, sky") NO_PRE_VERB (VERB "to get up") (VERB_TRANSITIVE "to lift") (ADJECTIVE "elevated; superior, religious, formal") (ADVERB "up; superiorly, religiously, elevated, formal") NO_NUMERAL NO_PREP )
        , ( "tomo", WORD "tomo" NOUN_KIND (NOUN "house; home, room, building, indoor constructed space") NO_PRE_VERB BE (VERB_TRANSITIVE "to build; to construct, to engineer") (ADJECTIVE "urban; domestic, household") (ADVERB "urbanly; domestically") NO_NUMERAL NO_PREP )
        , ( "utala", WORD "utala" NOUN_KIND (NOUN "conflict; disharmony, fight, war, battle, attack, violence") NO_PRE_VERB (VERB "to hit; to strike, to attack, to compete against") (VERB_TRANSITIVE "to make bad, to worsen") (ADJECTIVE "fighting") (ADVERB "fighting") NO_NUMERAL NO_PREP )

        -- lesson #6
        , ( "kama", WORD "kama" VERB_KIND (NOUN "event; beginning, happening, chance, arrival") (PRE_VERB "going to; to become, to manage to") (VERB "to come; to become, to arrive, to happen") (VERB_TRANSITIVE "to cause; to bring about, to summon") (ADJECTIVE "coming; future") (ADVERB "coming; future") NO_NUMERAL NO_PREP )
        , ( "kepeken", WORD "kepeken" VERB_KIND (NOUN "tool; use, usage") (PRE_VERB "to use") (VERB "to use") (VERB_TRANSITIVE "to use") (ADJECTIVE "using") (ADVERB "usingly") NO_NUMERAL (PREPOSITION "with") )
        , ( "kiwen", WORD "kiwen" NOUN_KIND (NOUN "stone|rock; hard thing, metal, mineral, clay") NO_PRE_VERB (VERB "to be solid; to be harden") (VERB_TRANSITIVE "to solidify; to harden, to petrify, to fossilize") (ADJECTIVE "hard; solid, stone-like, made of stone or metal") (ADVERB "hardly; solidly, stone-likely, metal-likely") NO_NUMERAL NO_PREP )
        , ( "kon", WORD "kon" NOUN_KIND (NOUN "air; wind, smell, soul") NO_PRE_VERB (VERB "to breathe") (VERB_TRANSITIVE "to blow away something, to puff away something") (ADJECTIVE "air-like; ethereal, gaseous") (ADVERB "air-likely; ethereally, gaseous") NO_NUMERAL NO_PREP )
        , ( "lon", WORD "lon" VERB_KIND (NOUN "existence; being, presence") NO_PRE_VERB (VERB "to be in|at|on; to be present, to be real|true, to exist") (VERB_TRANSITIVE "to make real|aware|conscious|awake") (ADJECTIVE "true; existing, correct, real, genuine") (ADVERB "truely; existing, correctly, really, genuinely") NO_NUMERAL (PREPOSITION "in|at|on") )
        , ( "pana", WORD "pana" VERB_TRANSITIVE_KIND (NOUN "giving; transfer, exchange") NO_PRE_VERB (VERB "to give; to put, to send, to place, to release, to emit, to cause") (VERB_TRANSITIVE "to give; to put, to send, to place, to release, to emit, to cause") (ADJECTIVE "generous") (ADVERB "generously") NO_NUMERAL NO_PREP )
        , ( "poki", WORD "poki" NOUN_KIND (NOUN "container; box, bowl, cup, glass") NO_PRE_VERB (VERB "to put in; to box up, to can, to bottle") (VERB_TRANSITIVE "to put in; to box up, to can, to bottle") (ADJECTIVE "restrained; subdued") (ADVERB "restrainedly") NO_NUMERAL NO_PREP )

        -- TODO toki! hello, hi, good morning
        , ( "toki", WORD "toki" NOUN_KIND (NOUN "language; speech, tongue, lingo, jargon") NO_PRE_VERB (VERB "to talk; to chat, to communicate") (VERB_TRANSITIVE "to speak; to talk, to say, to pronounce, to discourse") (ADJECTIVE "speaking; eloquent, linguistic, verbal, grammatical") (ADVERB "speakingly; eloquently, linguistically, verbally, grammatically") NO_NUMERAL NO_PREP )
        , ( "tawa", WORD "tawa" VERB_KIND (NOUN "movement; transportation") NO_PRE_VERB (VERB "to go to; to walk, to travel, to move, to leave") (VERB_TRANSITIVE "to move; to displace") (ADJECTIVE "moving; mobile") (ADVERB "mobilely") NO_NUMERAL (PREPOSITION "to|for; in order to, towards, until") )
        ]


tokiponaLipu : Dict String WORD
tokiponaLipu =
    fromList
        [ ( "ike lukin", WORD "ike lukin" ADJECTIVE_KIND (NOUN "ugliness") NO_PRE_VERB (VERB "to be ugly") (VERB_TRANSITIVE "to make ugly") (ADJECTIVE "ugly") (ADVERB "basely") NO_NUMERAL NO_PREP )
        , ( "ilo moku", WORD "ilo moku" NOUN_KIND (NOUN "spoon|fork|knife") NO_PRE_VERB BE (VERB_TRANSITIVE "to make a spoon|fork|knife of") ADJ ADV NO_NUMERAL NO_PREP )
        , ( "ilo suno", WORD "ilo suno" NOUN_KIND (NOUN "flashlight; lamp, light") NO_PRE_VERB BE (VERB_TRANSITIVE "to make a light of") ADJ ADV NO_NUMERAL NO_PREP )
        , ( "jan ike", WORD "jan ike" NOUN_KIND (NOUN "enemy") NO_PRE_VERB (VERB "to be an ennemy") (VERB_TRANSITIVE "to make an enemy of") (ADJECTIVE "inimical") (ADVERB "hostilely") NO_NUMERAL NO_PREP )
        , ( "jan lawa", WORD "jan lawa" NOUN_KIND (NOUN "leader") NO_PRE_VERB (VERB "to be a leader") (VERB_TRANSITIVE "to make a leader of") (ADJECTIVE "leaderly") (ADVERB "leaderly") NO_NUMERAL NO_PREP )
        , ( "jan lili", WORD "jan lili" NOUN_KIND (NOUN "child") NO_PRE_VERB (VERB "to be a child; to be childly") (VERB_TRANSITIVE "to give birth") (ADJECTIVE "childly") (ADVERB "childly") NO_NUMERAL NO_PREP )
        , ( "jan pona", WORD "jan pona" NOUN_KIND (NOUN "friend") NO_PRE_VERB (VERB "to be a friend") (VERB_TRANSITIVE "to make a friend of") (ADJECTIVE "friendly") (ADVERB "friendly") NO_NUMERAL NO_PREP )
        , ( "jan sewi", WORD "jan sewi" NOUN_KIND (NOUN "clergyman; god") NO_PRE_VERB (VERB "to be a clergyman") (VERB_TRANSITIVE "to ordain as a clergyman") (ADJECTIVE "as a clergyman") (ADVERB "as a clergyman") NO_NUMERAL NO_PREP )
        , ( "jan suli", WORD "jan suli" NOUN_KIND (NOUN "adult") NO_PRE_VERB (VERB "to be an adult") (VERB_TRANSITIVE "to raise as an adult") (ADJECTIVE "adultly") (ADVERB "adultly") NO_NUMERAL NO_PREP )
        , ( "jan unpa", WORD "jan unpa" NOUN_KIND (NOUN "lover") NO_PRE_VERB (VERB "to be a lover") (VERB_TRANSITIVE "to make a lover of") (ADJECTIVE "as a lover") (ADVERB "as a lover") NO_NUMERAL NO_PREP )
        , ( "jan utala", WORD "jan utala" NOUN_KIND (NOUN "soldier") NO_PRE_VERB (VERB "to be a soldier") (VERB_TRANSITIVE "to make a soldier of") (ADJECTIVE "soldiery") (ADVERB "soldiery") NO_NUMERAL NO_PREP )
        , ( "ma tomo", WORD "ma tomo" NOUN_KIND (NOUN "town; city") NO_PRE_VERB (VERB "to be a town") (VERB_TRANSITIVE "to make a town with") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "ma telo", WORD "ma telo" NOUN_KIND (NOUN "lake; mud, swamp") NO_PRE_VERB (VERB "to be a lake") (VERB_TRANSITIVE "to transform as a lake, mud, swamp") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "mi mute", WORD "mi mute" NOUN_KIND (NOUN "we") NO_PRE_VERB (VERB "to be us") NO_VERB_TRANSITIVE (ADJECTIVE "our") (ADVERB "us") NO_NUMERAL NO_PREP )
        , ( "moku lili", WORD "moku lili" NOUN_KIND (NOUN "nibbling") NO_PRE_VERB (VERB "to nibble") (VERB_TRANSITIVE "to nibble") (ADJECTIVE "nibbled") (ADVERB "nibbling") NO_NUMERAL NO_PREP )
        , ( "ona mute", WORD "ona mute" NOUN_KIND (NOUN "they") NO_PRE_VERB (VERB "to be them") NO_VERB_TRANSITIVE (ADJECTIVE "their") (ADVERB "them") NO_NUMERAL NO_PREP )
        , ( "pona lukin", WORD "pona lukin" ADJECTIVE_KIND (NOUN "beauty") NO_PRE_VERB (VERB "to be beautiful") (VERB_TRANSITIVE "to make beautyful") (ADJECTIVE "beautyful") (ADVERB "beautifully") NO_NUMERAL NO_PREP )
        , ( "sewi kiwen", WORD "sewi kiwen" NOUN_KIND (NOUN "top of the rock") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as a top of the rock") ADJ ADV NO_NUMERAL NO_PREP )
        , ( "telo kili", WORD "telo kili" NOUN_KIND (NOUN "fruit juice") NO_PRE_VERB (VERB "to be a fruit juice") (VERB_TRANSITIVE "to transform as a fruit juice") (ADJECTIVE "fruity") (ADVERB "fruity") NO_NUMERAL NO_PREP )
        , ( "telo nasa", WORD "telo nasa" NOUN_KIND (NOUN "alcohol; beer, wine") NO_PRE_VERB (VERB "to be an alcohol") (VERB_TRANSITIVE "to transform as an alcohol") (ADJECTIVE "alcoholic") (ADVERB "alcoholic") NO_NUMERAL NO_PREP )
        , ( "telo suli", WORD "telo suli" NOUN_KIND (NOUN "ocean") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as an ocean") ADJ ADV NO_NUMERAL NO_PREP )
        , ( "tomo tawa", WORD "tomo tawa" NOUN_KIND (NOUN "car; vehicle") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as a vehicle") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "tomo tawa telo", WORD "tomo tawa telo" NOUN_KIND (NOUN "boat") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as a boat") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "tomo tawa kon", WORD "tomo tawa kon" NOUN_KIND (NOUN "airplane; helicopter, air air vehicle") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as an air vehicle") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "tomo telo", WORD "tomo telo" NOUN_KIND (NOUN "restroom") NO_PRE_VERB (VERB "to be a restroom") (VERB_TRANSITIVE "to transform as a restroom") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "tomo toki", WORD "tomo toki" NOUN_KIND (NOUN "chat room") NO_PRE_VERB BE (VERB_TRANSITIVE "to make a chat room with") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "utala toki", WORD "utala toki" NOUN_KIND (NOUN "an argue") NO_PRE_VERB (VERB "to argue") (VERB_TRANSITIVE "to argue; to argue that, to argue") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        ]


getRawTokipona : String -> WORD
getRawTokipona word =
    let
        tp =
            get word tokiponaWords

        tp2 =
            get word tokiponaLipu
    in
    case tp of
        Just w ->
            w

        Nothing ->
            case tp2 of
                Just w ->
                    w

                Nothing ->
                    ERROR word


word_mi =
    getRawTokipona "mi"


word_sina =
    getRawTokipona "sina"


word_mute =
    getRawTokipona "mute"


word_lili =
    getRawTokipona "lili"


word_sewi =
    getRawTokipona "sewi"


word_li =
    getRawTokipona "li"


word_e =
    getRawTokipona "e"


word_wile =
    getRawTokipona "wile"


word_kama =
    getRawTokipona "kama"


word_lukin =
    getRawTokipona "lukin"


doPona : Bool -> String -> String
doPona pona str =
    if pona then
        let
            ( begin, end, idx ) =
                cutList (\c -> c == ';') False False (String.toList str)
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

                WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num prep ->
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
                            "[ERROR: " ++ tokipona ++ " is unknown]"

                        PREPOSITION_KIND ->
                            case prep of
                                PREPOSITION s ->
                                    s

                                NO_PREP ->
                                    "[ERROR: " ++ tokipona ++ " is not a preposition]"
    in
    doPona pona result


rawTokiponaToString : Bool -> WORD_KIND -> WORD -> String
rawTokiponaToString pona kind w =
    case w of
        ERROR s ->
            "[ERROR: '" ++ s ++ "' is not a valid word]"

        PARTICLE tokipona grammar ->
            grammar

        WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num prep ->
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

                PREPOSITION_KIND ->
                    case prep of
                        PREPOSITION s ->
                            doPona pona s

                        NO_PREP ->
                            "[ERROR: " ++ tokipona ++ " is not a preposition]"


getKind : WORD -> WORD_KIND
getKind w =
    case w of
        ERROR s ->
            UNKNOW_KIND

        PARTICLE tokipona grammar ->
            GRAMMAR_KIND

        WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num prep ->
            defaultKind
