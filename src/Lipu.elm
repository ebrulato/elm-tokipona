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
    , convertForeign2Word
    , convertSimple2Word
    , getKind
    , getRawTokipona
    , getTokipona
    , rawTokiponaToString
    , tokiponaLipu
    , tokiponaWords
    , word_ala
    , word_anu
    , word_e
    , word_kama
    , word_kepeken
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
    | SIMPLE TOKIPONA String
    | FOREIGN TOKIPONA String
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
    | LOC_KIND
    | SIMPLE_KIND
    | FOREIGN_KIND


tokiponaWords : Dict String WORD
tokiponaWords =
    fromList
        [ -- lesson #3
          ( "jan", WORD "jan" NOUN_KIND (NOUN "people; human, being, person, somebody, anybody") NO_PRE_VERB BE (VERB_TRANSITIVE "to personify; to humanize, to personalize") (ADJECTIVE "somebody’s; human, personal, of people") (ADVERB "human, somebody’s, personal, of people") NO_NUMERAL NO_PREP )
        , ( "mi", WORD "mi" NOUN_KIND (NOUN "I; me, we, us") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "my; our") (ADVERB "my|our") NO_NUMERAL NO_PREP )
        , ( "moku", WORD "moku" VERB_KIND (NOUN "food; meal") NO_PRE_VERB (VERB "to ingest; to eat, to drink, to consume, to swallow") (VERB_TRANSITIVE "to ingest; to eat, to drink, to consume, to swallow") (ADJECTIVE "eating") (ADVERB "eating") NO_NUMERAL NO_PREP )
        , ( "sina", WORD "sina" NOUN_KIND (NOUN "you") NO_PRE_VERB (VERB "to be you") NO_VERB_TRANSITIVE (ADJECTIVE "your") (ADVERB "your") NO_NUMERAL NO_PREP )
        , ( "suli", WORD "suli" ADJECTIVE_KIND (NOUN "size") NO_PRE_VERB (VERB "to be big; to be heavy, large, long, tall; to be important; to be adult") (VERB_TRANSITIVE "to enlarge, to lengthen") (ADJECTIVE "big; heavy, large, long, tall; important; adult") (ADVERB "big; tall, long, adult, important") NO_NUMERAL NO_PREP )
        , ( "suno", WORD "suno" NOUN_KIND (NOUN "sun; light, brightness, glow, radiance, shine; light source") NO_PRE_VERB BE (VERB_TRANSITIVE "to light; to illumine") (ADJECTIVE "sunny; sunnily") (ADVERB "sunny; sunnily") NO_NUMERAL NO_PREP )
        , ( "telo", WORD "telo" NOUN_KIND (NOUN "water; liquid, fluid, wet substance; beverage; juice, sauce") NO_PRE_VERB (VERB "to be wet") (VERB_TRANSITIVE "to wash with water; to water,to put water to, to melt, to liquify") (ADJECTIVE "wett; slobbery, moist, damp, humid, sticky, sweaty, dewy, drizzly") (ADVERB "wett; slobbery, moist, damp, humid, sticky, sweaty, dewy, drizzly") NO_NUMERAL NO_PREP )

        -- TODO interjection "pona!" conditional "pona la"
        , ( "pona", WORD "pona" ADJECTIVE_KIND AS_NOUN NO_PRE_VERB (VERB "to be good; to be simple, positive, nice, correct, right, useful; to be friendly, peaceful") (VERB_TRANSITIVE "to fix; to improve, to repair, to make good") (ADJECTIVE "good; simple, positive, nice, correct, right, useful; friendly, peaceful") (ADVERB "well; good, simple, positive, nice, correct, right") NO_NUMERAL NO_PREP )
        , ( "li", PARTICLE "li" "separates some subjects (especially third-person) from the verb" )

        -- lesson #4
        , ( "ilo", WORD "ilo" NOUN_KIND (NOUN "tool; implement, machine, device; thing used for a specific purpose") NO_PRE_VERB BE (VERB_TRANSITIVE "{to hack; to transform something as a tool}") (ADJECTIVE "useful") (ADVERB "usefully") NO_NUMERAL NO_PREP )
        , ( "kili", WORD "kili" NOUN_KIND (NOUN "fruit; vegetable, mushroom") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "fruity") (ADVERB "fruity") NO_NUMERAL NO_PREP )
        , ( "ni", WORD "ni" NOUN_KIND (NOUN "this|that") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "this|that") NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "ona", WORD "ona" NOUN_KIND (NOUN "he|she|it") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "his|her|its") NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "pipi", WORD "pipi" NOUN_KIND (NOUN "insect; bug, ant, spider") NO_PRE_VERB BE NO_VERB_TRANSITIVE ADJ ADV NO_NUMERAL NO_PREP )
        , ( "ma", WORD "ma" NOUN_KIND (NOUN "land; earth, area; outdoor area; world; region, country, territory; soil") NO_PRE_VERB BE NO_VERB_TRANSITIVE (ADJECTIVE "countrified; outdoor, alfresco, open-air") NO_ADVERB NO_NUMERAL NO_PREP )
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
        , ( "jaki", WORD "jaki" ADJECTIVE_KIND (NOUN "dirt; pollution, garbage, filth, feces") NO_PRE_VERB (VERB "to be dirty") (VERB_TRANSITIVE "to pollute; to dirty") (ADJECTIVE "dirty; gross, filthy, obscene") (ADVERB "dirty; gross, filthy") NO_NUMERAL NO_PREP )
        , ( "lawa", WORD "lawa" ADJECTIVE_KIND (NOUN "head; mind") NO_PRE_VERB BE (VERB_TRANSITIVE "to lead; to control, to rule, to steer") (ADJECTIVE "main; leading, in charge") (ADVERB "main; leading, in charge") NO_NUMERAL NO_PREP )
        , ( "len", WORD "len" NOUN_KIND (NOUN "clothing; cloth, fabric, network, internet") NO_PRE_VERB BE (VERB_TRANSITIVE "to wear; to be dressed, to dress") (ADJECTIVE "dressed; clothed, costumed, dressed up") NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "lili", WORD "lili" ADJECTIVE_KIND (NOUN "smallness; youth, immaturity") NO_PRE_VERB (VERB "to be small; little, young, short") (VERB_TRANSITIVE "to reduce; to shorten, to shrink, to lessen") (ADJECTIVE "small; little, young, a bit, short, few, less") (ADVERB "barely; small little, young, a bit, short, few, less") NO_NUMERAL NO_PREP )
        , ( "mute", WORD "mute" ADJECTIVE_KIND (NOUN "amount, quantity") NO_PRE_VERB BE (VERB_TRANSITIVE "to make many or much") (ADJECTIVE "many; very, much, several, a lot, abundant, numerous, more") (ADVERB "many; very, much, several, a lot, abundant, numerous, more") (ADJECTIVE_NUMERAL 20) NO_PREP )
        , ( "nasa", WORD "nasa" ADJECTIVE_KIND (NOUN "stupidity; foolishness, silliness, nonsense, idiocy, obtuseness, muddler") NO_PRE_VERB (VERB "to be crazy") (VERB_TRANSITIVE "to drive crazy; to make weird") (ADJECTIVE "crazy; silly, foolish, drunk, strange, stupid, weird") (ADVERB "silly; crazy, foolish, drunk, strange, stupid, weird") NO_NUMERAL NO_PREP )
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
        , ( "toki", WORD "toki" NOUN_KIND (NOUN "language; speech, tongue, lingo, jargon, hello, hi") NO_PRE_VERB (VERB "to talk; to chat, to communicate") (VERB_TRANSITIVE "to speak; to talk, to say, to pronounce, to discourse") (ADJECTIVE "speaking; eloquent, linguistic, verbal, grammatical") (ADVERB "speakingly; eloquently, linguistically, verbally, grammatically") NO_NUMERAL NO_PREP )
        , ( "tawa", WORD "tawa" VERB_KIND (NOUN "movement; transportation") NO_PRE_VERB (VERB "to go to; to walk, to travel, to move, to leave") (VERB_TRANSITIVE "to move; to displace") (ADJECTIVE "moving; mobile") (ADVERB "mobilely") NO_NUMERAL (PREPOSITION "to|for; in order to, towards, until") )

        -- lesson #7
        , ( "anpa", WORD "anpa" LOC_KIND (NOUN "bottom; lower part, under, below, floor, beneath, ground") NO_PRE_VERB (VERB "to prostrate oneself") (VERB_TRANSITIVE "to defeat; to beat, to vanquish, to conquer, to enslave") (ADJECTIVE "low; lower, bottom, down") (ADVERB "deeply; downstairs, below, deep, low") NO_NUMERAL (PREPOSITION "bottom; beneath") )
        , ( "insa", WORD "insa" LOC_KIND (NOUN "inside; inner world, centre, stomach") NO_PRE_VERB BE (VERB_TRANSITIVE "to put inside") (ADJECTIVE "inner; internal") (ADVERB "internally; innerly") NO_NUMERAL (PREPOSITION "inside") )
        , ( "monsi", WORD "monsi" LOC_KIND (NOUN "back; rear end, butt, behind") NO_PRE_VERB BE (VERB_TRANSITIVE "to put in the back") (ADJECTIVE "back; rear") (ADVERB "rearly") NO_NUMERAL (PREPOSITION "back; behind") )
        , ( "sama", WORD "sama" PREPOSITION_KIND (NOUN "equality; parity, equity, identity, par, sameness") NO_PRE_VERB (VERB "to be like; to be equal") (VERB_TRANSITIVE "to make equal; to equate, to make similar to") (ADJECTIVE "same; similar, equal, of equal status or position") (ADVERB "equally; just as, exactly the same, just the same, similarly") NO_NUMERAL (PREPOSITION "like; as, seem") )
        , ( "tan", WORD "tan" PREPOSITION_KIND (NOUN "reason; cause, origin") NO_PRE_VERB (VERB "to come from; originate from, come out of") (VERB_TRANSITIVE "to put in the origin") (ADJECTIVE "causal") (ADVERB "mobilely") NO_NUMERAL (PREPOSITION "because of; by, from, since") )
        , ( "poka", WORD "poka" PREPOSITION_KIND (NOUN "side; hip, next to") NO_PRE_VERB BE (VERB_TRANSITIVE "to put aside") (ADJECTIVE "neighbouring") (ADVERB "nearby") NO_NUMERAL (PREPOSITION "with; in the accompaniment of") )

        -- lesson #8
        -- ala! = no!
        , ( "ala", WORD "ala" NOUN_KIND (NOUN "nothing; negation, zero") NO_PRE_VERB (VERB "not to be") (VERB_TRANSITIVE "to deny") (ADJECTIVE "none; no, not, un-") (ADVERB "not; don't") (ADJECTIVE_NUMERAL 0) NO_PREP )
        , ( "ale", WORD "ale" NOUN_KIND (NOUN "everything; anything, life, the universe") NO_PRE_VERB BE (VERB_TRANSITIVE "to be everything") (ADJECTIVE "all; every, complete, whole") (ADVERB "always; forever, evermore, eternally") (ADJECTIVE_NUMERAL 100) NO_PREP )
        , ( "ali", WORD "ali" NOUN_KIND (NOUN "everything; anything, life, the universe") NO_PRE_VERB BE (VERB_TRANSITIVE "to be everything") (ADJECTIVE "all; every, complete, whole") (ADVERB "always; forever, evermore, eternally") (ADJECTIVE_NUMERAL 100) NO_PREP )

        -- ken la => maybe
        , ( "ken", WORD "ken" PRE_VERB_KIND (NOUN "possibility; ability, power to do things, permission") (PRE_VERB "can; may") (VERB "is able to; can, is allowed to, may, is possible") (VERB_TRANSITIVE "to make possible; to enable, to allow, to permit") (ADJECTIVE "possible") (ADVERB "possibly") NO_NUMERAL NO_PREP )
        , ( "lape", WORD "lape" VERB_KIND (NOUN "sleep; rest") NO_PRE_VERB (VERB "to sleep; to rest") (VERB_TRANSITIVE "to knock out; to put someone to sleep") (ADJECTIVE "sleeping; of sleep, dormant") (ADVERB "asleep") NO_NUMERAL NO_PREP )
        , ( "musi", WORD "musi" VERB_KIND (NOUN "fun; playing, game, recreation, art, entertainment") NO_PRE_VERB (VERB "to have|be fun; to play") (VERB_TRANSITIVE "to amuse; to entertain") (ADJECTIVE "funny; artful, recreational") (ADVERB "cheerfully") NO_NUMERAL NO_PREP )
        , ( "pali", WORD "pali" VERB_TRANSITIVE_KIND (NOUN "activity; work, deed, project") NO_PRE_VERB (VERB "to work; to act; to function") (VERB_TRANSITIVE "to do; to make, to build, to create") (ADJECTIVE "active; work-related, operating, working") (ADVERB "actively; briskly") NO_NUMERAL NO_PREP )
        , ( "sona", WORD "sona" VERB_TRANSITIVE_KIND (NOUN "knowledge; wisdom, intelligence, understanding") (PRE_VERB "to know how to") (VERB "to know; to understand") (VERB_TRANSITIVE "to know; to understand, to know how to") (ADJECTIVE "knowing; cognizant, shrewd") (ADVERB "knowing") NO_NUMERAL NO_PREP )
        , ( "wawa", WORD "wawa" NOUN_KIND (NOUN "energy; strength, power") NO_PRE_VERB (VERB "to be powerful") (VERB_TRANSITIVE "to energize; to strengthen, to empower") (ADJECTIVE "energetic; strong, fierce, intense, sure, confident") (ADVERB "strongly; powerfully") NO_NUMERAL NO_PREP )
        , ( "anu", PARTICLE "anu" "or" )

        -- lesson #9
        -- a a a! => laugh
        , ( "a", SIMPLE "a" "ah; ha, uh, oh, ooh, aw, well (emotion word)" )
        , ( "awen", WORD "awen" VERB_KIND (NOUN "inertia; continuity, continuum, stay") NO_PRE_VERB (VERB "to wait; to stay, to remain") (VERB_TRANSITIVE "to keep") (ADJECTIVE "remaining; stationary, permanent, sedentary") (ADVERB "still; yet") NO_NUMERAL NO_PREP )
        , ( "mama", WORD "mama" NOUN_KIND (NOUN "parent; mother, father") NO_PRE_VERB (VERB "to be a parent") (VERB_TRANSITIVE "to mother somebody; to wet-nurse, mothering") (ADJECTIVE "parental; of the parent, maternal, fatherly, motherly, mumsy") (ADVERB "parentally") NO_NUMERAL NO_PREP )
        , ( "mije", WORD "mije" NOUN_KIND (NOUN "man; male, husband, boyfriend") NO_PRE_VERB (VERB "to be a man; to be a male|husband|boyfriend") (VERB_TRANSITIVE "to transform somebody as a male") (ADJECTIVE "male; masculine, manly") (ADVERB "manfully") NO_NUMERAL NO_PREP )
        , ( "meli", WORD "meli" NOUN_KIND (NOUN "woman; female, girl, wife, girlfriend") NO_PRE_VERB (VERB "to be a woman; to be a female|girl|wife|girlfriend") (VERB_TRANSITIVE "to transform somebody as a female") (ADJECTIVE "female; feminine, womanly") (ADVERB "femininely") NO_NUMERAL NO_PREP )
        , ( "mu", WORD "mu" NOUN_KIND (NOUN "animal noise") NO_PRE_VERB (VERB "to do animal noise") NO_VERB_TRANSITIVE (ADJECTIVE "animal nois-") (ADVERB "animal nois-") NO_NUMERAL NO_PREP )
        , ( "nimi", WORD "nimi" NOUN_KIND (NOUN "name; word") NO_PRE_VERB (VERB "to be named") (VERB_TRANSITIVE "to name") (ADJECTIVE "named") NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "o", PARTICLE "o" "used for vocative and imperative" )
        ]



{-

   o! interjection: hey! (calling somebody’s attention)
   . . . o, . . . interjection: adressing people
   o . . . ! subject: An ”o” is used for imperative (commands). ”o” replace the subject.
   . . . o . . . ! separator : An ”o” is used for imperative (commands): ”o” replace ”li”.
   Don’t use ”o” before or after the other separators ”e”, ”li”, ”pi”.


-}


tokiponaLipu : Dict String WORD
tokiponaLipu =
    fromList
        [ ( "ike lukin", WORD "ike lukin" ADJECTIVE_KIND (NOUN "ugliness") NO_PRE_VERB (VERB "to be ugly") (VERB_TRANSITIVE "to make ugly") (ADJECTIVE "ugly") (ADVERB "basely") NO_NUMERAL NO_PREP )
        , ( "ilo moku", WORD "ilo moku" NOUN_KIND (NOUN "spoon|fork|knife") NO_PRE_VERB BE (VERB_TRANSITIVE "to make a spoon|fork|knife with") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "ilo suno", WORD "ilo suno" NOUN_KIND (NOUN "flashlight; lamp, light") NO_PRE_VERB BE (VERB_TRANSITIVE "to make a light with") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "jan ala", WORD "jan ala" NOUN_KIND (NOUN "nobody") NO_PRE_VERB (VERB "to be nobody") (VERB_TRANSITIVE "to make nobody with") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "jan ike", WORD "jan ike" NOUN_KIND (NOUN "enemy") NO_PRE_VERB (VERB "to be an ennemy") (VERB_TRANSITIVE "to make an enemy of") (ADJECTIVE "inimical") (ADVERB "hostilely") NO_NUMERAL NO_PREP )
        , ( "jan lawa", WORD "jan lawa" NOUN_KIND (NOUN "leader") NO_PRE_VERB (VERB "to be a leader") (VERB_TRANSITIVE "to make a leader of") (ADJECTIVE "leaderly") (ADVERB "leaderly") NO_NUMERAL NO_PREP )
        , ( "jan lili", WORD "jan lili" NOUN_KIND (NOUN "child") NO_PRE_VERB (VERB "to be a child; to be childly") (VERB_TRANSITIVE "to give birth") (ADJECTIVE "childly") (ADVERB "childly") NO_NUMERAL NO_PREP )
        , ( "jan pona", WORD "jan pona" NOUN_KIND (NOUN "friend") NO_PRE_VERB (VERB "to be a friend") (VERB_TRANSITIVE "to make a friend of") (ADJECTIVE "friendly") (ADVERB "friendly") NO_NUMERAL NO_PREP )
        , ( "jan sewi", WORD "jan sewi" NOUN_KIND (NOUN "clergyman; god") NO_PRE_VERB (VERB "to be a clergyman") (VERB_TRANSITIVE "to ordain as a clergyman") (ADJECTIVE "as a clergyman") (ADVERB "as a clergyman") NO_NUMERAL NO_PREP )
        , ( "jan suli", WORD "jan suli" NOUN_KIND (NOUN "adult") NO_PRE_VERB (VERB "to be an adult") (VERB_TRANSITIVE "to raise as an adult") (ADJECTIVE "adultly") (ADVERB "adultly") NO_NUMERAL NO_PREP )
        , ( "jan unpa", WORD "jan unpa" NOUN_KIND (NOUN "lover") NO_PRE_VERB (VERB "to be a lover") (VERB_TRANSITIVE "to make a lover of") (ADJECTIVE "as a lover") (ADVERB "as a lover") NO_NUMERAL NO_PREP )
        , ( "jan utala", WORD "jan utala" NOUN_KIND (NOUN "soldier; warrior") NO_PRE_VERB (VERB "to be a soldier") (VERB_TRANSITIVE "to make a soldier of") (ADJECTIVE "soldiery") (ADVERB "soldiery") NO_NUMERAL NO_PREP )
        , ( "jan sama", WORD "jan sama" NOUN_KIND (NOUN "sibling") NO_PRE_VERB (VERB "to be a siblings") (VERB_TRANSITIVE "to make a siblings of") (ADJECTIVE "fraternal") (ADVERB "fraternally") NO_NUMERAL NO_PREP )
        , ( "kama sona", WORD "kama sona" VERB_TRANSITIVE_KIND (NOUN "study") NO_PRE_VERB (VERB "to learn; to study") (VERB_TRANSITIVE "to learn; to study") (ADJECTIVE "studying") (ADVERB "studying") NO_NUMERAL NO_PREP )

        --, ( "ken la", WORD "ken la" CONDITIONAL_KIND AS_NOUN NO_PRE_VERB (VERB "to be underneath") (VERB_TRANSITIVE "to put underneath") ADJ (ADVERB "underneathly") NO_NUMERAL (PREPOSITION "underneath") )
        , ( "lon anpa", WORD "lon anpa" PRE_VERB_KIND AS_NOUN NO_PRE_VERB (VERB "to be underneath") (VERB_TRANSITIVE "to put underneath") ADJ (ADVERB "underneathly") NO_NUMERAL (PREPOSITION "underneath") )
        , ( "lon insa", WORD "lon insa" PRE_VERB_KIND AS_NOUN NO_PRE_VERB (VERB "to be inside") (VERB_TRANSITIVE "to put inside") ADJ (ADVERB "internally") NO_NUMERAL (PREPOSITION "in") )
        , ( "lon monsi", WORD "lon monsi" PRE_VERB_KIND AS_NOUN NO_PRE_VERB (VERB "to be behind") (VERB_TRANSITIVE "to put behind") ADJ (ADVERB "behindly") NO_NUMERAL (PREPOSITION "behind") )
        , ( "lon poka", WORD "lon poka" PRE_VERB_KIND AS_NOUN NO_PRE_VERB (VERB "to be nearby") (VERB_TRANSITIVE "to put aside") ADJ (ADVERB "nearby") NO_NUMERAL (PREPOSITION "beside; nearby, with") )
        , ( "lon sewi", WORD "lon sewi" PRE_VERB_KIND AS_NOUN NO_PRE_VERB (VERB "to be above") (VERB_TRANSITIVE "to put above") ADJ (ADVERB "above") NO_NUMERAL (PREPOSITION "above") )
        , ( "ma tomo", WORD "ma tomo" NOUN_KIND (NOUN "town; city") NO_PRE_VERB (VERB "to be a town") (VERB_TRANSITIVE "to make a town with") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "ma tomo lawa", SIMPLE "ma tomo lawa" "capital" )

        -- Towns
        , ( "ma tomo Solu", SIMPLE "ma tomo Solu" "Seoul" )
        , ( "ma tomo Asina", SIMPLE "ma tomo Asina" "Athens" )
        , ( "ma tomo Sakata", SIMPLE "ma tomo Sakata" "Jakarta" )
        , ( "ma tomo Telawi", SIMPLE "ma tomo Telawi" "Tel Aviv" )
        , ( "ma tomo Loma", SIMPLE "ma tomo Loma" "Rome" )
        , ( "ma tomo Milano", SIMPLE "ma tomo Milano" "Milan" )
        , ( "ma tomo Napoli", SIMPLE "ma tomo Napoli" "Naples" )
        , ( "ma tomo Pilense", SIMPLE "ma tomo Pilense" "Florence" )
        , ( "ma tomo Wenesija", SIMPLE "ma tomo Wenesija" "Venice" )
        , ( "ma tomo Alawa", SIMPLE "ma tomo Alawa" "Ottawa" )
        , ( "ma tomo Towano", SIMPLE "ma tomo Towano" "Toronto" )
        , ( "ma tomo Solu", SIMPLE "ma tomo Solu" "Seoul" )
        , ( "ma tomo Kakawi", SIMPLE "ma tomo Kakawi" "Calgary" )
        , ( "ma tomo Monkela", SIMPLE "ma tomo Monkela" "Montreal" )
        , ( "ma tomo Alipasi", SIMPLE "ma tomo Alipasi" "Halifax" )
        , ( "ma tomo Sensan", SIMPLE "ma tomo Sensan" "St. John’s" )
        , ( "ma tomo Manten", SIMPLE "ma tomo Manten" "Moncton" )
        , ( "ma tomo Sawi", SIMPLE "ma tomo Sawi" "Sackville" )
        , ( "ma tomo Sesija", SIMPLE "ma tomo Sesija" "Shediac" )
        , ( "ma tomo Sije", SIMPLE "ma tomo Sije" "Dieppe" )
        , ( "ma tomo Wankuwa", SIMPLE "ma tomo Wankuwa" "Vancouver" )
        , ( "ma tomo Paki", SIMPLE "ma tomo Paki" "Paris" )
        , ( "ma tomo Akajela", SIMPLE "ma tomo Akajela" "Cairo" )
        , ( "ma tomo Mesiko", SIMPLE "ma tomo Mesiko" "Mexico City" )
        , ( "ma tomo Ele", SIMPLE "ma tomo Ele" "Los Angeles" )
        , ( "ma tomo Sanpansiko", SIMPLE "ma tomo Sanpansiko" "San Francisco" )
        , ( "ma tomo Kenpisi", SIMPLE "ma tomo Kenpisi" "Cambridge" )
        , ( "ma tomo Pasen", SIMPLE "ma tomo Pasen" "Boston" )
        , ( "ma tomo Nujoka", SIMPLE "ma tomo Nujoka" "New York" )
        , ( "ma tomo Polan", SIMPLE "ma tomo Polan" "Portland" )
        , ( "ma tomo Alana", SIMPLE "ma tomo Alana" "Atlanta" )
        , ( "ma tomo Elena", SIMPLE "ma tomo Elena" "Atlanta" )
        , ( "ma tomo Putapesi", SIMPLE "ma tomo Putapesi" "Budapest" )
        , ( "ma tomo Ansetan", SIMPLE "ma tomo Ansetan" "Amsterdam" )
        , ( "ma tomo Iwesun", SIMPLE "ma tomo Iwesun" "Hilversum" )
        , ( "ma tomo Osaka", SIMPLE "ma tomo Osaka" "Osaka" )
        , ( "ma tomo Tokijo", SIMPLE "ma tomo Tokijo" "Tokyo" )
        , ( "ma tomo Lanten", SIMPLE "ma tomo Lanten" "London" )
        , ( "ma tomo Lantan", SIMPLE "ma tomo Lantan" "London" )
        , ( "ma tomo Peminan", SIMPLE "ma tomo Peminan" "Birmingham" )
        , ( "ma tomo Pesin", SIMPLE "ma tomo Pesin" "Beijing, Peking" )
        , ( "ma tomo Esupo", SIMPLE "ma tomo Esupo" "Espoo" )
        , ( "ma tomo Lesinki", SIMPLE "ma tomo Lesinki" "Helsinki" )
        , ( "ma tomo Tanpele", SIMPLE "ma tomo Tanpele" "Tampere" )
        , ( "ma tomo Tuku", SIMPLE "ma tomo Tuku" "Turku" )
        , ( "ma tomo Sene", SIMPLE "ma tomo Sene" "Geneva" )
        , ( "ma tomo Kunte", SIMPLE "ma tomo Kunte" "Bangkok" )
        , ( "ma tomo Anpu", SIMPLE "ma tomo Anpu" "Hamburg" )
        , ( "ma tomo Minsen", SIMPLE "ma tomo Minsen" "Munich" )
        , ( "ma tomo Pelin", SIMPLE "ma tomo Pelin" "Berlin" )
        , ( "Solu", FOREIGN "Solu" "Seoul" )
        , ( "Asina", FOREIGN "Asina" "Athens" )
        , ( "Sakata", FOREIGN "Sakata" "Jakarta" )
        , ( "Telawi", FOREIGN "Telawi" "Tel Aviv" )
        , ( "Loma", FOREIGN "Loma" "Rome" )
        , ( "Milano", FOREIGN "Milano" "Milan" )
        , ( "Napoli", FOREIGN "Napoli" "Naples" )
        , ( "Pilense", FOREIGN "Pilense" "Florence" )
        , ( "Wenesija", FOREIGN "Wenesija" "Venice" )
        , ( "Alawa", FOREIGN "Alawa" "Ottawa" )
        , ( "Towano", FOREIGN "Towano" "Toronto" )
        , ( "Solu", FOREIGN "Solu" "Seoul" )
        , ( "Kakawi", FOREIGN "Kakawi" "Calgary" )
        , ( "Monkela", FOREIGN "Monkela" "Montreal" )
        , ( "Alipasi", FOREIGN "Alipasi" "Halifax" )
        , ( "Sensan", FOREIGN "Sensan" "St. John’s" )
        , ( "Manten", FOREIGN "Manten" "Moncton" )
        , ( "Sawi", FOREIGN "Sawi" "Sackville" )
        , ( "Sesija", FOREIGN "Sesija" "Shediac" )
        , ( "Sije", FOREIGN "Sije" "Dieppe" )
        , ( "Wankuwa", FOREIGN "Wankuwa" "Vancouver" )
        , ( "Paki", FOREIGN "Paki" "Paris" )
        , ( "Akajela", FOREIGN "Akajela" "Cairo" )
        , ( "Mesiko", FOREIGN "Mesiko" "Mexico City" )
        , ( "Ele", FOREIGN "Ele" "Los Angeles" )
        , ( "Sanpansiko", FOREIGN "Sanpansiko" "San Francisco" )
        , ( "Kenpisi", FOREIGN "Kenpisi" "Cambridge" )
        , ( "Pasen", FOREIGN "Pasen" "Boston" )
        , ( "Nujoka", FOREIGN "Nujoka" "New York" )
        , ( "Polan", FOREIGN "Polan" "Portland" )
        , ( "Alana", FOREIGN "Alana" "Atlanta" )
        , ( "Putapesi", FOREIGN "Putapesi" "Budapest" )
        , ( "Ansetan", FOREIGN "Ansetan" "Amsterdam" )
        , ( "Iwesun", FOREIGN "Iwesun" "Hilversum" )
        , ( "Osaka", FOREIGN "Osaka" "Osaka" )
        , ( "Tokijo", FOREIGN "Tokijo" "Tokyo" )
        , ( "Lanten", FOREIGN "Lanten" "London" )
        , ( "Lantan", FOREIGN "Lantan" "London" )
        , ( "Peminan", FOREIGN "Peminan" "Birmingham" )
        , ( "Pesin", FOREIGN "Pesin" "Beijing, Peking" )
        , ( "Esupo", FOREIGN "Esupo" "Espoo" )
        , ( "Lesinki", FOREIGN "Lesinki" "Helsinki" )
        , ( "Tanpele", FOREIGN "Tanpele" "Tampere" )
        , ( "Tuku", FOREIGN "Tuku" "Turku" )
        , ( "Sene", FOREIGN "Sene" "Geneva" )
        , ( "Kunte", FOREIGN "Kunte" "Bangkok" )
        , ( "Anpu", FOREIGN "Anpu" "Hamburg" )
        , ( "Minsen", FOREIGN "Minsen" "Munich" )
        , ( "Pelin", FOREIGN "Pelin" "Berlin" )

        -- Other words
        , ( "ma telo", WORD "ma telo" NOUN_KIND (NOUN "lake; mud, swamp") NO_PRE_VERB (VERB "to be a lake") (VERB_TRANSITIVE "to transform as a lake, mud, swamp") ADJ NO_ADVERB NO_NUMERAL NO_PREP )

        -- Continents
        , ( "ma Amelika", SIMPLE "ma Amelika" "Americas" )
        , ( "ma Antasika", SIMPLE "ma Amelika" "Antarctica" )
        , ( "ma Apika", SIMPLE "ma Apika" "Africa" )
        , ( "ma Asija", SIMPLE "ma Asija" "Asia" )
        , ( "ma Elopa", SIMPLE "ma Elopa" "Europe" )
        , ( "ma Osejanija", SIMPLE "ma Osejanija" "Oceania" )
        , ( "Amelika", FOREIGN "Amelika" "Americas" )
        , ( "Antasika", FOREIGN "Amelika" "Antarctica" )
        , ( "Apika", FOREIGN "Apika" "Africa" )
        , ( "Asija", FOREIGN "Asija" "Asia" )
        , ( "Elopa", FOREIGN "Elopa" "Europe" )
        , ( "Osejanija", FOREIGN "Osejanija" "Oceania" )

        -- Africa
        , ( "ma Ankola", SIMPLE "ma Ankola" "Angola" )
        , ( "ma Eliteja", SIMPLE "ma Eliteja" "Eritrea" )
        , ( "ma Isijopija", SIMPLE "ma Isijopija" "Ethiopia" )
        , ( "ma Kamelun", SIMPLE "ma Kamelun" "Cameroon" )
        , ( "ma Kana", SIMPLE "ma Kana" "Ghana" )
        , ( "ma Kanpija", SIMPLE "ma Kanpija" "Gambia" )
        , ( "ma Kapon", SIMPLE "ma Kapon" "Gabon" )
        , ( "ma Kenja", SIMPLE "ma Kenja" "Kenya" )
        , ( "ma Kilipasi", SIMPLE "ma Kilipasi" "Kiribati" )
        , ( "ma Kine", SIMPLE "ma Kine" "Guinea" )
        , ( "ma Kinejekatolija", SIMPLE "ma Kinejekatolija" "Equatorial Guinea" )
        , ( "ma Kinepisa", SIMPLE "ma Kinepisa" "Guinea-Bissau" )
        , ( "ma Komo", SIMPLE "ma Komo" "Comoros" )
        , ( "ma Konko", SIMPLE "ma Konko" "Congo" )
        , ( "ma Kosiwa", SIMPLE "ma Kosiwa" "Côte d’Ivoire" )
        , ( "ma Lapewija", SIMPLE "ma Lapewija" "Liberia" )
        , ( "ma Lesoto", SIMPLE "ma Lesoto" "Lesotho" )
        , ( "ma Lipija", SIMPLE "ma Lipija" "Libya" )
        , ( "ma Luwanta", SIMPLE "ma Luwanta" "Rwanda" )
        , ( "ma Malakasi", SIMPLE "ma Malakasi" "Madagascar" )
        , ( "ma Malawi", SIMPLE "ma Malawi" "Malawi" )
        , ( "ma Mali", SIMPLE "ma Mali" "Mali" )
        , ( "ma Malipe", SIMPLE "ma Malipe" "Morocco" )
        , ( "ma Masu", SIMPLE "ma Masu" "Egypt" )
        , ( "ma Mosanpi", SIMPLE "ma Mosanpi" "Mozambique" )
        , ( "ma Mowisi", SIMPLE "ma Mowisi" "Mauritius" )
        , ( "ma Mulitanija", SIMPLE "ma Mulitanija" "Mauritania" )
        , ( "ma Namipija", SIMPLE "ma Namipija" "Namibia" )
        , ( "ma Naselija", SIMPLE "ma Naselija" "Nigeria" )
        , ( "ma Nise", SIMPLE "ma Nise" "Niger" )
        , ( "ma Penen", SIMPLE "ma Penen" "Benin" )
        , ( "ma Posuwana", SIMPLE "ma Posuwana" "Botswana" )
        , ( "ma Pukinapaso", SIMPLE "ma Pukinapaso" "Burkina Faso" )
        , ( "ma Sanpija", SIMPLE "ma Sanpija" "Zambia" )
        , ( "ma Santapiken", SIMPLE "ma Santapiken" "Central African Republic" )
        , ( "ma Sasali", SIMPLE "ma Sasali" "Algeria" )
        , ( "ma Sate", SIMPLE "ma Sate" "Chad" )
        , ( "ma Sawasi", SIMPLE "ma Sawasi" "Swaziland" )
        , ( "ma Seneka", SIMPLE "ma Seneka" "Senegal" )
        , ( "ma Setapika", SIMPLE "ma Setapika" "South Africa" )
        , ( "ma Sijelalijon", SIMPLE "ma Sijelalijon" "Sierra Leone" )
        , ( "ma Sinpapuwe", SIMPLE "ma Sinpapuwe" "Zimbabwe" )
        , ( "ma Sipusi", SIMPLE "ma Sipusi" "Djibouti" )
        , ( "ma Somalija", SIMPLE "ma Somalija" "Somalia" )
        , ( "ma Sutan", SIMPLE "ma Sutan" "Sudan" )
        , ( "ma Tansanija", SIMPLE "ma Tansanija" "Tanzania" )
        , ( "ma Toko", SIMPLE "ma Toko" "Togo" )
        , ( "ma Tunisi", SIMPLE "ma Tunisi" "Tunisia" )
        , ( "ma Ukanta", SIMPLE "ma Ukanta" "Uganda" )
        , ( "Ankola", FOREIGN "Ankola" "Angola" )
        , ( "Eliteja", FOREIGN "Eliteja" "Eritrea" )
        , ( "Isijopija", FOREIGN "Isijopija" "Ethiopia" )
        , ( "Kamelun", FOREIGN "Kamelun" "Cameroon" )
        , ( "Kana", FOREIGN "Kana" "Ghana" )
        , ( "Kanpija", FOREIGN "Kanpija" "Gambia" )
        , ( "Kapon", FOREIGN "Kapon" "Gabon" )
        , ( "Kenja", FOREIGN "Kenja" "Kenya" )
        , ( "Kilipasi", FOREIGN "Kilipasi" "Kiribati" )
        , ( "Kine", FOREIGN "Kine" "Guinea" )
        , ( "Kinejekatolija", FOREIGN "Kinejekatolija" "Equatorial Guinea" )
        , ( "Kinepisa", FOREIGN "Kinepisa" "Guinea-Bissau" )
        , ( "Komo", FOREIGN "Komo" "Comoros" )
        , ( "Konko", FOREIGN "Konko" "Congo" )
        , ( "Kosiwa", FOREIGN "Kosiwa" "Côte d’Ivoire" )
        , ( "Lapewija", FOREIGN "Lapewija" "Liberia" )
        , ( "Lesoto", FOREIGN "Lesoto" "Lesotho" )
        , ( "Lipija", FOREIGN "Lipija" "Libya" )
        , ( "Luwanta", FOREIGN "Luwanta" "Rwanda" )
        , ( "Malakasi", FOREIGN "Malakasi" "Madagascar" )
        , ( "Malawi", FOREIGN "Malawi" "Malawi" )
        , ( "Mali", FOREIGN "Mali" "Mali" )
        , ( "Malipe", FOREIGN "Malipe" "Morocco" )
        , ( "Masu", FOREIGN "Masu" "Egypt" )
        , ( "Mosanpi", FOREIGN "Mosanpi" "Mozambique" )
        , ( "Mowisi", FOREIGN "Mowisi" "Mauritius" )
        , ( "Mulitanija", FOREIGN "Mulitanija" "Mauritania" )
        , ( "Namipija", FOREIGN "Namipija" "Namibia" )
        , ( "Naselija", FOREIGN "Naselija" "Nigeria" )
        , ( "Nise", FOREIGN "Nise" "Niger" )
        , ( "Penen", FOREIGN "Penen" "Benin" )
        , ( "Posuwana", FOREIGN "Posuwana" "Botswana" )
        , ( "Pukinapaso", FOREIGN "Pukinapaso" "Burkina Faso" )
        , ( "Sanpija", FOREIGN "Sanpija" "Zambia" )
        , ( "Santapiken", FOREIGN "Santapiken" "Central African Republic" )
        , ( "Sasali", FOREIGN "Sasali" "Algeria" )
        , ( "Sate", FOREIGN "Sate" "Chad" )
        , ( "Sawasi", FOREIGN "Sawasi" "Swaziland" )
        , ( "Seneka", FOREIGN "Seneka" "Senegal" )
        , ( "Setapika", FOREIGN "Setapika" "South Africa" )
        , ( "Sijelalijon", FOREIGN "Sijelalijon" "Sierra Leone" )
        , ( "Sinpapuwe", FOREIGN "Sinpapuwe" "Zimbabwe" )
        , ( "Sipusi", FOREIGN "Sipusi" "Djibouti" )
        , ( "Somalija", FOREIGN "Somalija" "Somalia" )
        , ( "Sutan", FOREIGN "Sutan" "Sudan" )
        , ( "Tansanija", FOREIGN "Tansanija" "Tanzania" )
        , ( "Toko", FOREIGN "Toko" "Togo" )
        , ( "Tunisi", FOREIGN "Tunisi" "Tunisia" )
        , ( "Ukanta", FOREIGN "Ukanta" "Uganda" )

        -- Americas
        , ( "ma Alensina", SIMPLE "ma Alensina" "Argentina" )
        , ( "ma Awisi", SIMPLE "ma Awisi" "Haiti" )
        , ( "ma Ekato", SIMPLE "ma Ekato" "Ecuador" )

        --, ( "ma Kalalinuna", SIMPLE "ma Kalalinuna" "Greenland" )
        , ( "ma Kanata", SIMPLE "ma Kanata" "Canada" )
        , ( "ma Katemala", SIMPLE "ma Katemala" "Guatemala" )
        , ( "ma Kenata", SIMPLE "ma Kenata" "Grenada" )
        , ( "ma Kosalika", SIMPLE "ma Kosalika" "Costa Rica" )
        , ( "ma Kupa", SIMPLE "ma Kupa" "Cuba" )
        , ( "ma Mesiko", SIMPLE "ma Mesiko" "Mexico" )
        , ( "ma Mewika", SIMPLE "ma Mewika" "United States" )
        , ( "ma Ontula", SIMPLE "ma Ontula" "Honduras" )
        , ( "ma Palakawi", SIMPLE "ma Palakawi" "Paraguay" )
        , ( "ma Panama", SIMPLE "ma Panama" "Panama" )
        , ( "ma Papeto", SIMPLE "ma Papeto" "Barbados" )
        , ( "ma Pasila", SIMPLE "ma Pasila" "Brasil" )
        , ( "ma Pawama", SIMPLE "ma Pawama" "Bahamas" )
        , ( "ma Pelu", SIMPLE "ma Pelu" "Peru" )
        , ( "ma Pemuta", SIMPLE "ma Pemuta" "Bermuda" )
        , ( "ma Penesuwela", SIMPLE "ma Penesuwela" "Venezuela" )
        , ( "ma Sameka", SIMPLE "ma Sameka" "Jamaica" )
        , ( "ma Sile", SIMPLE "ma Sile" "Chile" )
        , ( "ma Sinita", SIMPLE "ma Sinita" "Trinidad and Tobago" )
        , ( "ma Tominika", SIMPLE "ma Tominika" "Dominican Republic" )
        , ( "ma Ulukawi", SIMPLE "ma Ulukawi" "Uruguay" )
        , ( "Alensina", FOREIGN "Alensina" "Argentina" )
        , ( "Awisi", FOREIGN "Awisi" "Haiti" )
        , ( "Ekato", FOREIGN "Ekato" "Ecuador" )

        --, ( "Kalalinuna", FOREIGN "Kalalinuna" "Greenland" )
        , ( "Kanata", FOREIGN "Kanata" "Canada" )
        , ( "Katemala", FOREIGN "Katemala" "Guatemala" )
        , ( "Kenata", FOREIGN "Kenata" "Grenada" )
        , ( "Kosalika", FOREIGN "Kosalika" "Costa Rica" )
        , ( "Kupa", FOREIGN "Kupa" "Cuba" )
        , ( "Mesiko", FOREIGN "Mesiko" "Mexico" )
        , ( "Mewika", FOREIGN "Mewika" "United States" )
        , ( "Ontula", FOREIGN "Ontula" "Honduras" )
        , ( "Palakawi", FOREIGN "Palakawi" "Paraguay" )
        , ( "Panama", FOREIGN "Panama" "Panama" )
        , ( "Papeto", FOREIGN "Papeto" "Barbados" )
        , ( "Pasila", FOREIGN "Pasila" "Brasil" )
        , ( "Pawama", FOREIGN "Pawama" "Bahamas" )
        , ( "Pelu", FOREIGN "Pelu" "Peru" )
        , ( "Pemuta", FOREIGN "Pemuta" "Bermuda" )
        , ( "Penesuwela", FOREIGN "Penesuwela" "Venezuela" )
        , ( "Sameka", FOREIGN "Sameka" "Jamaica" )
        , ( "Sile", FOREIGN "Sile" "Chile" )
        , ( "Sinita", FOREIGN "Sinita" "Trinidad and Tobago" )
        , ( "Tominika", FOREIGN "Tominika" "Dominican Republic" )
        , ( "Ulukawi", FOREIGN "Ulukawi" "Uruguay" )

        -- Asia
        , ( "ma Aja", SIMPLE "ma Aja" "Armenia" )
        , ( "ma Akanisan", SIMPLE "ma Akanisan" "Afghanistan" )
        , ( "ma Anku", SIMPLE "ma Anku" "South Korea" )
        , ( "ma Ilakija", SIMPLE "ma Ilakija" "Iraq" )
        , ( "ma Ilan", SIMPLE "ma Ilan" "Iran" )
        , ( "ma Intonesija", SIMPLE "ma Intonesija" "Indonesia" )
        , ( "ma Isale", SIMPLE "ma Isale" "Israel" )
        , ( "ma Jamanija", SIMPLE "ma Jamanija" "Yemen" )
        , ( "ma Kanpusi", SIMPLE "ma Kanpusi" "Cambodia" )
        , ( "ma Katelo", SIMPLE "ma Katelo" "Georgia" )
        , ( "ma Kuli", SIMPLE "ma Kuli" "Kurdistan" )
        , ( "ma Kusala", SIMPLE "ma Kusala" "Gujarat" )
        , ( "ma Kuwasi", SIMPLE "ma Kuwasi" "Kuwait" )
        , ( "ma Lanka", SIMPLE "ma Lanka" "Sri Lanka" )
        , ( "ma Losi", SIMPLE "ma Losi" "Russia" )
        , ( "ma Lunpan", SIMPLE "ma Lunpan" "Lebanon" )
        , ( "ma Malasija", SIMPLE "ma Malasija" "Malaysia" )
        , ( "ma Masu", SIMPLE "ma Masu" "Egypt" )
        , ( "ma Mijama", SIMPLE "ma Mijama" "Myanmar" )
        , ( "ma Nijon", SIMPLE "ma Nijon" "Japan" )
        , ( "ma Pakisan", SIMPLE "ma Pakisan" "Pakistan" )
        , ( "ma Palani", SIMPLE "ma Palani" "Bahrain" )
        , ( "ma Palata", SIMPLE "ma Palata" "India" )
        , ( "ma Panla", SIMPLE "ma Panla" "Bangladesh" )
        , ( "ma Pilipina", SIMPLE "ma Pilipina" "Philippines" )
        , ( "ma Pilisin", SIMPLE "ma Pilisin" "Palestine" )
        , ( "ma Po", SIMPLE "ma Po" "Tibet" )
        , ( "ma Sawusi", SIMPLE "ma Sawusi" "Saudi Arabia" )
        , ( "ma Sonko", SIMPLE "ma Sonko" "China" )
        , ( "ma Sulija", SIMPLE "ma Sulija" "Syria" )
        , ( "ma Tawi", SIMPLE "ma Tawi" "Thailand" )
        , ( "ma Tuki", SIMPLE "ma Tuki" "Turkey" )
        , ( "ma Uman", SIMPLE "ma Uman" "Oman" )
        , ( "ma Utun", SIMPLE "ma Utun" "Jordan" )
        , ( "ma Wije", SIMPLE "ma Wije" "Vietnam" )
        , ( "Aja", FOREIGN "Aja" "Armenia" )
        , ( "Akanisan", FOREIGN "Akanisan" "Afghanistan" )
        , ( "Anku", FOREIGN "Anku" "South Korea" )
        , ( "Ilakija", FOREIGN "Ilakija" "Iraq" )
        , ( "Ilan", FOREIGN "Ilan" "Iran" )
        , ( "Intonesija", FOREIGN "Intonesija" "Indonesia" )
        , ( "Isale", FOREIGN "Isale" "Israel" )
        , ( "Jamanija", FOREIGN "Jamanija" "Yemen" )
        , ( "Kanpusi", FOREIGN "Kanpusi" "Cambodia" )
        , ( "Katelo", FOREIGN "Katelo" "Georgia" )
        , ( "Kuli", FOREIGN "Kuli" "Kurdistan" )
        , ( "Kusala", FOREIGN "Kusala" "Gujarat" )
        , ( "Kuwasi", FOREIGN "Kuwasi" "Kuwait" )
        , ( "Lanka", FOREIGN "Lanka" "Sri Lanka" )
        , ( "Losi", FOREIGN "Losi" "Russia" )
        , ( "Lunpan", FOREIGN "Lunpan" "Lebanon" )
        , ( "Malasija", FOREIGN "Malasija" "Malaysia" )
        , ( "Masu", FOREIGN "Masu" "Egypt" )
        , ( "Mijama", FOREIGN "Mijama" "Myanmar" )
        , ( "Nijon", FOREIGN "Nijon" "Japan" )
        , ( "Pakisan", FOREIGN "Pakisan" "Pakistan" )
        , ( "Palani", FOREIGN "Palani" "Bahrain" )
        , ( "Palata", FOREIGN "Palata" "India" )
        , ( "Panla", FOREIGN "Panla" "Bangladesh" )
        , ( "Pilipina", FOREIGN "Pilipina" "Philippines" )
        , ( "Pilisin", FOREIGN "Pilisin" "Palestine" )
        , ( "Po", FOREIGN "Po" "Tibet" )
        , ( "Sawusi", FOREIGN "Sawusi" "Saudi Arabia" )
        , ( "Sonko", FOREIGN "Sonko" "China" )
        , ( "Sulija", FOREIGN "Sulija" "Syria" )
        , ( "Tawi", FOREIGN "Tawi" "Thailand" )
        , ( "Tuki", FOREIGN "Tuki" "Turkey" )
        , ( "Uman", FOREIGN "Uman" "Oman" )
        , ( "Utun", FOREIGN "Utun" "Jordan" )
        , ( "Wije", FOREIGN "Wije" "Vietnam" )

        -- Europe
        , ( "ma Alan", SIMPLE "ma Alan" "Ireland" )
        , ( "ma Antola", SIMPLE "ma Antola" "Andorra" )
        , ( "ma Elena", SIMPLE "ma Elena" "Greece" )
        , ( "ma Epanja", SIMPLE "ma Epanja" "Spain" )
        , ( "ma Esalasi", SIMPLE "ma Esalasi" "Austria" )
        , ( "ma Esi", SIMPLE "ma Esi" "Estonia" )
        , ( "ma Esuka", SIMPLE "ma Esuka" "Basque Country" )
        , ( "ma Inli", SIMPLE "ma Inli" "England" )
        , ( "ma Isilan", SIMPLE "ma Isilan" "Iceland" )
        , ( "ma Italija", SIMPLE "ma Italija" "Italy" )
        , ( "ma Juke", SIMPLE "ma Juke" "United Kingdom" )
        , ( "ma Kalalinuna", SIMPLE "ma Kalalinuna" "Greenland" )
        , ( "ma Kanse", SIMPLE "ma Kanse" "France" )
        , ( "ma Katala", SIMPLE "ma Katala" "Catalan Countries" )
        , ( "ma Katelo", SIMPLE "ma Katelo" "Georgia" )
        , ( "ma Kinla", SIMPLE "ma Kinla" "Wales" )
        , ( "ma Kiposi", SIMPLE "ma Kiposi" "Cyprus" )
        , ( "ma Lawi", SIMPLE "ma Lawi" "Latvia" )
        , ( "ma Lijatuwa", SIMPLE "ma Lijatuwa" "Lithuania" )
        , ( "ma Lisensan", SIMPLE "ma Lisensan" "Liechtenstein" )
        , ( "ma Lomani", SIMPLE "ma Lomani" "Romania" )
        , ( "ma Lowasi", SIMPLE "ma Lowasi" "Croatia" )
        , ( "ma Lowenki", SIMPLE "ma Lowenki" "Slovakia" )
        , ( "ma Lowensina", SIMPLE "ma Lowensina" "Slovenia" )
        , ( "ma Lusepu", SIMPLE "ma Lusepu" "Luxembourg" )
        , ( "ma Maketonija", SIMPLE "ma Maketonija" "Macedonia" )
        , ( "ma Mosijo", SIMPLE "ma Mosijo" "Hungary" )
        , ( "ma Motowa", SIMPLE "ma Motowa" "Moldova" )
        , ( "ma Netelan", SIMPLE "ma Netelan" "Netherlands" )
        , ( "ma Nosiki", SIMPLE "ma Nosiki" "Norway" )
        , ( "ma Pelalusi", SIMPLE "ma Pelalusi" "Belarus" )
        , ( "ma Pesije", SIMPLE "ma Pesije" "Belgium" )
        , ( "ma Peson", SIMPLE "ma Peson" "Brittany" )
        , ( "ma Pokasi", SIMPLE "ma Pokasi" "Bulgaria" )
        , ( "ma Posan", SIMPLE "ma Posan" "Bosnia" )
        , ( "ma Posuka", SIMPLE "ma Posuka" "Poland" )
        , ( "ma Potuke", SIMPLE "ma Potuke" "Portugal" )
        , ( "ma Samalino", SIMPLE "ma Samalino" "San Marino" )
        , ( "ma Seki", SIMPLE "ma Seki" "Czech" )
        , ( "ma Sipe", SIMPLE "ma Sipe" "Albania" )
        , ( "ma Sopisi", SIMPLE "ma Sopisi" "Serbia" )
        , ( "ma Sukosi", SIMPLE "ma Sukosi" "Scotland" )
        , ( "ma Sumi", SIMPLE "ma Sumi" "Finland" )
        , ( "ma Suwasi", SIMPLE "ma Suwasi" "Switzerland" )
        , ( "ma Tansi", SIMPLE "ma Tansi" "Denmark" )
        , ( "ma Tosi", SIMPLE "ma Tosi" "Germany" )
        , ( "ma Ukawina", SIMPLE "ma Ukawina" "Ukraine" )
        , ( "ma Wasikano", SIMPLE "ma Wasikano" "Vatican" )
        , ( "ma Wensa", SIMPLE "ma Wensa" "Wensa" )
        , ( "Alan", FOREIGN "Alan" "Ireland" )
        , ( "Antola", FOREIGN "Antola" "Andorra" )
        , ( "Elena", FOREIGN "Elena" "Greece" )
        , ( "Epanja", FOREIGN "Epanja" "Spain" )
        , ( "Esalasi", FOREIGN "Esalasi" "Austria" )
        , ( "Esi", FOREIGN "Esi" "Estonia" )
        , ( "Esuka", FOREIGN "Esuka" "Basque Country" )
        , ( "Inli", FOREIGN "Inli" "England" )
        , ( "Isilan", FOREIGN "Isilan" "Iceland" )
        , ( "Italija", FOREIGN "Italija" "Italy" )
        , ( "Juke", FOREIGN "Juke" "United Kingdom" )
        , ( "Kalalinuna", FOREIGN "Kalalinuna" "Greenland" )
        , ( "Kanse", FOREIGN "Kanse" "France" )
        , ( "Katala", FOREIGN "Katala" "Catalan Countries" )
        , ( "Katelo", FOREIGN "Katelo" "Georgia" )
        , ( "Kinla", FOREIGN "Kinla" "Wales" )
        , ( "Kiposi", FOREIGN "Kiposi" "Cyprus" )
        , ( "Lawi", FOREIGN "Lawi" "Latvia" )
        , ( "Lijatuwa", FOREIGN "Lijatuwa" "Lithuania" )
        , ( "Lisensan", FOREIGN "Lisensan" "Liechtenstein" )
        , ( "Lomani", FOREIGN "Lomani" "Romania" )
        , ( "Lowasi", FOREIGN "Lowasi" "Croatia" )
        , ( "Lowenki", FOREIGN "Lowenki" "Slovakia" )
        , ( "Lowensina", FOREIGN "Lowensina" "Slovenia" )
        , ( "Lusepu", FOREIGN "Lusepu" "Luxembourg" )
        , ( "Maketonija", FOREIGN "Maketonija" "Macedonia" )
        , ( "Mosijo", FOREIGN "Mosijo" "Hungary" )
        , ( "Motowa", FOREIGN "Motowa" "Moldova" )
        , ( "Netelan", FOREIGN "Netelan" "Netherlands" )
        , ( "Nosiki", FOREIGN "Nosiki" "Norway" )
        , ( "Pelalusi", FOREIGN "Pelalusi" "Belarus" )
        , ( "Pesije", FOREIGN "Pesije" "Belgium" )
        , ( "Peson", FOREIGN "Peson" "Brittany" )
        , ( "Pokasi", FOREIGN "Pokasi" "Bulgaria" )
        , ( "Posan", FOREIGN "Posan" "Bosnia" )
        , ( "Posuka", FOREIGN "Posuka" "Poland" )
        , ( "Potuke", FOREIGN "Potuke" "Portugal" )
        , ( "Samalino", FOREIGN "Samalino" "San Marino" )
        , ( "Seki", FOREIGN "Seki" "Czech" )
        , ( "Sipe", FOREIGN "Sipe" "Albania" )
        , ( "Sopisi", FOREIGN "Sopisi" "Serbia" )
        , ( "Sukosi", FOREIGN "Sukosi" "Scotland" )
        , ( "Sumi", FOREIGN "Sumi" "Finland" )
        , ( "Suwasi", FOREIGN "Suwasi" "Switzerland" )
        , ( "Tansi", FOREIGN "Tansi" "Denmark" )
        , ( "Tosi", FOREIGN "Tosi" "Germany" )
        , ( "Ukawina", FOREIGN "Ukawina" "Ukraine" )
        , ( "Wasikano", FOREIGN "Wasikano" "Vatican" )
        , ( "Wensa", FOREIGN "Wensa" "Wensa" )

        -- Oceania
        , ( "ma Intonesija", SIMPLE "ma Intonesija" "Indonesia" )
        , ( "ma Nusilan", SIMPLE "ma Nusilan" "New Zealand" )
        , ( "ma Oselija", SIMPLE "ma Oselija" "Australia" )
        , ( "ma Papuwanijukini", SIMPLE "ma Papuwanijukini" "Papua New Guinea" )
        , ( "ma Pisi", SIMPLE "ma Pisi" "Fiji" )
        , ( "ma Samowa", SIMPLE "ma Samowa" "Samoa" )
        , ( "ma Tona", SIMPLE "ma Tona" "Tonga" )
        , ( "ma Tuwalu", SIMPLE "ma Tuwalu" "Tuvalu" )
        , ( "ma Wanuwatu", SIMPLE "ma Wanuwatu" "Vanuatu" )
        , ( "Intonesija", FOREIGN "Intonesija" "Indonesia" )
        , ( "Nusilan", FOREIGN "Nusilan" "New Zealand" )
        , ( "Oselija", FOREIGN "Oselija" "Australia" )
        , ( "Papuwanijukini", FOREIGN "Papuwanijukini" "Papua New Guinea" )
        , ( "Pisi", FOREIGN "Pisi" "Fiji" )
        , ( "Samowa", FOREIGN "Samowa" "Samoa" )
        , ( "Tona", FOREIGN "Tona" "Tonga" )
        , ( "Tuwalu", FOREIGN "Tuwalu" "Tuvalu" )
        , ( "Wanuwatu", FOREIGN "Wanuwatu" "Vanuatu" )

        -- Other words
        , ( "mama meli", SIMPLE "mama meli" "mother" )
        , ( "mama mije", SIMPLE "mama mije" "father" )
        , ( "mi mute", WORD "mi mute" NOUN_KIND (NOUN "we") NO_PRE_VERB (VERB "to be us") NO_VERB_TRANSITIVE (ADJECTIVE "our") (ADVERB "us") NO_NUMERAL NO_PREP )
        , ( "moku lili", WORD "moku lili" NOUN_KIND (NOUN "nibbling") NO_PRE_VERB (VERB "to nibble") (VERB_TRANSITIVE "to nibble") (ADJECTIVE "nibbled") (ADVERB "nibbling") NO_NUMERAL NO_PREP )
        , ( "ona mute", WORD "ona mute" NOUN_KIND (NOUN "they") NO_PRE_VERB (VERB "to be them") NO_VERB_TRANSITIVE (ADJECTIVE "their") (ADVERB "them") NO_NUMERAL NO_PREP )
        , ( "poki moku", WORD "poki moku" ADJECTIVE_KIND (NOUN "bowl|cup") NO_PRE_VERB (VERB "to be a bow|cup") (VERB_TRANSITIVE "to make a bow|cup with") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "pona lukin", WORD "pona lukin" ADJECTIVE_KIND (NOUN "beauty") NO_PRE_VERB (VERB "to be beautiful") (VERB_TRANSITIVE "to make beautyful") (ADJECTIVE "beautyful") (ADVERB "beautifully") NO_NUMERAL NO_PREP )
        , ( "sewi kiwen", WORD "sewi kiwen" NOUN_KIND (NOUN "top of the rock") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as a top of the rock") ADJ ADV NO_NUMERAL NO_PREP )
        , ( "telo kili", WORD "telo kili" NOUN_KIND (NOUN "fruit juice") NO_PRE_VERB (VERB "to be a fruit juice") (VERB_TRANSITIVE "to transform as a fruit juice") (ADJECTIVE "fruity") (ADVERB "fruity") NO_NUMERAL NO_PREP )
        , ( "telo nasa", WORD "telo nasa" NOUN_KIND (NOUN "alcohol; beer, wine") NO_PRE_VERB (VERB "to be an alcohol") (VERB_TRANSITIVE "to transform as an alcohol") (ADJECTIVE "alcoholic") (ADVERB "alcoholic") NO_NUMERAL NO_PREP )
        , ( "telo suli", WORD "telo suli" NOUN_KIND (NOUN "ocean") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as an ocean") ADJ ADV NO_NUMERAL NO_PREP )
        , ( "toki luka", SIMPLE "toki luka" "Sign Language" )

        -- Language
        , ( "toki Alapi", SIMPLE "toki Alapi" "Arabic" )
        , ( "toki Apikan", SIMPLE "toki Apikan" "Afrikaans" )
        , ( "toki Awasa", SIMPLE "toki Awasa" "Hausa" )
        , ( "toki Awisi", SIMPLE "toki Awisi" "Haitian Creole" )
        , ( "toki Elena", SIMPLE "toki Elena" "Greek" )
        , ( "toki Epanja", SIMPLE "toki Epanja" "Spanish" )
        , ( "toki Esi", SIMPLE "toki Esi" "Estonian" )
        , ( "toki Esuka", SIMPLE "toki Esuka" "Basque" )
        , ( "toki Inli", SIMPLE "toki Inli" "English" )
        , ( "toki Insi", SIMPLE "toki Insi" "Hindi" )
        , ( "toki Intonesija", SIMPLE "toki Intonesija" "Indonesian" )
        , ( "toki Inu", SIMPLE "toki Inu" "Inuit" )
        , ( "toki Ipo", SIMPLE "toki Ipo" "Igbo" )
        , ( "toki Isilan", SIMPLE "toki Isilan" "Icelandic" )
        , ( "toki Italija", SIMPLE "toki Italija" "Italian" )
        , ( "toki Iwisi", SIMPLE "toki Iwisi" "Hebrew" )
        , ( "toki Jolupa", SIMPLE "toki Jolupa" "Yoruba" )
        , ( "toki Kalike", SIMPLE "toki Kalike" "Scottish Gaelic" )
        , ( "toki Kanse", SIMPLE "toki Kanse" "French" )
        , ( "toki Kantun", SIMPLE "toki Kantun" "Cantonese" )
        , ( "toki Kinla", SIMPLE "toki Kinla" "Welsh" )
        , ( "toki Lasina", SIMPLE "toki Lasina" "Latin" )
        , ( "toki Lomani", SIMPLE "toki Lomani" "Romanian" )
        , ( "toki Losi", SIMPLE "toki Losi" "Russian" )
        , ( "toki Lowasi", SIMPLE "toki Lowasi" "Croatian" )
        , ( "toki Mosijo", SIMPLE "toki Mosijo" "Hungarian" )
        , ( "toki Netelan", SIMPLE "toki Netelan" "Dutch" )
        , ( "toki Nijon", SIMPLE "toki Nijon" "Japanese" )
        , ( "toki Nosiki", SIMPLE "toki Nosiki" "Norwegian (Bokmål)" )
        , ( "toki Nosiki sin", SIMPLE "toki Iwisi" "Norwegian Nynorsk" )
        , ( "toki Panla", SIMPLE "toki Panla" "Bengali" )
        , ( "toki Peson", SIMPLE "toki Peson" "Breton" )
        , ( "toki Pokasi", SIMPLE "toki Pokasi" "Bulgarian" )
        , ( "toki Posan", SIMPLE "toki Posan" "Bosnian" )
        , ( "toki Potuke", SIMPLE "toki Potuke" "Portuguese" )
        , ( "toki Sameka", SIMPLE "toki Sameka" "Jamaican" )
        , ( "toki Seki", SIMPLE "toki Seki" "Czech" )
        , ( "toki Sesi", SIMPLE "toki Sesi" "Tsez" )
        , ( "toki Sikipe", SIMPLE "toki Sikipe" "Albanian" )
        , ( "toki Sonko", SIMPLE "toki Sonko" "Chinese" )
        , ( "toki Sopisi", SIMPLE "toki Sopisi" "Serbian" )
        , ( "toki Sumi", SIMPLE "toki Sumi" "Finnish" )
        , ( "toki Tansi", SIMPLE "toki Tansi" "Danish" )
        , ( "toki Topisin", SIMPLE "toki Topisin" "Tok Pisin" )
        , ( "toki Tosi", SIMPLE "toki Tosi" "German" )
        , ( "Alapi", FOREIGN "Alapi" "Arabic" )
        , ( "Apikan", FOREIGN "Apikan" "Afrikaans" )
        , ( "Awasa", FOREIGN "Awasa" "Hausa" )
        , ( "Awisi", FOREIGN "Awisi" "Haitian Creole" )
        , ( "Elena", FOREIGN "Elena" "Greek" )
        , ( "Epanja", FOREIGN "Epanja" "Spanish" )
        , ( "Esi", FOREIGN "Esi" "Estonian" )
        , ( "Esuka", FOREIGN "Esuka" "Basque" )
        , ( "Inli", FOREIGN "Inli" "English" )
        , ( "Insi", FOREIGN "Insi" "Hindi" )
        , ( "Intonesija", FOREIGN "Intonesija" "Indonesian" )
        , ( "Inu", FOREIGN "Inu" "Inuit" )
        , ( "Ipo", FOREIGN "Ipo" "Igbo" )
        , ( "Isilan", FOREIGN "Isilan" "Icelandic" )
        , ( "Italija", FOREIGN "Italija" "Italian" )
        , ( "Iwisi", FOREIGN "Iwisi" "Hebrew" )
        , ( "Jolupa", FOREIGN "Jolupa" "Yoruba" )
        , ( "Kalike", FOREIGN "Kalike" "Scottish Gaelic" )
        , ( "Kanse", FOREIGN "Kanse" "French" )
        , ( "Kantun", FOREIGN "Kantun" "Cantonese" )
        , ( "Kinla", FOREIGN "Kinla" "Welsh" )
        , ( "Lasina", FOREIGN "Lasina" "Latin" )
        , ( "Lomani", FOREIGN "Lomani" "Romanian" )
        , ( "Losi", FOREIGN "Losi" "Russian" )
        , ( "Lowasi", FOREIGN "Lowasi" "Croatian" )
        , ( "Mosijo", FOREIGN "Mosijo" "Hungarian" )
        , ( "Netelan", FOREIGN "Netelan" "Dutch" )
        , ( "Nijon", FOREIGN "Nijon" "Japanese" )
        , ( "Nosiki", FOREIGN "Nosiki" "Norwegian (Bokmål)" )
        , ( "Nosiki sin", FOREIGN "Iwisi" "Norwegian Nynorsk" )
        , ( "Panla", FOREIGN "Panla" "Bengali" )
        , ( "Peson", FOREIGN "Peson" "Breton" )
        , ( "Pokasi", FOREIGN "Pokasi" "Bulgarian" )
        , ( "Posan", FOREIGN "Posan" "Bosnian" )
        , ( "Potuke", FOREIGN "Potuke" "Portuguese" )
        , ( "Sameka", FOREIGN "Sameka" "Jamaican" )
        , ( "Seki", FOREIGN "Seki" "Czech" )
        , ( "Sesi", FOREIGN "Sesi" "Tsez" )
        , ( "Sikipe", FOREIGN "Sikipe" "Albanian" )
        , ( "Sonko", FOREIGN "Sonko" "Chinese" )
        , ( "Sopisi", FOREIGN "Sopisi" "Serbian" )
        , ( "Sumi", FOREIGN "Sumi" "Finnish" )
        , ( "Tansi", FOREIGN "Tansi" "Danish" )
        , ( "Topisin", FOREIGN "Topisin" "Tok Pisin" )
        , ( "Tosi", FOREIGN "Tosi" "German" )

        -- Constructed languages
        , ( "toki Apiwili", SIMPLE "toki Apiwili" "Afrihili" )
        , ( "toki Epelanto", SIMPLE "toki Epelanto" "Esperanto" )
        , ( "toki Inli pona", SIMPLE "toki Inli pona" "Basic English" )
        , ( "toki Inota", SIMPLE "toki Inota" "Lingua Ignota" )
        , ( "toki Intelinwa", SIMPLE "toki Intelinwa" "Interlingua" )
        , ( "toki Ito", SIMPLE "toki Ito" "Ido" )
        , ( "toki Kuwenja", SIMPLE "toki Kuwenja" "Quenya" )
        , ( "toki Latan", SIMPLE "toki Latan" "Láadan" )
        , ( "toki Losupan", SIMPLE "toki Losupan" "Lojban" )
        , ( "toki Mansi", SIMPLE "toki Mansi" "Mänti" )
        , ( "toki Nawi", SIMPLE "toki Nawi" "Na’vi" )
        , ( "toki Olapi", SIMPLE "toki Olapi" "Volapük" )
        , ( "toki Palepelen", SIMPLE "toki Palepelen" "Baôleybelen" )
        , ( "toki Selen", SIMPLE "toki Selen" "Seren" )
        , ( "toki Semisi", SIMPLE "toki Semisi" "Semitish" )
        , ( "toki Sinan", SIMPLE "toki Sinan" "Klingon" )
        , ( "toki Sintalin", SIMPLE "toki Sintalin" "Sindarin" )
        , ( "toki sitelen Anlasi", SIMPLE "toki sitelen Anlasi" "Unker Non-Linear Writing System" )
        , ( "toki sitelen Pisinpo", SIMPLE "toki sitelen Pisinpo" "Blissymbols" )
        , ( "toki Soleso", SIMPLE "toki Soleso" "Solresol" )
        , ( "toki Soma", SIMPLE "toki Soma" "Somish" )
        , ( "toki Tolome", SIMPLE "toki Tolome" "Traumae" )
        , ( "toki Tosulaki", SIMPLE "toki Tosulaki" "Dothraki" )
        , ( "Apiwili", FOREIGN "Apiwili" "Afrihili" )
        , ( "Epelanto", FOREIGN "Epelanto" "Esperanto" )
        , ( "Inli pona", FOREIGN "Inli pona" "Basic English" )
        , ( "Inota", FOREIGN "Inota" "Lingua Ignota" )
        , ( "Intelinwa", FOREIGN "Intelinwa" "Interlingua" )
        , ( "Ito", FOREIGN "Ito" "Ido" )
        , ( "Kuwenja", FOREIGN "Kuwenja" "Quenya" )
        , ( "Latan", FOREIGN "Latan" "Láadan" )
        , ( "Losupan", FOREIGN "Losupan" "Lojban" )
        , ( "Mansi", FOREIGN "Mansi" "Mänti" )
        , ( "Nawi", FOREIGN "Nawi" "Na’vi" )
        , ( "Olapi", FOREIGN "Olapi" "Volapük" )
        , ( "Palepelen", FOREIGN "Palepelen" "Baôleybelen" )
        , ( "Selen", FOREIGN "Selen" "Seren" )
        , ( "Semisi", FOREIGN "Semisi" "Semitish" )
        , ( "Sinan", FOREIGN "Sinan" "Klingon" )
        , ( "Sintalin", FOREIGN "Sintalin" "Sindarin" )
        , ( "sitelen Anlasi", FOREIGN "sitelen Anlasi" "Unker Non-Linear Writing System" )
        , ( "sitelen Pisinpo", FOREIGN "sitelen Pisinpo" "Blissymbols" )
        , ( "Soleso", FOREIGN "Soleso" "Solresol" )
        , ( "Soma", FOREIGN "Soma" "Somish" )
        , ( "Tolome", FOREIGN "Tolome" "Traumae" )
        , ( "Tosulaki", FOREIGN "Tosulaki" "Dothraki" )

        -- Other words
        , ( "tomo tawa", WORD "tomo tawa" NOUN_KIND (NOUN "car; vehicle") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as a vehicle") ADJ NO_ADVERB NO_NUMERAL NO_PREP )

        --, ( "tomo tawa telo", WORD "tomo tawa telo" NOUN_KIND (NOUN "boat") NO_PRE_VERB BE (VERB_TRANSITIVE "to transform as a boat") ADJ NO_ADVERB NO_NUMERAL NO_PREP )
        , ( "tomo tawa telo", SIMPLE "tomo tawa telo" "boat" )
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


word_ala =
    getRawTokipona "ala"


word_anu =
    getRawTokipona "anu"


word_kepeken =
    getRawTokipona "kepeken"


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


getTokipona : WORD -> String
getTokipona w =
    case w of
        ERROR s ->
            "UNKNOWN"

        PARTICLE tokipona grammar ->
            tokipona

        SIMPLE tokipona str ->
            tokipona

        FOREIGN tokipona str ->
            tokipona

        WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num prep ->
            tokipona


getDefaultKind : Bool -> WORD -> String
getDefaultKind pona w =
    let
        result =
            case w of
                ERROR s ->
                    "[ERROR: '" ++ s ++ "' is not a valid word]"

                PARTICLE tokipona grammar ->
                    grammar

                SIMPLE tokipona str ->
                    str

                FOREIGN tokipona str ->
                    str

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

                        LOC_KIND ->
                            case noun of
                                NOUN s ->
                                    s

                                AS_NOUN ->
                                    "[ERROR: default kind is LOC but there is no NOUN definition]"

                        SIMPLE_KIND ->
                            "[ERROR: " ++ tokipona ++ " is not a simle word]"

                        FOREIGN_KIND ->
                            "[ERROR: " ++ tokipona ++ " is not a foreign word]"
    in
    doPona pona result


convertSimple2Word : WORD -> WORD
convertSimple2Word word =
    case word of
        SIMPLE tokipona str ->
            WORD tokipona
                NOUN_KIND
                (NOUN str)
                NO_PRE_VERB
                (VERB ("to be [" ++ str ++ "]"))
                (VERB_TRANSITIVE ("to transform as a [" ++ str ++ "]"))
                (ADJECTIVE ("[" ++ str ++ "] as an adjective"))
                (ADVERB ("[" ++ str ++ "] as an adverb"))
                NO_NUMERAL
                NO_PREP

        _ ->
            word


convertForeign2Word : WORD -> WORD
convertForeign2Word word =
    case word of
        FOREIGN tokipona str ->
            WORD tokipona
                ADJECTIVE_KIND
                (NOUN str)
                NO_PRE_VERB
                (VERB ("to be [" ++ str ++ "]"))
                NO_VERB_TRANSITIVE
                (ADJECTIVE str)
                NO_ADVERB
                NO_NUMERAL
                NO_PREP

        _ ->
            word


rawTokiponaToString : Bool -> WORD_KIND -> WORD -> String
rawTokiponaToString pona kind w =
    case w of
        ERROR s ->
            "[ERROR: '" ++ s ++ "' is not a valid word]"

        PARTICLE tokipona grammar ->
            grammar

        SIMPLE tokipona str ->
            str

        FOREIGN tokipona str ->
            str

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

                LOC_KIND ->
                    case noun of
                        NOUN s ->
                            doPona pona s

                        AS_NOUN ->
                            "('" ++ getDefaultKind pona w ++ "' as a noun)"

                SIMPLE_KIND ->
                    case noun of
                        NOUN s ->
                            doPona pona s

                        AS_NOUN ->
                            "[ERROR: " ++ tokipona ++ " is not a simple word]"

                FOREIGN_KIND ->
                    case adjective of
                        ADJECTIVE a ->
                            doPona pona a

                        ADJ ->
                            "('" ++ getDefaultKind pona w ++ "' as an adjective)"


getKind : WORD -> WORD_KIND
getKind w =
    case w of
        ERROR s ->
            UNKNOW_KIND

        PARTICLE tokipona grammar ->
            GRAMMAR_KIND

        SIMPLE tokipona str ->
            SIMPLE_KIND

        FOREIGN tokipona str ->
            FOREIGN_KIND

        WORD tokipona defaultKind noun preVerb verb verbTransive adjective adverb num prep ->
            defaultKind
