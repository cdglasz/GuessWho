options([arya, barristan, bran, brienne, cersei, daenerys,
         davos, drogo, jaime, joffrey, jon,
         margaery, melisandre, ned, oberyn, petyr, robb, sandor,
         sansa, stannis, tyrion, tywin, varys, ygritte]).

age_words([age, young, old, child, adult, elder, elderly, kid]).
hair_words([hair, bald, balding, blonde, brunette, haired]).
beard_words([beard, goatee, neckbeard, stubble]).
gender_words([male, female, boy, girl, man, woman, lady, gentleman, dude]).
clothing_words([wearing, wear, wears, clothes, clothing | List]) :-
    setof(X, C^wearing(C,X), List).
    

male_words([male, boy, man, gentleman, dude]).
female_words([female, girl, woman, lady, she]).

old_words([old, elderly, elder, aged]).
adult_words([adult]).
young_words([young, child, kid]).

hair_colors(List) :-
    setof(X, C^hair_color(C,X), List).
clothing_colors(List) :-
    setof(X, C^clothing_color(C,X), List).

colors([red, black, blonde, grey, gray, brown, 
        blonde, white, gold, golden, blue, bronze, 
        orange, yellow, green]).

lengths(List) :-
    setof(X, C^hair_length(C,X), List).

have_words([have, has, possess, possesses, hold, holding, holds]).
objects(List) :-
    setof(X, C^has_a(C,X), List).
outfits(List) :-
    setof(X, C^wearing(C,X), List).

yes_words([yes,affirmative,yep,yup,indeed,correct,right]).
no_words([no,nope,not,wrong,incorrect]).

hair_color(arya, brown).
hair_color(arya, brunette).
hair_color(barristan, white).
hair_color(bran, brown).
hair_color(bran, brunette).
hair_color(brienne, blonde).
hair_color(cersei, blonde).
hair_color(cersei, gold).
hair_color(cersei, golden).
hair_color(daenerys, white).
hair_color(daenerys, blonde).
hair_color(davos, gray).
hair_color(davos, grey).
hair_color(davos, white).
hair_color(davos, black).
hair_color(drogo, black).
hair_color(jaime, brown).
hair_color(jaime, brunette).
hair_color(joffrey, blonde).
hair_color(joffrey, golden).
hair_color(joffrey, gold).
hair_color(jon, black).
hair_color(margaery, brown).
hair_color(margaery, brunette).
hair_color(melisandre, red).
hair_color(ned, brown).
hair_color(ned, black).
hair_color(oberyn, black).
hair_color(petyr, brown).
hair_color(petyr, brunette).
hair_color(petyr, black).
hair_color(robb, brown).
hair_color(sandor, brown).
hair_color(sandor, brunette).
hair_color(sansa, red).
hair_color(sansa, orange).
hair_color(stannis, white).
hair_color(stannis, grey).
hair_color(stannis, gray).
hair_color(stannis, black).
hair_color(tyrion, blonde).
hair_color(tyrion, brown).
hair_color(tyrion, brunette).
hair_color(tywin, white).
hair_color(ygritte, red).
hair_color(ygritte, orange).

bald(varys).

balding(barristan).
balding(bronn).
balding(davos).
balding(stannis).
balding(tywin).

hair_length(arya, short).
hair_length(bran, long).
hair_length(barristan, short).
hair_length(brienne, short).
hair_length(bronn, long).
hair_length(cersei, long).
hair_length(daenerys, long).
hair_length(davos, short).
hair_length(drogo, long).
hair_length(jaime, short).
hair_length(joffrey, short).
hair_length(jon, long).
hair_length(jorah, short).
hair_length(margaery, long).
hair_length(melisandre, long).
hair_length(ned, long).
hair_length(oberyn, short).
hair_length(petyr, short).
hair_length(robb, long).
hair_length(sandor, long).
hair_length(sansa, long).
hair_length(stannis, short).
hair_length(tyrion, long).
hair_length(tywin, short).
hair_length(ygritte, long).

age(arya, young).
age(barristan, old).
age(bran, young).
age(brienne, adult).
age(cersei, adult).
age(daenerys, adult).
age(davos, adult).
age(davos, old).
age(drogo, adult).
age(jaime, adult).
age(joffrey, young).
age(jon, adult).
age(margaery, adult).
age(melisandre, adult).
age(ned, adult).
age(oberyn, adult).
age(petyr, adult).
age(robb, adult).
age(sandor, adult).
age(sansa, adult).
age(sansa, young).
age(stannis, adult).
age(tyrion, adult).
age(tywin, old).
age(varys, adult).
age(ygritte, adult).

female(arya).
female(brienne).
female(cersei).
female(daenerys).
female(margaery).
female(melisandre).
female(sansa).
female(ygritte).

male(barristan).
male(bran).
male(davos).
male(drogo).
male(jaime).
male(joffrey).
male(jon).
male(ned).
male(oberyn).
male(petyr).
male(robb).
male(sandor).
male(stannis).
male(tyrion).
male(tywin).
male(varys).

has_a(arya, hair).
has_a(barristan, hair).
has_a(bran, hair).
has_a(brienne, hair).
has_a(cersei, hair).
has_a(daenerys, hair).
has_a(davos, hair).
has_a(drogo, hair).
has_a(jaime, hair).
has_a(joffrey, hair).
has_a(jon, hair).
has_a(jorah, hair).
has_a(margaery, hair).
has_a(melisandre, hair).
has_a(ned, hair).
has_a(oberyn, hair).
has_a(petyr, hair).
has_a(robb, hair).
has_a(sandor, hair).
has_a(sansa, hair).
has_a(stannis, hair).
has_a(tyrion, hair).
has_a(tywin, hair).
has_a(ygritte, hair).

has_a(barristan, beard).
has_a(davos, beard).
has_a(drogo, beard).
has_a(jon, beard).
has_a(ned, beard).
has_a(oberyn, beard).
has_a(petyr, beard).
has_a(robb, beard).
has_a(sandor, beard).
has_a(stannis, beard).
has_a(tywin, beard).

has_a(barristan, mustache).
has_a(davos, mustache).
has_a(drogo, mustache).
has_a(jon, mustache).
has_a(ned, mustache).
has_a(oberyn, mustache).
has_a(petyr, mustache).
has_a(robb, mustache).
has_a(sandor, mustache).
has_a(stannis, mustache).
has_a(tywin, mustache).

has_a(drogo, scar).
has_a(tyrion, scar).
has_a(sandor, scar).

has_a(sandor, burn).

has_a(drogo, tattoo).
has_a(drogo, tattoos).

has_a(arya, sword).
has_a(jaime, sword).
has_a(ned, sword).
has_a(robb, sword).
has_a(sandor, sword).

has_a(ygritte, bow).

has_a(jaime, hand).

has_a(joffrey, crown).

has_a(melisandre, horse).
has_a(davos, horse).

has_a(cersei, necklace).
has_a(margaery, necklace).
has_a(melisandre, necklace).
has_a(sansa, necklace).

wearing(brienne, armor).
wearing(jaime, armor).
wearing(jon, armor).
wearing(robb, armor).
wearing(sandor, armor).
wearing(stannis, armor).
wearing(tywin, armor).

wearing(melisandre, hood).

wearing(bran, fur).
wearing(ned, fur).
wearing(robb, fur).
wearing(ygritte, fur).

wearing(cersei, necklace).
wearing(margaery, necklace).
wearing(melisandre, necklace).
wearing(sansa, necklace).

clothing_color(arya, brown).
clothing_color(barristan, black).
clothing_color(barristan, brown).
clothing_color(bran, brown).
clothing_color(brienne, brown).
clothing_color(brienne, gold).
clothing_color(brienne, bronze).
clothing_color(cersei, red).
clothing_color(daenerys, blue).
clothing_color(davos, black).
clothing_color(davos, brown).
clothing_color(davos, gray).
clothing_color(davos, grey).
clothing_color(jaime, gold).
clothing_color(jaime, golden).
clothing_color(joffrey, gold).
clothing_color(joffrey, golden).
clothing_color(joffrey, yellow).
clothing_color(jon, black).
clothing_color(margaery, blue).
clothing_color(melisandre, red).
clothing_color(ned, brown).
clothing_color(ned, black).
clothing_color(oberyn, gold).
clothing_color(oberyn, brown).
clothing_color(oberyn, orange).
clothing_color(oberyn, yellow).
clothing_color(petyr, black).
clothing_color(petyr, brown).
clothing_color(robb, brown).
clothing_color(sandor, black).
clothing_color(sansa, white).
clothing_color(sansa, blue).
clothing_color(stannis, gray).
clothing_color(stannis, grey).
clothing_color(tyrion, red).
clothing_color(tywin, red).
clothing_color(tywin, grey).
clothing_color(tywin, gray).
clothing_color(varys, brown).
clothing_color(varys, green).
clothing_color(ygritte, brown).
