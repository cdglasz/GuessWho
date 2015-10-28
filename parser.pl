% Christopher Glasz
% CSC 481 - Artificial Intelligence
% Final Project - Guess Who

% main
%   Runs the Guess Who game proper
main:-
    consult('~/Desktop/CSC 481 Coursework/Final/prolog/factbase.pl'),
    options(Cast),
    random_select(Chosen, Cast, _),
    ask_computer(Chosen, Cast).

% ask_computer(+Chosen, +Cast)
%   The human player may ask the computer a question, and the computer
%   will respond based on Chosen, the character they randomly chose
ask_computer(Chosen,Cast) :-
    write('#####################'),nl,
    write('#     YOUR TURN     #'),nl,
    write('#####################'),nl,
    get_question(Q),
    separate(Q, L),
    ask_about(L, Subject), 
    parse(L, Chosen, Answer, Subject),
    write(Answer), nl, nl,
    !,
    (   
        % This question ends the game
        ask_about(L, name) ->
            (   
                % If the player guessed right, they win.
                Answer = yes -> 
                    write('You win! My person was '), write(Chosen), nl
                ;

                % If the player guessed wrong, they lose.
                Answer = no -> 
                    write('You lose! My person was '), write(Chosen), nl
            )
        ;   
        ask_human(Chosen,Cast)
    ).

% ask_human(+Chosen, +Cast)
%   The computer will ask the human player the question that will narrow
%   down the current Cast as much as possible
ask_human(Chosen,Cast) :-
    write('#####################'),nl,
    write('#  COMPUTER\'S TURN  #'),nl,
    write('#####################'),nl,
    choose_question(Cast,Question),
    print_question(Question),
    get_answer(Answer),nl,
    !,
    narrow_down(Question,Answer,Cast,New_Cast),
        
    (   
        % This question ends the game
        ask_about(Question, name) ->
            (   
                % If computer guessed right, the player loses.
                Answer = yes -> 
                    write('You lose! My person was '),  write(Chosen), nl
                ;
                
                % If the computer was wrong, the player wins.
                Answer = no -> 
                    write('You win! My person was '),  write(Chosen), nl
            )
        ;   
        ask_computer(Chosen,New_Cast)
    ).

% ask_about(+Question, ?Subject)
%   Unifies Subject with the topic of Question.
%
% Examples of use:
%   ?- ask_about([are, they, old], age).
%   true ;
%   false.
%
%   ?- ask_about([is, your, person, brienne], X).
%   X = name ;
%   false.
%
%   ?- ask_about([do, they, have, red, hair], X).
%   X = have ;
%   X = hair ;
%   false.
ask_about(L, hair) :-
    hair_words(Words),
    intersection(L, Words, [_|_]).
ask_about(L, beard) :-
    beard_words(Words),
    intersection(L, Words, [_|_]).
ask_about(L, age) :-
    age_words(Words),
    intersection(L, Words, [_|_]).
ask_about(L, gender) :-
    gender_words(Words),
    intersection(L, Words, [_|_]).
ask_about(L, clothing) :-
    clothing_words(Words),
    intersection(L, Words, [_|_]).
ask_about(L, have) :-
    have_words(Words),
    hair_words(Hair),
    beard_words(Beard),
    age_words(Age),
    gender_words(Gender),
    clothing_words(Clothing),
    intersection(L, Hair, []),
    intersection(L, Beard, []),
    intersection(L, Age, []),
    intersection(L, Gender, []),
    intersection(L, Clothing, []),
    intersection(L, Words, [_|_]).
ask_about(L, name) :-
    options(Words),
    intersection(L, Words, [_|_]).

% ask(+Question, +Chosen)
%   True if the answer to the Question about Chosen is yes
ask(Question, Chosen) :-
    ask_about(Question, Subject),
    parse(Question, Chosen, yes, Subject).

% ask(+Question, +Chosen, -Answer)
%   True if the answer to the Question about Chosen is Answer
ask(Question, Chosen, Answer) :-
    ask_about(Question, Subject), 
    parse(Question, Chosen, Answer, Subject).

% parse(+Question, +Chosen, -Answer, +Subject)
%   Unifies Answer with the answer to the Question about Chosen
parse(Question, Chosen, Answer, Subject) :-
    Suffix = '_parse',
    % Instead of having a dozen different predicates, I create the command
    atom_codes(Subject, Subject_Codes),
    atom_codes(Suffix, Suffix_Codes),
    append(Subject_Codes, Suffix_Codes, Command_List),
    atom_codes(Predicate, Command_List),
    Command =.. [Predicate, Question, Chosen, Answer],
    Command.

% hair_parse(+Question, +Chosen, -Answer)
%   Unifies Answer with the answer to the Question about Chosen.
%
% Examples of use:
%   hair_parse([do, they, have, red, hair], sansa, X).
%   X = yes.
%
%   hair_parse([are, they bald], stannis, X).
%   X = no.
%
%   hair_parse([are, they, balding], stannis, X).
%   X = yes.
hair_parse(L, Chosen, yes) :-
    colors(Colors),
    intersection(L, Colors, [_|_]),
    hair_color_check(L,Chosen).
hair_parse(L, Chosen, yes) :-
    lengths(Lengths),
    intersection(L, Lengths, [_|_]),
    hair_length_check(L,Chosen).
hair_parse(L, Chosen, yes) :-
    colors(Colors),
    lengths(Lengths),
    intersection(L, Colors, []),
    intersection(L, Lengths, []),
    have_hair_check(L,Chosen).
hair_parse(_, _, no).

% beard_parse(+Question, +Chosen, -Answer)
%   Unifies Answer with the answer to the Question about Chosen.
%
% Examples of use:
%   beard_parse([do, they, have, a, beard], sansa, X).
%   X = no.
%
%   beard_parse([does, your, person, have, a, beard], jon, X).
%   X = yes.
beard_parse(L, Chosen, yes) :-
    have_beard_check(L, Chosen).
beard_parse(_, _, no).

% age_parse(+Question, +Chosen, -Answer)
%   Unifies Answer with the answer to the Question about Chosen.
%
% Examples of use:
%   age_parse([is, your, person, old], tywin, X).
%   X = yes.
%
%   age_parse([is, she, young], brienne, X).
%   X = no.
%
%   age_parse([are, they, an, adult], jorah, X).
%   X = yes.
age_parse(L, Chosen, yes) :-
    age_check(L, Chosen).
age_parse(_, _, no).

% gender_parse(+Question, +Chosen, -Answer)
%   Unifies Answer with the answer to the Question about Chosen.
%
% Examples of use:
%   gender_parse([is, your, person, a, man], tywin, X).
%   X = yes.
%
%   gender_parse([is, it, a, lady], brienne, X).
%   X = yes.
%
%   gender_parse([are, they, a, boy], arya, X).
%   X = no.
gender_parse(L, Chosen, yes) :-
        gender_check(L, Chosen).
gender_parse(_, _, no).

% have_parse(+Question, +Chosen, -Answer)
%   Unifies Answer with the answer to the Question about Chosen.
%
% Examples of use:
%   have_parse([does, your, person, have, hair], varys, X).
%   X = no.
%
%   have_parse([do, they, have, a, scar], tyrion, X).
%   X = yes.
have_parse(L, Chosen, yes) :-
    have_check(L, Chosen).
have_parse(_, _, no).

% clothing_parse(+Question, +Chosen, -Answer)
%   Unifies Answer with the answer to the Question about Chosen.
%
% Examples of use:
%   clothing_parse([is, your, person, wearing, armor], varys, X).
%   X = no.
%
%   clothing_parse([do, they, have, a, scar], tyrion, X).
%   X = yes.
clothing_parse(L, Chosen, yes) :-
    clothing_colors(Colors),
    intersection(L, Colors, [_|_]),
    clothing_color_check(L, Chosen).
clothing_parse(L, Chosen, yes) :-
    colors(Colors),
    intersection(L, Colors, []),
    wearing_check(L, Chosen).
clothing_parse(_, _, no).

% name_parse(+Question, +Chosen, -Answer)
%   Unifies Answer with the answer to the Question about Chosen.
%
% Examples of use:
%   name_parse([is, their, name, jorah], varys, X).
%   X = no.
%
%   name_parse([is, it, tyrion], tyrion, X).
%   X = yes.
name_parse(L, Chosen, yes) :-
    name_check(L, Chosen).
name_parse(_, _, no).

% have_hair_check(+Question, ?X)
%   Unifies X with people about which the Question is true
have_hair_check([have, hair|_], X) :-
    has_a(X, hair).
have_hair_check([have, any, hair|_], X) :-
    has_a(X, hair).
have_hair_check([no, hair|_], X) :-
    bald(X).
have_hair_check([bald|_], X) :-
    bald(X).
have_hair_check([balding|_], X) :-
    balding(X).
have_hair_check([losing|_], X) :-
    balding(X).
have_hair_check([_|T], X) :-
    have_hair_check(T, X).

% hair_color_check(+Question, ?X)
%   Unifies X with people about which the Question is true
hair_color_check([Color|_], X) :-
    hair_color(X, Color).
hair_color_check([_|T], X) :-
    hair_color_check(T, X).

% clothing_color_check(+Question, ?X)
%   Unifies X with people about which the Question is true
clothing_color_check([Color|_], X) :-
    clothing_color(X, Color).
clothing_color_check([_|T], X) :-
    clothing_color_check(T, X).

% hair_length_check(+Question, ?X)
%   Unifies X with people about which the Question is true
hair_length_check([Length|_], X) :-
    hair_length(X, Length).
hair_length_check([_|T], X) :-
    hair_length_check(T, X).

% have_beard_check(+Question, ?X)
%   Unifies X with people about which the Question is true
have_beard_check([beard|_], X) :-
    has_a(X, beard).
have_beard_check([_|T], X) :-
    have_beard_check(T, X).

% have_check(+Question, ?X)
%   Unifies X with people about which the Question is true
have_check([Obj|_], X) :-
    has_a(X,Obj).
have_check([_|T], X) :-
    have_check(T, X).

% wearing_check(+Question, ?X)
%   Unifies X with people about which the Question is true
wearing_check([Obj|_], X) :-
    wearing(X,Obj).
wearing_check([_|T], X) :-
    wearing_check(T, X).

% age_check(+Question, ?X)
%   Unifies X with people about which the Question is true
age_check(L, X) :-
    old_words(Words),
    intersection(L, Words, [_|_]),
    age(X, old).
age_check(L, X) :-
    adult_words(Words),
    intersection(L, Words, [_|_]),
    age(X, adult).
age_check(L, X) :-
    young_words(Words),
    intersection(L, Words, [_|_]),
    age(X, young).

% gender_check(+Question, ?X)
%   Unifies X with people about which the Question is true
gender_check(L, X) :-
    male_words(Pronouns),
    intersection(L, Pronouns, [_|_]),
    male(X).
gender_check(L, X) :-
    female_words(Pronouns),
    intersection(L, Pronouns, [_|_]),
    female(X).

% name_check(+Question, ?X)
%   Unifies X with people about which the Question is true
name_check([X|_], X).
name_check([_|T], X) :-
    name_check(T, X).

% separate(+Question, -Separated)
%   Unifies Separated into the list of words in Question
separate(X, L) :-
    remove_char(X, '?', R),
    atomic_list_concat(L, ' ', R).

% remove_char(+Sentence, +Character, -Removed)
%   Unifies Removed with the Sentence after removing the Character
remove_char(S, C, X) :-
    atom_concat(L, R, S),
    atom_concat(C, W, R),
    atom_concat(L, W, X).

% is_a_q(+Sentence)
%   Is true if the Sentence is a question. A question must begin with a 
%   special word (like 'is', or 'does'), and end in a question mark
is_a_q(S) :-
    sub_atom(S, _, _, 0, '?'),
    (sub_atom(S, 0, _, _, 'Is');
     sub_atom(S, 0, _, _, 'is');
     sub_atom(S, 0, _, _, 'Does');
     sub_atom(S, 0, _, _, 'does');
     sub_atom(S, 0, _, _, 'Do');
     sub_atom(S, 0, _, _, 'do');
     sub_atom(S, 0, _, _, 'Are');
     sub_atom(S, 0, _, _, 'are')).

% choose_question(+Cast, -Question)
%   Unifies Question with the question with the most entropy over Cast
%   i.e. the question that rule out the largest number of characters in 
%        the worst case
choose_question([Person],[is, your, person, Person]).
choose_question(Cast, Question) :-
    build_questions(Questions),
    Questions = [Head | Tail],
    choose_question(Cast, Tail, Head, Question). 
choose_question(_, [], Question, Question).
choose_question(Cast, [Current | Rest], Choice, Question) :-
    entropy(Choice, Cast, Old_Entropy),
    entropy(Current, Cast, Entropy),
    Entropy > Old_Entropy,
    choose_question(Cast, Rest, Current, Question).
choose_question(Cast, [Current | Rest], Choice, Question) :-
    entropy(Choice, Cast, Old_Entropy),
    entropy(Current, Cast, Entropy),
    Entropy < Old_Entropy,
    choose_question(Cast, Rest, Choice, Question).
choose_question(Cast, [_ | Rest], Choice, Question) :-
    choose_question(Cast, Rest, Choice, Question).

% entropy(+Question, +Cast, -Entropy)
%   Sets Entropy equal to the entropy of the given Question 
%   for the given Cast
entropy(Question, Cast, 0) :-
    count(Question, Cast, 0, _).
entropy(Question, Cast, 0) :-
    count(Question, Cast, _, 0).
entropy(Question, Cast, E) :-
    count(Question, Cast, T, F),
    T > 0, F > 0,
    Total is T + F,
    P1 is T/Total,
    P2 is F/Total,
    log(2, P1, L1), log(2, P2, L2),
    E is - P1 * L1 - P2 * L2.

% log(+Base, +N, -Result)
%   Sets Result to the log of N, with the given Base
log(Base, N, Result) :-
    Num is log(N),
    Den is log(Base),
    Result is Num / Den.

% count(+Question, +Cast, -T, -F)
%   Sets T to the number of characters in Cast for which the Question is
%   true, and F to the number of characters in Cast for which the
%   Question is false
count(_,[],0,0).
count(Question, [Current | Rest], T, F) :-
    ask(Question, Current),
    count(Question, Rest, T0, F),
    T is T0 + 1.
count(Question, [Current | Rest], T, F) :-
    not(ask(Question, Current)),
    count(Question, Rest, T, F0),
    F is F0 + 1.

% narrow_down(+Question, +Answer, +Cast, -New_Cast)
%   Unifies New_Cast with a list of characters in Cast for 
%   which Answer is the answer to the Question
narrow_down(_, _, [], []).
narrow_down(Question, yes, [Current | Rest],  [Current | Cast]) :-
    ask(Question, Current),
    narrow_down(Question, yes, Rest, Cast).
narrow_down(Question, yes, [Current | Rest],  Cast) :-
    not(ask(Question, Current)),
    narrow_down(Question, yes, Rest, Cast).
narrow_down(Question, no, [Current | Rest],  [Current | Cast]) :-
    not(ask(Question, Current)),
    narrow_down(Question, no, Rest, Cast).
narrow_down(Question, no, [Current | Rest],  Cast) :-
    ask(Question, Current),
    narrow_down(Question, no, Rest, Cast).

% build_questions(-Questions)
%   Unifies Questions with a list of all viable questions. Questions are
%   formatted as a list of words in the question, so as to be more easily 
%   manipulated, interpreted, and printed.
build_questions(Questions) :-
    add_hair_questions([], Hair_Qs),
    add_clothing_questions(Hair_Qs, Clothing_Qs),
    add_gender_questions(Clothing_Qs, Gender_Qs),
    add_have_questions(Gender_Qs, Have_Qs),
    add_age_questions(Have_Qs, Age_Qs),
    add_beard_questions(Age_Qs, Beard_Qs),
    Questions = Beard_Qs.
    
% add_X_questions(+So_Far, -Questions)
%   Unifies Questions with the union of So_Far and all viable questions
%   relating to X
add_hair_questions(So_Far, Questions) :-
     hair_colors(Colors),
     add_hair_questions(Colors, So_Far, Questions).
add_hair_questions([], So_Far, [[are, they, bald], 
                                  [are, they, balding] | So_Far]).
add_hair_questions([Color|Rest], So_Far, Questions) :-
    Q = [is, their, hair, Color],
    add_hair_questions(Rest, [Q | So_Far], Questions).

% add_X_questions(+So_Far, -Questions)
%   Unifies Questions with the union of So_Far and all viable questions
%   relating to X
add_clothing_questions(So_Far, Questions) :-
    clothing_colors(Colors),
    outfits(Clothes),
    add_clothing_color_questions(Colors, So_Far, Qs),
    add_wearing_questions(Clothes, Qs, Questions).
add_clothing_color_questions([], So_Far, So_Far).
add_clothing_color_questions([Color|Rest], So_Far, Questions) :-
    Q = [are, they, wearing, Color, clothes],
    add_clothing_color_questions(Rest, [Q | So_Far], Questions).
add_wearing_questions([], So_Far, So_Far).
add_wearing_questions([Object | Rest], So_Far, Questions) :-
    Q = [are, they, wearing, a, Object],
    add_wearing_questions(Rest, [Q | So_Far], Questions).

% add_X_questions(+So_Far, -Questions)
%   Unifies Questions with the union of So_Far and all viable questions
%   relating to X
add_have_questions(So_Far, Questions) :-
    objects(Objects),
    add_have_questions(Objects, So_Far, Questions).
add_have_questions([], So_Far, So_Far).
add_have_questions([Object|Rest], So_Far, Questions) :-
    Q = [do, they, have, a(n), Object],
    add_have_questions(Rest, [Q | So_Far], Questions).

% add_X_questions(+So_Far, -Questions)
%   Unifies Questions with the union of So_Far and all viable questions
%   relating to X
add_gender_questions(So_Far, [[are, they, female],
                                [are, they, male] | So_Far]).

% add_X_questions(+So_Far, -Questions)
%   Unifies Questions with the union of So_Far and all viable questions
%   relating to X
add_age_questions(So_Far, [[are, they, elderly],
                             [are, they, a, child] | So_Far]).

% add_X_questions(+So_Far, -Questions)
%   Unifies Questions with the union of So_Far and all viable questions
%   relating to X
add_beard_questions(So_Far, [[do, they, have, a, beard] | So_Far]).

% get_question(?Question)
%   Continues to ask the user for a question until an appropriate
%   question is supplied.
get_question(Q):-
    read(X),
    check_question(X, Q).
check_question(X, X) :-
    is_a_q(X).
check_question(_, Q):-
    write('Please enter a question (must end with a question mark)'), nl,
    read(X),
    check_question(X, Q).

% get_answer(?Answer)
%   Continues to ask the user for an answer until an appropriate
%   answer is supplied.
get_answer(Answer) :-
    read(A),
    check_answer(A, Answer).
check_answer(yes, yes).
check_answer('Yes', yes).
check_answer(no, no).
check_answer('No', no).
check_answer(_, Answer) :-
    write('Response must be either yes or no.'), nl,
    read(A),
    check_answer(A, Answer).

% print_question(+Question)
%   Prints the given list in the form of a question
print_question([]) :- write('?'),nl.
print_question([H | T]) :-
    write(' '), write(H),
    print_question(T).
