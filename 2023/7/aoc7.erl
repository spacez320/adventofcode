%
% Advent of Code 2023, Day 7
%
% Plays poker-like games.

-module(aoc7).
-include_lib("eunit/include/eunit.hrl").
-export([main/1]).

% Reads input from a file.
read_input() ->
    {ok, Input} = file:read_file("7.txt"),
    string:tokens(binary_to_list(Input), "\n").

% Parses input entries into tuples of hands and bids.
parse_input(Input) ->
    lists:map(
        fun(I) ->
            I_S = string:split(I, " "),
            {lists:nth(1, I_S), element(1, string:to_integer(lists:nth(2, I_S)))}
        end,
        Input
    ).

% Provides a value enumeration of cards.
card(Card) ->
    case Card of
        "A" -> 14;
        "K" -> 13;
        "Q" -> 12;
        % Joker is a low-value wildcard.
        "J" -> 1;
        "T" -> 10;
        _ -> element(1, string:to_integer(Card))
    end.

% Provides an value enumeration of hand types.
hand_type(HandType) ->
    case HandType of
        fiveofakind -> 7;
        fourofakind -> 6;
        fullhouse -> 5;
        threeofakind -> 4;
        twopair -> 3;
        onepair -> 2;
        _ -> 1
    end.

% Parses a hand into its component cards. Used for future comparisons.
parse_hand(Hand) ->
    ParsedHand = parse_hand(element(1, Hand), []),
    {
        element(1, Hand),
        ParsedHand,
        case lists:keysearch("J", 1, ParsedHand) of
            % No wildcards.
            false ->
                read_hand(ParsedHand);
            % We have at least one wildcard and should account for it.
            _ ->
                read_hand_with_wildcard(ParsedHand)
        end,
        element(2, Hand)
    }.
parse_hand([Head | Tail], Hand) ->
    parse_hand(
        Tail,
        case lists:keyfind([Head], 1, Hand) of
            false ->
                % This card had not appeared before and should be added.
                lists:append([{[Head], 1}], Hand);
            Found ->
                % This card already exists and its count should be incremented.
                lists:keyreplace([Head], 1, Hand, {element(1, Found), element(2, Found) + 1})
        end
    );
parse_hand([], Hand) ->
    Hand.

% Determines what kind of hand we have.
%
% Should be provided a list of tuples;
%
%     [{C1, N1}, {C2, N2}, ..., {Cn, Nn}]
%
% where C is a card and N is the number of that card in the hand.
read_hand(Hand) when length(Hand) == 1 ->
    fiveofakind;
read_hand(Hand) when length(Hand) == 2 ->
    case lists:keymember(4, 2, Hand) of
        % There is one card with four instances.
        true -> fourofakind;
        % There must be two and three instances of two different cards.
        _ -> fullhouse
    end;
read_hand(Hand) when length(Hand) == 3 ->
    case lists:keymember(3, 2, Hand) of
        % There is one card with three instances.
        true -> threeofakind;
        % There must be two cards with each having two instances.
        _ -> twopair
    end;
read_hand(Hand) when length(Hand) == 4 ->
    onepair;
read_hand(_) ->
    highcard.

% Like the above function but accounts for the existence of a wildcard Joker.
read_hand_with_wildcard(Hand) when length(Hand) < 3 ->
    fiveofakind;
read_hand_with_wildcard(Hand) when length(Hand) == 3 ->
    case element(2, lists:keyfind("J", 1, Hand)) of
        1 ->
            case lists:keysearch(2, 2, Hand) of
                false -> fourofakind;
                % There must be one wildcard and two of each other card.
                _ -> fullhouse
            end;
        _ ->
            fourofakind
    end;
read_hand_with_wildcard(Hand) when length(Hand) == 4 ->
    threeofakind;
read_hand_with_wildcard(_) ->
    onepair.

% Returns true if Hand1 is stronger than Hand2.
%
% Each hand should be a tuple;
%
%     {H, [{C1, N1}, {C2, N2}, ..., {Cn, Nn}], Ht}
%
% where C is a card, N is the number of that card in the hand, H is the
% original hand, and Ht is the hand type.
is_stronger_hand(Hand1, Hand2) ->
    Hand1TypeVal = hand_type(element(3, Hand1)),
    Hand2TypeVal = hand_type(element(3, Hand2)),

    if
        Hand1TypeVal == Hand2TypeVal ->
            % These hand tpyes are the same and we must compare cards.
            is_stronger_cards(element(1, Hand1), element(1, Hand2));
        true ->
            % Pick the hand with the better value.
            Hand1TypeVal > Hand2TypeVal
    end.

% Compares a hand card by card, used for hand type tie-breaking.
is_stronger_cards([Card1 | Hand1Tail], [Card2 | Hand2Tail]) ->
    Card1Val = card([Card1]),
    Card2Val = card([Card2]),

    if
        Card1Val == Card2Val ->
            % These cards are equal--move on to compare the next card.
            is_stronger_cards(Hand1Tail, Hand2Tail);
        true ->
            % Pick the card with the better value.
            Card1Val > Card2Val
    end;
% Occurs when the compared hands are the same.
is_stronger_cards([], []) ->
    false.

% Provides a list of hands sorted from most valuable to least.
parse_and_sort_hands(Hands) ->
    lists:sort(
        fun(Hand1, Hand2) -> is_stronger_hand(Hand2, Hand1) end,
        lists:map(fun(Hand) -> parse_hand(Hand) end, Hands)
    ).

% Entry-point.
main(_) ->
    Input = read_input(),
    ParsedInput = parse_and_sort_hands(parse_input(Input)),

    lists:foldl(
        fun(X, Sum) -> (element(1, X) * element(4, element(2, X))) + Sum end,
        0,
        lists:enumerate(ParsedInput)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TESTS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

card_test() ->
    14 = card("A"),
    5 = card("5").

hand_type_test() ->
    7 = hand_type(fiveofakind).

parse_hand_test() ->
    {"AAAAA", [{"A", 5}], fiveofakind, 1} = parse_hand({"AAAAA", 1}),
    {"KAJAA", [{"J", 1}, {"A", 3}, {"K", 1}], fourofakind, 1} = parse_hand({"KAJAA", 1}),
    {"12341", [{"4", 1}, {"3", 1}, {"2", 1}, {"1", 2}], onepair, 1} = parse_hand({"12341", 1}).

read_hand_test() ->
    fiveofakind = read_hand([{"A", 5}]),
    fourofakind = read_hand([{"A", 4}, {"K", 1}]),
    fullhouse = read_hand([{"A", 2}, {"K", 3}]),
    threeofakind = read_hand([{"A", 1}, {"K", 3}, {"Q", 1}]),
    twopair = read_hand([{"A", 1}, {"K", 2}, {"Q", 2}]),
    onepair = read_hand([{"A", 2}, {"K", 1}, {"Q", 1}, {"J", 1}]),
    highcard = read_hand([{"A", 1}, {"K", 1}, {"Q", 1}, {"J", 1}, {"10", 1}]).

is_stronger_hand_test() ->
    true = is_stronger_hand(
        {"AAAAA", [{"A", 5}], fiveofakind, 1}, {"AAAAK", [{"A", 4}, {"K", 1}], fourofakind, 1}
    ),
    false = is_stronger_hand(
        {"AAAAK", [{"A", 4}, {"K", 1}], fourofakind, 1}, {"AAAAA", [{"A", 5}], fiveofakind, 1}
    ),
    true = is_stronger_hand(
        {"AAAAA", [{"A", 5}], fiveofakind, 1}, {"KKKKK", [{"K", 5}], fiveofakind, 1}
    ),
    false = is_stronger_hand(
        {"AAAA1", [{"A", 4}, {"1", 1}], fourofakind, 1},
        {"AAAA2", [{"A", 5}, {"2", 1}], fourofakind, 1}
    ),
    false = is_stronger_hand(
        {"AAAAA", [{"A", 5}], fiveofakind, 1}, {"AAAAA", [{"A", 5}], fiveofakind, 1}
    ).
