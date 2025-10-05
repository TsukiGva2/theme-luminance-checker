:- module(theme_utils, [
              kitty_theme_output/1,
              theme_dir/1,
              die/0,
              choose/3,
              list_themes/2,
              write_list/1
          ]).

theme_dir('color-themes/').
kitty_theme_output('~/.config/kitty/theme.conf').

die :- write('ok, bye'), nl, fail.

choose(List, N, Elem) :- nth1(N, List, Elem).
choose(List, N, Elem) :-
    \+ nth1(N, List, Elem),
    format('No such element: \'~d\'~n', [N]), fail.

list_themes("l", T) :- light_themes(T).
list_themes("d", T) :- dark_themes(T).

light_themes(Light) :-
    theme_dir(Dir),
    directory_files(Dir, List),
    list_clean(List, Themes),
    classify(Themes, Light, _).

dark_themes(Dark) :-
    theme_dir(Dir),
    directory_files(Dir, List),
    list_clean(List, Themes),
    classify(Themes, _, Dark).

ignored('.').
ignored('..').

% clean up the list based on 'ignored' elements.
list_clean([], []).
list_clean([H | T], R) :-
    ignored(H),
    list_clean(T, R).
list_clean([H | T], [H | R]) :-
    \+ ignored(H),
    list_clean(T, R).

write_list(List) :-
    wl_helper(1, List).
wl_helper(_, []).
wl_helper(C, [H|T]) :-
    format('~t~d~3+    ~a~n', [C, H]),
    I is C + 1,
    wl_helper(I, T).

% classify(Themes, Light, Dark) :-

classify([], [], []).
classify([T | Themes], [T | Light], Dark) :-
    theme_dir(Dir),
    atom_concat(Dir, T, TFile),
    check_lum(TFile, Lum),
    Lum > 128,
    classify(Themes, Light, Dark).
classify([T | Themes], Light, [T | Dark]) :-
    theme_dir(Dir),
    atom_concat(Dir, T, TFile),
    check_lum(TFile, Lum),
    Lum < 128,
    classify(Themes, Light, Dark).

check_lum(Theme, Lum) :-
    read_file_to_string(Theme, ThemeSrc, []),
    get_bg_hex_code(ThemeSrc, Hex),
    hex_to_rgb(Hex, R, G, B),
    luminance(R, G, B, Lum).

get_bg_hex_code(ThemeSrc, Color) :-
    re_matchsub('background\\s+\\#(?<color>[0-9a-fA-F]{6})'/x, ThemeSrc, Groups, []),
    Color = Groups.color.

hex_to_rgb(HexString, R, G, B) :-
    string_to_list(HexString, Hex),
    htr_helper(Hex, R, G, B).

htr_helper([R1, R2, G1, G2, B1, B2], R, G, B) :-
    hex_to_dec([R1, R2], R),
    hex_to_dec([G1, G2], G),
    hex_to_dec([B1, B2], B).

hex_to_dec([H1, H2], D) :-
    hex_to_dec_digit(H1, D1),
    hex_to_dec_digit(H2, D2),
    D is D1 * 16 + D2.

hex_to_dec_digit(H, D) :-
    H =< 57,
    D is H - 48.
hex_to_dec_digit(H, D) :-
    H >= 97,
    H =< 102,
    D is H - 87.

luminance(R, G, B, Lum) :-
    Lum is 0.299*R + 0.587*G + 0.114*B.
