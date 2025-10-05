:- module(graphical_theme_menu, [choice_menu/3]).
:- use_module(library(pce)).

choice_menu(DarkThemes, LightThemes, ThemeSelector) :-
    new(D, dialog),
    send(D, append,
         new(M, menu('Theme list:', choice))),

    theme_browser(DarkThemes, DB),
    theme_browser(LightThemes, LB),

    send_list(D, append, [DB, LB]),

    send(M, append,
         menu_item(light_themes,
                   message(@prolog, toggle_themes, DB, LB, light_themes), light_themes)),

    send(M, append,
         menu_item(dark_themes,
                   message(@prolog, toggle_themes, DB, LB, dark_themes), dark_themes)),

    send(M, selection, light_themes),

    send(D, append,
         new(_, button('Set theme',
                       message(@prolog,
                               set_selected_theme, DB, LB, ThemeSelector)))),

    send(D, open),

    toggle_themes(DB, LB, light_themes).

set_selected_theme(Dark, Light, ThemeSelector) :-
    get(Dark, displayed, @on),
    get(Light, displayed, @off),
    get(Light, selection, Item),
    get(Item, label, Theme),
    send(@prolog, ThemeSelector, Theme).
set_selected_theme(Dark, Light, ThemeSelector) :-
    get(Light, displayed, @on),
    get(Dark, displayed, @off),
    get(Light, selection, Item),
    get(Item, label, Theme),
    send(@prolog, ThemeSelector, Theme).

toggle_themes(Dark, Light, dark_themes) :-
    send(Dark, displayed, @on),
    send(Light, displayed, @off).
toggle_themes(Dark, Light, light_themes) :-
    send(Light, displayed, @on),
    send(Dark, displayed, @off).

theme_browser(Themes, Browser) :-
    new(Browser, browser('Dark Themes')),
    send(Browser, width, 40),
    send(Browser, font, fixed),
    append_themes(Themes, Browser).

append_themes([], _).
append_themes([Theme | Themes], Browser) :-
    send(Browser, append, string(Theme)),
    append_themes(Themes, Browser).





