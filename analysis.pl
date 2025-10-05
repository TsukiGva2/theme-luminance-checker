:- use_module(library(readutil)).
:- use_module(library(filesex)).

:- use_module('util.pl').
:- use_module('graphical.pl').

choose_theme_pce :-
    list_themes("d", DarkThemes),
    list_themes("l", LightThemes),
    choice_menu(DarkThemes, LightThemes, theme_selected).

theme_selected(Theme) :-
    format('Setting \'~a\' as the kitty terminal theme.~n', [Theme]),
    kitty_theme_output(OutFile),
    expand_file_name(OutFile, [OutPath]),
    theme_dir(ThemeDir),
    atom_concat(ThemeDir, Theme, ThemePath),
    copy_file(ThemePath, OutPath), !.
