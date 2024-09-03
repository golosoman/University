% Предметная область – спортивные соревнования.
% Каждое соревнование может быть описано структурой: ранг соревнований, вид спорта,
% год проведения, страна проведения, список команд - участников. Команды
% - участники могут быть описаны следующей структурой: название команды,
% страна, результаты соревнований. Результаты соревнований могут быть
% описаны списком структур: название команды – соперника, страна, тип
% результата (выигрыш, проигрыш, ничья). Реализовать следующие типы
% запросов:
% 1 Найти год, в который участвовало максимальное число команд, в
% заданном ранге соревнований;
% 2 Найти вид спорта, в котором выступает заданная команда;
% 3 Найти все команды, которые участвовали в Олимпийских играх по
% определенному виду спорта;
% 4 Найти все команды, участвовавшие в соревнованиях в заданном году;
% 5 Найти все команды определенной страны, у которых были выигрыши.

% Библиотеки для работы с http
:-use_module(library(http/http_server)).
:-use_module(library(http/http_dispatch)).
:-use_module(library(http/http_parameters)).
:-use_module(library(http/html_write)).

% Библиотека для полного стектрейса ошибок
:- use_module(library(http/http_error)).

% Хэндлер для корневой страницы
:- http_handler(root(.), home_page, []).

% Хэндлер странички для ввода данных для поиска года, с максимальным
% числом команд
:- http_handler(root(max_teams_year_find), max_teams_year_find_page, []).
% Хэндлер поиска года, в который учавствовало максимальное число комад
:- http_handler(root(max_teams_year), max_teams_year_page, [method(post)]).

% Хэндлер странички для ввода данных для поиска вида спорта, в котором
% выступает заданная команда
:- http_handler(root(sport_by_team_find), sport_by_team_find_page, []).
% Хэндлер поиска вида спорта, в котором выступает заданная команда
:- http_handler(root(sport_by_team), sport_by_team_page, [method(post)]).

% Хэндлер странички для ввода данных для поиска всех команд, которые
% учавствовали в Олимпийских играх в определенном виде спорта
:- http_handler(root(olympc_teams_by_sport_find), olympc_teams_by_sport_find_page, []).
% Хэндлер поиска всех команд, которые учавствовали в Олимпийких играх в
% определенном виде спорта
:- http_handler(root(olympc_teams_by_sport), olympc_teams_by_sport_page, [method(post)]).

% Хэндлер странички для ввода данных для поиска всех команд, которые
% учавствовали в заданном году
:- http_handler(root(teams_in_year_find), teams_in_year_find_page, []).
% Хэндлер поиска всех команд, учавствоваших в соревнованиях в заданном
% году
:- http_handler(root(teams_in_year), teams_in_year_page, [method(post)]).

% Хэндлер странички для ввода данных для поиска всех команд определенной
% страны, у которых были выигрыши
:- http_handler(root(winning_teams_by_country_find), winning_teams_by_country_find_page, []).
% Хэндлер поиска всех команд определенной страны, у которых были
% выигрыши
:- http_handler(root(winning_teams_by_country), winning_teams_by_country_page, [method(post)]).

% Хэндлер страницы для добавления нового соревнования
:- http_handler(root(add), add_page, []).
% Хэндлер для добавления нового соревнования
:- http_handler(root(add_competition), add_competition, [method(post)]).

% Хэндлер страницы для получения информации о соревновании, которое
% нужно считать
:- http_handler(root(read), read_page, []).
% Хэндлер для для чтения информации
:- http_handler(root(read_competition), read_competition_page, [method(post)]).



% Хэндлер страницы для удаления соревнований
:- http_handler(root(delete), delete_page, []).
% Хэндлер для удаления соревнования
:- http_handler(root(delete_competition), delete_competition, [method(post)]).

% Хэндлер страницы для нахождения соревнований для обновления
:- http_handler(root(update_find), update_find_page, []).
% Хэндлер страницы для изменения данных
:- http_handler(root(update), update_page, [method(post)]).
% Хэндлер страницы для изменения данных
:- http_handler(root(update_competition), update_competition, [method(post)]).


% Хэндлер для ресета БД
:- http_handler(root(reset_DB), resetDB, [method(post)]).

% Определеяем 5-ти местный предикат в БД
:- dynamic competition/5.
:- dynamic team/3.
:- dynamic result/4.

% Запуск сервера
server(Port):- http_server(http_dispatch, [port(Port)]).
server:-server(8080).

% Остановка сервера
stop(Port):- http_stop_server(Port, http_dispatch).
stop:- stop(8080).

resetDB(_):-
   % Очистка БД
   retractall(competition(_, _, _, _, _)),
   retractall(team(_, _, _)),
   retractall(result(_, _, _, _)),

   % Инициализация БД
    assertz(competition('Олимпийские игры', 'Легкая атлетика', '2024', 'Франция',
            [team('Сборная США', 'США',
                  [result('Сборная Германии', 'Германия', 'Выигрыш'),
                   result('Сборная Ямайки', 'Ямайка', 'Выигрыш')]),
             team('Сборная Германии', 'Германия',
                  [result('Сборная США', 'США', 'Проигрыш'),
                   result('Сборная Ямайки', 'Ямайка', 'Проигрыш')]),
             team('Сборная Ямайки', 'Ямайка',
                  [result('Сборная США', 'США', 'Проигрыш'),
                   result('Сборная Германии', 'Германия', 'Выигрыш')])
            ])),

   assertz(competition('Чемпионат', 'Футбол', '2020', 'Англия',
            [team('Сборная Англии', 'Англия',
                  [result('Сборная Франции', 'Франция', 'Выигрыш'),
                   result('Сборная Испании', 'Испания', 'Проигрыш')]),
            team('Сборная Франции', 'Франция',
                  [result('Сборная Англии', 'Англия', 'Проигрыш'),
                   result('Сборная Испании', 'Испания', 'Проигрыш')]),
            team('Сборная Испании', 'Испания',
                  [result('Сборная Англии', 'Англия', 'Выигрыш'),
                   result('Сборная Фрнации', 'Франция', 'Выигрыш')])
            ])),

   assertz(competition('Чемпионат', 'Баскетбол', '2021', 'США',
            [team('Сборная США', 'США',
                  [result('Сборная Канады', 'Канада', 'Выигрыш'),
                   result('Сборная Мексики', 'Мексика', 'Выигрыш'),
                   result('Сборная Франции', 'Франция', 'Проигрыш')]),
            team('Сборная Канады', 'Канада',
                  [result('Сборная США', 'США', 'Проигрыш'),
                   result('Сборная Мексики', 'Мексика', 'Ничья'),
                   result('Сборная Франции', 'Франция', 'Выигрыш')]),
            team('Сборная Мексики', 'Мексика',
                  [result('Сборная США', 'США', 'Проигрыш'),
                   result('Сборная Канады', 'Канада', 'Ничья'),
                   result('Сборная Франции', 'Франция', 'Выигрыш')]),
            team('Сборная Франции', 'Франция',
                  [result('Сборная США', 'США', 'Выигрыш'),
                   result('Сборная Канады', 'Канада', 'Проигрыш'),
                   result('Сборная Мексики', 'Мексика', 'Проигрыш')])
            ])),

   asserta(competition('Дружеский матч', 'Футбол', '2022', 'Россия',
	[team('Зенит', 'Россия',
              [result('Спартак', 'Россия', 'Выигрыш')]),
         team('Спартак', 'Россия',
              [result('Зенит', 'Россия', 'Проигрыш')])
        ])),
/*
   asserta(competition('Турнир', 'Волейбол', 2023, 'Бразилия',
            [team('Нова Серрана', 'Бразилия',
                  [result('Эквип Джекинс', 'Бразилия', 'Проигрыш')]),
            team('Эквип Джекинс', 'Бразилия',
                  [result('Нова Серрана', 'Бразилия', 'Выигрыш')])
            ])),*/

   assertz(competition('Турнир', 'Волейбол', '2023', 'Бразилия',
            [team('Нова Серрана', 'Бразилия',
                  [result('Эквип Джекинс', 'Бразилия', 'Проигрыш'),
                   result('Трез Волей', 'Бразилия', 'Выигрыш')]),
            team('Эквип Джекинс', 'Бразилия',
                  [result('Нова Серрана', 'Бразилия', 'Выигрыш'),
                   result('Трез Волей', 'Бразилия', 'Ничья')]),
            team('Трез Волей', 'Бразилия',
                  [result('Эквип Джекинс', 'Бразилия', 'Ничья'),
                   result('Нова Серрана', 'Бразилия', 'Проигрыш')])
            ])),


   assertz(competition('Олимпийские игры', 'Борьба', '2016', 'США',
           [team('Команда США', 'США',
                 [result('Команда Япония', 'Япония', 'Проигрыш')]),
           team('Команда Россия', 'Россия',
                 [result('Команда Дания', 'Дания', 'Выигрыш')]),
           team('Команда Дания', 'Дания',
                 [result('Команда Россия', 'Россия', 'Проигрыш')]),
           team('Команда Япония', 'Япония',
                 [result('Команда США', 'США', 'Выигрыш')]),
           team('Команда Куба', 'Куба',
                 [result('Команда Турция', 'Турция', 'Выигрыш')]),
           team('Команда Турция', 'Турция',
                 [result('Команда Куба', 'Куба', 'Проигрыш')])
           ])),



   http_redirect(moved, '/', _Request).

% Обработка главной страницы
home_page(_Request):-
    % Собираем  Ранг соревнований в список
    findall(Rank, competition(Rank, _, _, _, _), Ranks),

    % Собираем  Ранг соревнований в список
    findall(Sport, competition(_, Sport, _, _, _), Sports),

    % Собираем  Ранг соревнований в список
    findall(Year, competition(_, _, Year, _, _), Years),

    % Собираем  Ранг соревнований в список
    findall(Country, competition(_, _, _, Country, _), Countries),

    % Собираем  Ранг соревнований в список
    %findall(Teams, competition(_, _, _, _, Teams), TeamsList),

    % Добавляем заголовки таблицы
    generate_rows(Ranks, Sports, Years, Countries, Row),

    % Выравниваем список
    flatten(Row, FlattenRow),

    ins(FlattenRow, tr(
                  [
                    th('Ранг Соревнования'),
                    th('Вид спорта'),
                    th('Год'),
                    th('Страна проведения'),
                    th('Команда'),
                    th('Страна'),
                    th('Оппонент'),
                    th('Страна оппонента'),
                    th('Результат')
                  ]
              ), Rows_with_Headers),

    reply_html_page(
        title('Таблица спортивных соревнований'),
        [
             h1('Спортивные соревнования'),
             h3('Задания варианта'),
             form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/max_teams_year_find')], 'Task_1'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/sport_by_team_find')], 'Task_2'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/olympc_teams_by_sport_find')], 'Task_3'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/teams_in_year_find')], 'Task_4'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/winning_teams_by_country_find')], 'Task_5'))
            ),
            br(''),
            h3('CRUD - CREAT/READ/UPDATE/DELETE'),
            form(
                [style('display: inline-block'), method(post)],
                p(button([type(submit), formaction(location_by_id('resetDB'))], 'Сбросить БД'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/add')], 'Добавить соревнование'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/read')], 'Считать соревнование'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/update_find')], 'Изменить соревнование'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/delete')], 'Удалить соревнования'))
            ),

            h3('Таблица соревнований'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]).

% Добавление элемента в начало списка
ins(L, El, [El|L]).

%generate_rows([], [], [], [], []).
%generate_rows([Rank|Ranks],
%              [Sport|Sports],
%              [Year|Years],
%              [Country|Countries],
%              [tr([td(Rank), td(Sport), td(Year), td(Country)])|Rows]):-
%    generate_rows(Ranks, Sports, Years, Countries, Rows).

generate_rows([], []).
generate_rows([Value|Values], [tr([td(Value)])|Rows]):-
    generate_rows(Values, Rows).

generate_rows([], [], [], [], []).
generate_rows([Rank|RestRanks],
              [Sport|RestSports],
              [Year|RestYears],
              [Country|RestCountries],
              [Rows|RestRows]):-

    % Получаем список команд для данного Rank/Sport/Year/Country
    competition(Rank, Sport, Year, Country, TeamsList),

    % Генерируем строки для каждой команды и их результатов
    generate_team_rows(Rank, Sport, Year, Country, TeamsList, Rows),
    generate_rows(RestRanks, RestSports, RestYears, RestCountries, RestRows).



generate_team_rows(_, _, _, _,[], []).
generate_team_rows(Rank,
                   Sport,
                   Year,
                   Country,
                   [team(TeamName, TeamCountry, Results)|RestTeams], [TeamRow|RestRow]):-
        generate_result_rows(Rank, Sport, Year, Country, TeamName, TeamCountry, Results, TeamRow),
        %format(atom(Row), 'Team: ~w, Country: ~w', [TeamName, TeamCountry]),  % Выводим данные каждого матча
        %write(Row),  % Выводим данные каждого матча в консоль для отладки
        generate_team_rows(Rank, Sport, Year, Country, RestTeams, RestRow).

generate_result_rows(_, _, _, _, _, _, [], []).  % Базовый случай - завершение рекурсии

generate_result_rows(Rank,
                     Sport,
                     Year,
                     Country,
                     TeamName,
                     TeamCountry,
                     [result(Opponent, OpponentCountry, Outcome)|RestResults],
                     [tr([td(Rank), td(Sport), td(Year), td(Country), td(TeamName), td(TeamCountry),
                          td(Opponent), td(OpponentCountry), td(Outcome)])|RestRows]):-
    %format(atom(Row), 'Match: ~w vs. ~w - ~w', [TeamName, Opponent, Outcome]),  % Выводим данные каждого матча
    %write(Row),  % Выводим данные каждого матча в консоль для отладки
    generate_result_rows(Rank, Sport, Year, Country, TeamName, TeamCountry, RestResults, RestRows).


% 1 Найти год, в который участвовало максимальное число команд, в
% заданном ранге соревнований;
maxTeamsYearList(Rank, YearRow) :-
    findall(NumTeams, (competition(Rank, _, Year, _, Teams),
                       length(Teams, NumTeams)), NumTeamsList),
    max_list(NumTeamsList, MaxNumTeams),
    findall(Year, (competition(Rank, _, Year, _, Teams),
                   length(Teams, MaxNumTeams)), YearList),
    generate_rows(YearList, YearRow).


max_teams_year_find_page(_Request):-
   reply_html_page(
        title('Ввод ранга соревнований'),
        [form(
            [action=location_by_id(max_teams_year_page), method(post)],
            [
                h2('Найти год, в который участвовало максимальное число команд, в заданном ранге соревнований'),
                table([
                    tr([th('Ранг'), td(input([name(rank)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Искать'])))
                ] )
            ]
        )]
    ).

max_teams_year_page(Request):-
   http_parameters(
        Request,
        [
            rank(Rank, [])
        ]
    ),
   maxTeamsYearList(Rank, YearRow),
   ins(YearRow, tr(
                  [
                    th('Год')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('Год, с максимальным числом команд'),
        [
            h1('Год, с максимальным числом команд'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).

% 2 Найти вид спорта, в котором выступает заданная команда;
sportByTeam(TeamName, SportsRow) :-
    %competition(_, Sport, _, _, Teams),
    %member(team(TeamName, _, _), Teams),
    findall(Sport, (competition(_, Sport, _, _, Teams),
                    member(team(TeamName, _, _), Teams)), Sports),
    generate_rows(Sports, SportsRow).

sport_by_team_find_page(_Request):-
   reply_html_page(
        title('Ввод названия команды'),
        [form(
            [action=location_by_id(sport_by_team_page), method(post)],
            [
                h2('Найти вид спорта, в котором выступает заданная команда'),
                table([
                    tr([th('Название команды'), td(input([name(teamName)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Искать'])))
                ] )
            ]
        )]
    ).

sport_by_team_page(Request):-
   http_parameters(
        Request,
        [
            teamName(TeamName, [])
        ]
    ),
   sportByTeam(TeamName, SportRow),
   ins(SportRow, tr(
                  [
                    th('Вид спорта')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('Вид спорта, в котором выступает заданная команда'),
        [
            h1('Вид спорта, в котором выступает заданная команда'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).


% 3 Найти все команды, которые участвовали в Олимпийских играх по
% определенному виду спорта.
olympicTeamsBySport(Sport, TeamsRow) :-
    findall(Team,(competition('Олимпийские игры', Sport, _, _, TeamsList),
                  member(team(Team, _, _), TeamsList)), Teams),
    generate_rows(Teams, TeamsRow).

olympc_teams_by_sport_find_page(_Request):-
   reply_html_page(
        title('Ввод названия спорта'),
        [form(
            [action=location_by_id(olympc_teams_by_sport_page), method(post)],
            [
                h2('Найти все команды, которые участвовали в Олимпийских играх по определенному виду спорта'),
                table([
                    tr([th('Название вида спорта'), td(input([name(sport)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Искать'])))
                ] )
            ]
        )]
    ).

olympc_teams_by_sport_page(Request):-
   http_parameters(
        Request,
        [
            sport(Sport, [])
        ]
    ),
   olympicTeamsBySport(Sport, TeamsRow),
   ins(TeamsRow, tr(
                  [
                    th('Название команды')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('Команды, участвующие в Олимпийских играх в определенном виде спорта'),
        [
            h1('Команды, которые участвовали в Олимпийских играх в определенном виде спорта'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).


% 4 Найти все команды, участвовавшие в соревнованиях в заданном году;
teamsInYear(Year, TeamsRow) :-
   %atom_number(Year, YearNumber),
   findall(Team, (competition(_, _, Year, _, TeamsList),
                  member(team(Team, _, _), TeamsList)), Teams),
   generate_rows(Teams, TeamsRow).

teams_in_year_find_page(_Request):-
   reply_html_page(
        title('Ввод года'),
        [form(
            [action=location_by_id(teams_in_year_page), method(post)],
            [
                h2('Найти все команды, участвовавшие в соревнованиях в заданном году'),
                table([
                    tr([th('Год'), td(input([name(year), type(string)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Искать'])))
                ] )
            ]
        )]
    ).

teams_in_year_page(Request):-
   http_parameters(
        Request,
        [
            year(Year, [])
        ]
    ),
   teamsInYear(Year, TeamsRow),
   ins(TeamsRow, tr(
                  [
                    th('Название команды')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('Команды, участвовавшие в соревнованиях в заданном году'),
        [
            h1('Команды, участвовавшие в соревнованиях в заданном году'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).


% 5 Найти все команды определенной страны, у которых были выигрыши.
winningTeamsByCountry(Country, TeamsRow) :-
    findall(Team,
            (competition(_, _, _, _, TeamsList),
             member(team(Team, Country, Results), TeamsList),
             member(result(_, _, 'Выигрыш'), Results)),
            Teams),
    list_to_set(Teams, UniqueTeams),
    generate_rows(UniqueTeams, TeamsRow).

winning_teams_by_country_find_page(_Request):-
   reply_html_page(
        title('Ввод страны'),
        [form(
            [action=location_by_id(winning_teams_by_country_page), method(post)],
            [
                h2('Найти все команды определенной страны, у которых были выигрыши'),
                table([
                    tr([th('Страна'), td(input([name(country)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Искать'])))
                ] )
            ]
        )]
    ).

winning_teams_by_country_page(Request):-
   http_parameters(
        Request,
        [
            country(Country, [])
        ]
    ),
   winningTeamsByCountry(Country, TeamsRow),
   ins(TeamsRow, tr(
                  [
                    th('Название команды')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('Команды определенной страны, у которых были выигрыши'),
        [
            h1('Команды определенной страны, у которых были выигрыши'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).

add_or_update_competition(Name, Sport, Year, Country, NewTeams) :-
    competition(Name, Sport, Year, Country, ExistingTeams),
    % Если соревнование уже существует
    retract(competition(Name, Sport, Year, Country, ExistingTeams)),
    append(ExistingTeams, NewTeams, UpdatedTeams),
    asserta(competition(Name, Sport, Year, Country, UpdatedTeams)).

add_or_update_competition(Name, Sport, Year, Country, Teams) :-
    % Если соревнование не существует, добавляем новое соревнование в базу знаний
    asserta(competition(Name, Sport, Year, Country, Teams)).

% CRUD (Create/Read/Update/Delete)для своей базы фактов.
% Cтраница с добавлением нового соревнования
add_page(_Request):-
    reply_html_page(
        title('Добавление соревнования'),
        [form(
            [action=location_by_id(add_competition), method(post)],
            [
                table([
                    tr([th('Ранг'), td(input([name(rank)]))]),
                    tr([th('Вид спорта'), td(input([name(sport)]))]),
                    tr([th('Год'), td(input([name(year), type(number)]))]),
                    tr([th('Страна'), td(input([name(country)]))]),
                    tr([th('Команда'), td(input([name(teamName)]))]),
                    tr([th('Страна команды'), td(input([name(teamCountry)]))]),
                    tr([th('Команда соперника'), td(input([name(opponentName)]))]),
                    tr([th('Страна команды соперника'), td(input([name(opponentCountry)]))]),
                    tr([th('Результат соревнования'), td(input([name(result)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Добавить'])))
                ] )
            ]
        )]
    ).

% Предикат, который будет выполнять проверку и переприсвоение значения
% переменной
check_and_replace(Result, NewResult) :-
    (Result = 'Выигрыш' -> NewResult = 'Проигрыш';
    Result = 'Проигрыш' -> NewResult = 'Выигрыш';
    NewResult = Result).

%Добавление нового соревнования
add_competition(Request):-
    http_parameters(
        Request,
        [
            rank(Rank, []),
            sport(Sport, []),
            year(Year, []),
            country(Country, []),
            teamName(TeamName, []),
            teamCountry(TeamCountry, []),
            opponentName(OpponentName, []),
            opponentCountry(OpponentCountry, []),
            result(Result, [])
        ]
    ),
    check_and_replace(Result, NewResult),
    add_or_update_competition(Rank, Sport, Year, Country, [team(TeamName, TeamCountry, [result(OpponentName, OpponentCountry, Result)])]),
     add_or_update_competition(Rank, Sport, Year, Country, [team(OpponentName, OpponentCountry, [result(TeamName, TeamCountry, NewResult)])]),
    %assertz(competition(Rank, Sport, Year, Country, [
     %                                          team(TeamName, TeamCountry, [result(OpponentName, OpponentCountry, Result)]),
     %                                          team(OpponentName, OpponentCountry, [result(TeamName, TeamCountry, NewResult)])])),
    http_redirect(moved, '/', _Request).

read_page(_Request):-
    reply_html_page(
        title('Чтение соревнования'),
        [form(
            [action=location_by_id(read_competition_page), method(post)],
            [
                table([
                    tr([th('Ранг'), td(input([name(rank)]))]),
                    tr([th('Вид спорта'), td(input([name(sport)]))]),
                    tr([th('Год'), td(input([name(year), type(number)]))]),
                    tr([th('Страна'), td(input([name(country)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Искать'])))
                ] )
            ]
        )]
    ).

% Обработка главной страницы
read_competition_page(Request):-
   http_parameters(
        Request,
        [
            rank(Rank, []),
            sport(Sport, []),
            year(Year, []),
            country(Country, [])
        ]
    ),
    % Добавляем заголовки таблицы
    generate_rows([Rank], [Sport], [Year], [Country], Row),

    % Выравниваем список
    flatten(Row, FlattenRow),

    ins(FlattenRow, tr(
                  [
                    th('Ранг Соревнования'),
                    th('Вид спорта'),
                    th('Год'),
                    th('Страна проведения'),
                    th('Команда'),
                    th('Страна'),
                    th('Оппонент'),
                    th('Страна оппонента'),
                    th('Результат')
                  ]
              ), Rows_with_Headers),

    reply_html_page(
        title('Таблица спортивных соревнований'),
        [
            h1('Спортивные соревнования'),

            table(
                [border(2)],
                Rows_with_Headers
            )
        ]).

% Cтраница с удалением соревнований
delete_page(_Request):-
    reply_html_page(
        title('Удаление соревнований'),
        [form(
            [action=location_by_id(delete_competition), method(post)],
            [
                table([
                    tr([th('Ранг'), td(input([name(rank)]))]),
                    tr([th('Вид спорта'), td(input([name(sport)]))]),
                    tr([th('Год'), td(input([name(year), type(number)]))]),
                    tr([th('Страна'), td(input([name(country)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Удалить'])))
                ] )
            ]
        )]
    ).

check_competition(Rank, Sport, Year, Country):-
   findall(Competition, competition(Rank, Sport, Year, Country, Competition), Competitions),
   length(Competitions, Length),
   Length \= 0 -> retractall(competition(Rank, Sport, Year, Country, _)).

% Удаление соревнований
delete_competition(Request):-
    http_parameters(
        Request,
        [
            rank(Rank, []),
            sport(Sport, []),
            year(Year, []),
            country(Country, [])
        ]
    ),
    %check_competition(Rank, Sport, Year, Country), % И так работает
    retractall(competition(Rank, Sport, Year, Country, _)),
    http_redirect(moved, '/', Request).

% Cтраница поиска соревнования для обновления
update_find_page(_Request):-
    reply_html_page(
        title('Поиск соревнования'),
        [form(
            [action=location_by_id(update_page), method(post)],
            [
                table([
                    tr([th('Ранг'), td(input([name(rank)]))]),
                    tr([th('Вид спорта'), td(input([name(sport)]))]),
                    tr([th('Год'), td(input([name(year), type(number)]))]),
                    tr([th('Страна'), td(input([name(country)]))]),
                    tr([th('Команда'), td(input([name(teamName)]))]),
                    tr([th('Страна команды'), td(input([name(teamCountry)]))]),
                    tr([th('Команда соперника'), td(input([name(opponentName)]))]),
                    tr([th('Страна команды соперника'), td(input([name(opponentCountry)]))]),
                    tr([th('Результат соревнования'), td(input([name(result)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Найти'])))
                ] )
            ]
        )]
    ).

% Cтраница обновления данных
update_page(Request):-
   http_parameters(
        Request,
        [
            rank(Rank, []),
            sport(Sport, []),
            year(Year, []),
            country(Country, []),
            teamName(TeamName, []),
            teamCountry(TeamCountry, []),
            opponentName(OpponentName, []),
            opponentCountry(OpponentCountry, []),
            result(Result, [])
        ]
    ),
   reply_html_page(
        title('Изменение соревнования'),
        [form(
            [action=location_by_id(update_competition), method(post)],
            [
                table([
                    tr([th('Команда'), td(input([name(teamName), value(TeamName)]))]),
                    tr([th('Страна команды'), td(input([name(teamCountry), value(TeamCountry)]))]),
                    tr([th('Команда соперника'), td(input([name(opponentName), value(OpponentName)]))]),
                    tr([th('Страна команды соперника'), td(input([name(opponentCountry), value(OpponentCountry)]))]),
                    tr([th('Результат соревнования'), td(input([name(result), value(Result)]))]),

                    tr(td([colspan(2), align(right)], input([type=submit, value='Изменить']))),


                    tr([th(''), td(input([name(activTeamName), value(TeamName), type(hidden)]))]),
                    tr([th(''), td(input([name(activTeamCountry), value(TeamCountry), type(hidden)]))]),
                    tr([th(''), td(input([name(activOpponentName), value(OpponentName), type(hidden)]))]),
                    tr([th(''), td(input([name(activOpponentCountry), value(OpponentCountry), type(hidden)]))]),
                    tr([th(''), td(input([name(rank), value(Rank), type(hidden)]))]),
                    tr([th(''), td(input([name(sport), value(Sport), type(hidden)]))]),
                    tr([th(''), td(input([name(year), value(Year), type(hidden)]))]),
                    tr([th(''), td(input([name(country), value(Country), type(hidden)]))]),
                    tr([th(''), td(input([name(activResult), value(Result), type(hidden)]))])
                ] )
            ]
        )]
    ).

% Предикат для удаления старых и добавления новых данных
update_data(OldData, NewData):-
   retract(OldData),
   asserta(NewData).

% Предикат для обновления соревнований
update_competition(Rank, Sport, Year, Country, OldTeams, NewTeams):-
   update_data(competition(Rank, Sport,Year, Country, OldTeams),
               competition(Rank, Sport,Year, Country, NewTeams)).

% Предикат для замены команд в списке с учетом изменения одного результата
replace_teams([], _, _, []).
replace_teams([OldTeam|RestOldTeams], ChangedTeam, UpdateTeam, [ModifiedTeam|NewTeams]) :-
   % Если текущая команда совпадает с командой, которую нужно изменить
   OldTeam = team(OldTeamName, OldCountry, OldResults),
   ChangedTeam = team(ActivTeamName, ActivCountry, [result(ActivOpponentName, ActivOpponentCountry, ActivResult)]),
   UpdateTeam = team(UpdateTeamName, UpdateCountry, [result(UpdateOpponentName, UpdateOpponentCountry, UpdateResult)]),
   OldTeamName = ActivTeamName,  OldCountry = ActivCountry,

   modify_team_results(OldResults, result(ActivOpponentName, ActivOpponentCountry, ActivResult), result(UpdateOpponentName, UpdateOpponentCountry, UpdateResult), ModifiedResults),
   %format(atom(Row), 'Match: ~w vs. ~w ~n', [OldTeam, ChangedTeam]),  % Выводим данные каждого матча
   %write(Row),  % Выводим данные каждого матча в консоль для отладки
   ModifiedTeam = team(UpdateTeamName, UpdateCountry, ModifiedResults),
   % Заменяем только одну команду с измененным результатом, остальные остаются без изменений
   replace_teams(RestOldTeams, ChangedTeam, UpdateTeam, NewTeams).
replace_teams([OldTeam|RestOldTeams], ChangedTeam, UpdateTeam, [OldTeam|NewTeams]) :-
   %OldTeam = team(OldTeamName, OldCountry, _),
   %ChangedTeam = team(ActivTeamName, ActivCountry, _),
   %OldTeamName \== ActivTeamName, OldCountry \== ActivCountry,
   %format(atom(Row), 'Match: ~w vs. ~w ~n', [OldTeamName=ActivTeamName, OldCountry \= ActivCountry]),  % Выводим данные каждого матча
   %write(Row),  % Выводим данные каждого матча в консоль для отладки

   % Если текущая команда не совпадает с командой для замены, оставляем ее без изменений
   replace_teams(RestOldTeams, ChangedTeam, UpdateTeam, NewTeams).

% Предикат для модификации результатов команды
modify_team_results([], _, _, []).
modify_team_results([OldResult|RestOldResults], ChangedResult, NewResult, [NewResult|ModifiedResults]) :-
    OldResult = ChangedResult,
    modify_team_results(RestOldResults, ChangedResult, NewResult, ModifiedResults).
modify_team_results([OldResult|RestOldResults], ChangedResult, NewResult, [OldResult|ModifiedResults]) :-
    OldResult \= ChangedResult,
    modify_team_results(RestOldResults, ChangedResult, NewResult, ModifiedResults).

% Обновление соревнования
update_competition(Request):-
    http_parameters(
        Request,
        [
            rank(Rank, []),
            sport(Sport, []),
            year(Year, []),
            country(Country, []),
            teamName(TeamName, []),
            teamCountry(TeamCountry, []),
            opponentName(OpponentName, []),
            opponentCountry(OpponentCountry, []),
            result(Result, []),
            activTeamName(ActivTeamName, []),
            activTeamCountry(ActivTeamCountry, []),
            activOpponentName(ActivOpponentName, []),
            activOpponentCountry(ActivOpponentCountry, []),
            activResult(ActivResult, [])
        ]
    ),

    % Получаем список Активных команд в БД по Rank/Sport/Year/Country, производим замену команд на новый список и обновляем соревнование
    competition(Rank, Sport, Year, Country, ActivTeams),
    replace_teams(ActivTeams,
                  team(ActivTeamName, ActivTeamCountry,[result(ActivOpponentName, ActivOpponentCountry, ActivResult)]),
                  team(TeamName, TeamCountry, [result(OpponentName, OpponentCountry, Result)]),
                 NewTeams),
    update_competition(Rank, Sport, Year, Country, ActivTeams, NewTeams),

    % Получаем список Новых команд в БД по Rank/Sport/Year/Country, изменяем результаты соревнований и производим аналогичные действия как выше
    competition(Rank, Sport, Year, Country, NewTeams),
    check_and_replace(Result, NewResult),
    check_and_replace(ActivResult, NewActivResult),
    replace_teams(NewTeams,
                  team(ActivOpponentName, ActivOpponentCountry,[result(ActivTeamName, ActivTeamCountry, NewActivResult)]),
                  team(OpponentName, OpponentCountry, [result(TeamName, TeamCountry, NewResult)]),
                 NewTeamsTwo),
    update_competition(Rank, Sport, Year, Country, NewTeams, NewTeamsTwo),


    http_redirect(moved, '/', _Request).
