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
    assert(competition('Олимпийские игры', 'Легкая атлетика', 2024, 'Франция',
            [team('Сборная США', 'США',
                  [result('Сборная Кении', 'Кения', 'Проигрыш'),
                   result('Сборная Ямайки', 'Ямайка', 'Выигрыш')]),
             team('Сборная Германии', 'Германия',
                  [result('Сборная США', 'США', 'Проигрыш'),
                   result('Сборная Ямайки', 'Ямайка', 'Проигрыш')])
            ])),

   asserta(competition('Чемпионат', 'Футбол', 2020, 'Англия',
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

   asserta(competition('Чемпионат', 'Баскетбол', 2021, 'США',
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

   asserta(competition('Турнир', 'Волейбол', 2023, 'Бразилия',
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

   asserta(competition('Олимпийские игры', 'Борьба', 2016, 'США',
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
                p(button([type(submit), formaction('/update_find')], 'Изменить соревнование'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/delete')], 'Удалить соревнования'))
            ),

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
    findall(Team, (competition(_, _, Year, _, TeamsList),
                   member(team(Team, _, _), TeamsList)), Teams),
    generate_rows(Teams, TeamsRow).

teams_in_year_find_page(_Request):-
   reply_html_page(
        title('Ввод года'),
        [form(
            [action=location_by_id(teams_in_year_page), method(post)],
            [
                table([
                    tr([th('Год'), td(input([name(year), type(number)]))]),
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
             member(result(_, _, 'Выигрыш'), Results)), Teams),
    generate_rows(Teams, TeamsRow).

winning_teams_by_country_find_page(_Request):-
   reply_html_page(
        title('Ввод страны'),
        [form(
            [action=location_by_id(winning_teams_by_country_page), method(post)],
            [
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

    assertz(competition(Rank, Sport, Year, Country, [
                                               team(TeamName, TeamCountry, [result(OpponentName, OpponentCountry, Result)]),
                                               team(OpponentName, OpponentCountry, [result(TeamName, TeamCountry, NewResult)])])),
    http_redirect(moved, '/', _Request).

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
    retract(competition(Rank, Sport, Year, Country, _)),
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
   %check_and_replace(Result, NewResult),
   %retract(competition(Rank, Sport, Year, Country, team(TeamName, TeamCountry, result(OpponentName, OpponentCountry, Result)))),
   %retract(competition(Rank, Sport, Year, Country, team(OpponentName, OpponentCountry, result(TeamName, TeamCountry, NewResult)))),
    reply_html_page(
        title('Изменение соревнования'),
        [form(
            [action=location_by_id(update_competition), method(post)],
            [
                table([
                    tr([th('Ранг'), td(input([name(rank), value(Rank)]))]),
                    tr([th('Вид спорта'), td(input([name(sport), value(Sport)]))]),
                    tr([th('Год'), td(input([name(year), type(number), value(Year)]))]),
                    tr([th('Страна'), td(input([name(country), value(Country)]))]),
                    tr([th('Команда'), td(input([name(teamName), value(TeamName)]))]),
                    tr([th('Страна команды'), td(input([name(teamCountry), value(TeamCountry)]))]),
                    tr([th('Команда соперника'), td(input([name(opponentName), value(OpponentName)]))]),
                    tr([th('Страна команды соперника'), td(input([name(opponentCountry), value(OpponentCountry)]))]),
                    tr([th('Результат соревнования'), td(input([name(result), value(Result)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='Изменить'])))
                ] )
            ]
        )]
    ).


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
            result(Result, [])
        ]
    ),

    check_and_replace(Result, NewResult),

    assertz(competition(Rank, Sport, Year, Country, [
                                               team(TeamName, TeamCountry, [result(OpponentName, OpponentCountry, Result)]),
                                               team(OpponentName, OpponentCountry, [result(TeamName, TeamCountry, NewResult)])])),
    http_redirect(moved, '/', _Request).
