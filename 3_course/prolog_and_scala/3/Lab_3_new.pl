% ������� ��� ������ ��
:- http_handler(root(reset_DB), resetDB, [method(post)]).

% ����������� 5-�� ������� �������� � ��
:- dynamic competition/5.
:- dynamic team/3.
:- dynamic result/4.

% ������ �������
server(Port):- http_server(http_dispatch, [port(Port)]).
server:-server(8080).

% ��������� �������
stop(Port):- http_stop_server(Port, http_dispatch).
stop:- stop(8080).

resetDB(_):-
   % ������� ��
   retractall(competition(_, _, _, _, _)),
   retractall(team(_, _, _)),
    retractall(result(_, _, _, _)),

   % ������������� ��
    assert(competition('����������� ����', '������ ��������', 2024, '�������',
            [team('������� ���', '���',
                  [result('������� �����', '�����', '��������'),
                   result('������� ������', '������', '�������')]),
             team('������� ��������', '��������',
                  [result('������� ���', '���', '��������'),
                   result('������� ������', '������', '��������')])
            ])),

   asserta(competition('���������', '������', 2020, '������',
            [team('������� ������', '������',
                  [result('������� �������', '�������', '�������'),
                   result('������� �������', '�������', '��������')]),
            team('������� �������', '�������',
                  [result('������� ������', '������', '��������'),
                   result('������� �������', '�������', '��������')]),
            team('������� �������', '�������',
                  [result('������� ������', '������', '�������'),
                   result('������� �������', '�������', '�������')])
            ])),

   asserta(competition('���������', '���������', 2021, '���',
            [team('������� ���', '���',
                  [result('������� ������', '������', '�������'),
                   result('������� �������', '�������', '�������'),
                   result('������� �������', '�������', '��������')]),
            team('������� ������', '������',
                  [result('������� ���', '���', '��������'),
                   result('������� �������', '�������', '�����'),
                   result('������� �������', '�������', '�������')]),
            team('������� �������', '�������',
                  [result('������� ���', '���', '��������'),
                   result('������� ������', '������', '�����'),
                   result('������� �������', '�������', '�������')]),
            team('������� �������', '�������',
                  [result('������� ���', '���', '�������'),
                   result('������� ������', '������', '��������'),
                   result('������� �������', '�������', '��������')])
            ])),

   asserta(competition('��������� ����', '������', '2022', '������',
	[team('�����', '������',
              [result('�������', '������', '�������')]),
         team('�������', '������',
              [result('�����', '������', '��������')])
        ])),

   asserta(competition('������', '��������', 2023, '��������',
            [team('���� �������', '��������',
                  [result('����� �������', '��������', '��������'),
                   result('���� �����', '��������', '�������')]),
            team('����� �������', '��������',
                  [result('���� �������', '��������', '�������'),
                   result('���� �����', '��������', '�����')]),
            team('���� �����', '��������',
                  [result('����� �������', '��������', '�����'),
                   result('���� �������', '��������', '��������')])
            ])),

   asserta(competition('����������� ����', '������', 2016, '���',
           [team('������� ���', '���',
                 [result('������� ������', '������', '��������')]),
           team('������� ������', '������',
                 [result('������� �����', '�����', '�������')]),
           team('������� �����', '�����',
                 [result('������� ������', '������', '��������')]),
           team('������� ������', '������',
                 [result('������� ���', '���', '�������')]),
           team('������� ����', '����',
                 [result('������� ������', '������', '�������')]),
           team('������� ������', '������',
                 [result('������� ����', '����', '��������')])
           ])),

   http_redirect(moved, '/', _Request).

% ��������� ������� ��������
home_page(_Request):-
    % ��������  ���� ������������ � ������
    findall(Rank, competition(Rank, _, _, _, _), Ranks),

    % ��������  ���� ������������ � ������
    findall(Sport, competition(_, Sport, _, _, _), Sports),

    % ��������  ���� ������������ � ������
    findall(Year, competition(_, _, Year, _, _), Years),

    % ��������  ���� ������������ � ������
    findall(Country, competition(_, _, _, Country, _), Countries),

    % ��������  ���� ������������ � ������
    %findall(Teams, competition(_, _, _, _, Teams), TeamsList),

    % ��������� ��������� �������
    generate_rows(Ranks, Sports, Years, Countries, Row),

    % ����������� ������
    flatten(Row, FlattenRow),

    ins(FlattenRow, tr(
                  [
                    th('���� ������������'),
                    th('��� ������'),
                    th('���'),
                    th('������ ����������'),
                    th('�������'),
                    th('������'),
                    th('��������'),
                    th('������ ���������'),
                    th('���������')
                  ]
              ), Rows_with_Headers),

    reply_html_page(
        title('������� ���������� ������������'),
        [
            h1('���������� ������������'),

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
                p(button([type(submit), formaction(location_by_id('resetDB'))], '�������� ��'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/add')], '�������� ������������'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/update_find')], '�������� ������������'))
            ),

            form(
                [style('display: inline-block')],
                p(button([type(submit), formaction('/delete')], '������� ������������'))
            ),

            table(
                [border(2)],
                Rows_with_Headers
            )
        ]).

% ���������� �������� � ������ ������
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

    % �������� ������ ������ ��� ������� Rank/Sport/Year/Country
    competition(Rank, Sport, Year, Country, TeamsList),

    % ���������� ������ ��� ������ ������� � �� �����������
    generate_team_rows(Rank, Sport, Year, Country, TeamsList, Rows),
    generate_rows(RestRanks, RestSports, RestYears, RestCountries, RestRows).



generate_team_rows(_, _, _, _,[], []).
generate_team_rows(Rank,
                   Sport,
                   Year,
                   Country,
                   [team(TeamName, TeamCountry, Results)|RestTeams], [TeamRow|RestRow]):-
        generate_result_rows(Rank, Sport, Year, Country, TeamName, TeamCountry, Results, TeamRow),
        %format(atom(Row), 'Team: ~w, Country: ~w', [TeamName, TeamCountry]),  % ������� ������ ������� �����
        %write(Row),  % ������� ������ ������� ����� � ������� ��� �������
        generate_team_rows(Rank, Sport, Year, Country, RestTeams, RestRow).

generate_result_rows(_, _, _, _, _, _, [], []).  % ������� ������ - ���������� ��������

generate_result_rows(Rank,
                     Sport,
                     Year,
                     Country,
                     TeamName,
                     TeamCountry,
                     [result(Opponent, OpponentCountry, Outcome)|RestResults],
                     [tr([td(Rank), td(Sport), td(Year), td(Country), td(TeamName), td(TeamCountry),
                          td(Opponent), td(OpponentCountry), td(Outcome)])|RestRows]):-
    %format(atom(Row), 'Match: ~w vs. ~w - ~w', [TeamName, Opponent, Outcome]),  % ������� ������ ������� �����
    %write(Row),  % ������� ������ ������� ����� � ������� ��� �������
    generate_result_rows(Rank, Sport, Year, Country, TeamName, TeamCountry, RestResults, RestRows).


% 1 ����� ���, � ������� ����������� ������������ ����� ������, �
% �������� ����� ������������;
maxTeamsYearList(Rank, YearRow) :-
    findall(NumTeams, (competition(Rank, _, Year, _, Teams),
                       length(Teams, NumTeams)), NumTeamsList),
    max_list(NumTeamsList, MaxNumTeams),
    findall(Year, (competition(Rank, _, Year, _, Teams),
                   length(Teams, MaxNumTeams)), YearList),
    generate_rows(YearList, YearRow).


max_teams_year_find_page(_Request):-
   reply_html_page(
        title('���� ����� ������������'),
        [form(
            [action=location_by_id(max_teams_year_page), method(post)],
            [
                table([
                    tr([th('����'), td(input([name(rank)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='������'])))
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
                    th('���')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('���, � ������������ ������ ������'),
        [
            h1('���, � ������������ ������ ������'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).

% 2 ����� ��� ������, � ������� ��������� �������� �������;
sportByTeam(TeamName, SportsRow) :-
    %competition(_, Sport, _, _, Teams),
    %member(team(TeamName, _, _), Teams),
    findall(Sport, (competition(_, Sport, _, _, Teams),
                    member(team(TeamName, _, _), Teams)), Sports),
    generate_rows(Sports, SportsRow).

sport_by_team_find_page(_Request):-
   reply_html_page(
        title('���� �������� �������'),
        [form(
            [action=location_by_id(sport_by_team_page), method(post)],
            [
                table([
                    tr([th('�������� �������'), td(input([name(teamName)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='������'])))
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
                    th('��� ������')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('��� ������, � ������� ��������� �������� �������'),
        [
            h1('��� ������, � ������� ��������� �������� �������'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).


% 3 ����� ��� �������, ������� ����������� � ����������� ����� ��
% ������������� ���� ������.
olympicTeamsBySport(Sport, TeamsRow) :-
    findall(Team,(competition('����������� ����', Sport, _, _, TeamsList),
                  member(team(Team, _, _), TeamsList)), Teams),
    generate_rows(Teams, TeamsRow).

olympc_teams_by_sport_find_page(_Request):-
   reply_html_page(
        title('���� �������� ������'),
        [form(
            [action=location_by_id(olympc_teams_by_sport_page), method(post)],
            [
                table([
                    tr([th('�������� ���� ������'), td(input([name(sport)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='������'])))
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
                    th('�������� �������')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('�������, ����������� � ����������� ����� � ������������ ���� ������'),
        [
            h1('�������, ������� ����������� � ����������� ����� � ������������ ���� ������'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).


% 4 ����� ��� �������, ������������� � ������������� � �������� ����;
teamsInYear(Year, TeamsRow) :-
    findall(Team, (competition(_, _, Year, _, TeamsList),
                   member(team(Team, _, _), TeamsList)), Teams),
    generate_rows(Teams, TeamsRow).

teams_in_year_find_page(_Request):-
   reply_html_page(
        title('���� ����'),
        [form(
            [action=location_by_id(teams_in_year_page), method(post)],
            [
                table([
                    tr([th('���'), td(input([name(year), type(number)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='������'])))
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
                    th('�������� �������')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('�������, ������������� � ������������� � �������� ����'),
        [
            h1('�������, ������������� � ������������� � �������� ����'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).


% 5 ����� ��� ������� ������������ ������, � ������� ���� ��������.
winningTeamsByCountry(Country, TeamsRow) :-
    findall(Team,
            (competition(_, _, _, _, TeamsList),
             member(team(Team, Country, Results), TeamsList),
             member(result(_, _, '�������'), Results)), Teams),
    generate_rows(Teams, TeamsRow).

winning_teams_by_country_find_page(_Request):-
   reply_html_page(
        title('���� ������'),
        [form(
            [action=location_by_id(winning_teams_by_country_page), method(post)],
            [
                table([
                    tr([th('������'), td(input([name(country)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='������'])))
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
                    th('�������� �������')
                  ]
              ), Rows_with_Headers),
    reply_html_page(
        title('������� ������������ ������, � ������� ���� ��������'),
        [
            h1('������� ������������ ������, � ������� ���� ��������'),
            table(
                [border(2)],
                Rows_with_Headers
            )
        ]
    ).

% CRUD (Create/Read/Update/Delete)��� ����� ���� ������.
% C������� � ����������� ������ ������������
add_page(_Request):-
    reply_html_page(
        title('���������� ������������'),
        [form(
            [action=location_by_id(add_competition), method(post)],
            [
                table([
                    tr([th('����'), td(input([name(rank)]))]),
                    tr([th('��� ������'), td(input([name(sport)]))]),
                    tr([th('���'), td(input([name(year), type(number)]))]),
                    tr([th('������'), td(input([name(country)]))]),
                    tr([th('�������'), td(input([name(teamName)]))]),
                    tr([th('������ �������'), td(input([name(teamCountry)]))]),
                    tr([th('������� ���������'), td(input([name(opponentName)]))]),
                    tr([th('������ ������� ���������'), td(input([name(opponentCountry)]))]),
                    tr([th('��������� ������������'), td(input([name(result)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='��������'])))
                ] )
            ]
        )]
    ).

% ��������, ������� ����� ��������� �������� � �������������� ��������
% ����������
check_and_replace(Result, NewResult) :-
    (Result = '�������' -> NewResult = '��������';
    Result = '��������' -> NewResult = '�������';
    NewResult = Result).

%���������� ������ ������������
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

% C������� � ��������� ������������
delete_page(_Request):-
    reply_html_page(
        title('�������� ������������'),
        [form(
            [action=location_by_id(delete_competition), method(post)],
            [
                table([
                    tr([th('����'), td(input([name(rank)]))]),
                    tr([th('��� ������'), td(input([name(sport)]))]),
                    tr([th('���'), td(input([name(year), type(number)]))]),
                    tr([th('������'), td(input([name(country)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='�������'])))
                ] )
            ]
        )]
    ).


% �������� ������������
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

% C������� ������ ������������ ��� ����������
update_find_page(_Request):-
    reply_html_page(
        title('����� ������������'),
        [form(
            [action=location_by_id(update_page), method(post)],
            [
                table([
                    tr([th('����'), td(input([name(rank)]))]),
                    tr([th('��� ������'), td(input([name(sport)]))]),
                    tr([th('���'), td(input([name(year), type(number)]))]),
                    tr([th('������'), td(input([name(country)]))]),
                    tr([th('�������'), td(input([name(teamName)]))]),
                    tr([th('������ �������'), td(input([name(teamCountry)]))]),
                    tr([th('������� ���������'), td(input([name(opponentName)]))]),
                    tr([th('������ ������� ���������'), td(input([name(opponentCountry)]))]),
                    tr([th('��������� ������������'), td(input([name(result)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='�����'])))
                ] )
            ]
        )]
    ).

% C������� ���������� ������
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
        title('��������� ������������'),
        [form(
            [action=location_by_id(update_competition), method(post)],
            [
                table([
                    tr([th('����'), td(input([name(rank), value(Rank)]))]),
                    tr([th('��� ������'), td(input([name(sport), value(Sport)]))]),
                    tr([th('���'), td(input([name(year), type(number), value(Year)]))]),
                    tr([th('������'), td(input([name(country), value(Country)]))]),
                    tr([th('�������'), td(input([name(teamName), value(TeamName)]))]),
                    tr([th('������ �������'), td(input([name(teamCountry), value(TeamCountry)]))]),
                    tr([th('������� ���������'), td(input([name(opponentName), value(OpponentName)]))]),
                    tr([th('������ ������� ���������'), td(input([name(opponentCountry), value(OpponentCountry)]))]),
                    tr([th('��������� ������������'), td(input([name(result), value(Result)]))]),
                    tr(td([colspan(2), align(right)], input([type=submit, value='��������'])))
                ] )
            ]
        )]
    ).


% ���������� ������������
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
