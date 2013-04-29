-module(tzdata_parser).
-export([parse/1, parse_offset/1]).

-define(RULE_RE, "^Rule\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)").
-define(ZONE_RE, "^Zone\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)(\\s+([0-9]+(\\s+.*)?))?$").
-define(IN_ZONE_RE, "^\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)(\\s+([0-9]+(\\s+.*)?))?$").
-define(LINK_RE, "^Link\\s+([^\\s]+)\\s+([^\\s]+)").
-define(OFFSET_RE, "^(-)?(?:([0-9]+)(?::([0-9]+)(?::([0-9]+))?)?)?$").

-include("tzdata.hrl").

parse(Filename) ->
  {ok, File} = file:open(Filename, [read, binary]),
  try parse_file(File, #context{rules = dict:new(), zones = dict:new()}) of
    C -> C#context.zones
  after
    file:close(File)
  end.

parse_file(File, Context) ->
  case file:read_line(File) of
    {ok, Line} ->
      Context2 = parse_line(strip_comments_and_trailing_whitespace(Line), Context),
      parse_file(File, Context2);
    eof -> Context;
    {error, Reason} ->
      throw(Reason)
  end.

strip_comments_and_trailing_whitespace(Line) ->
  Line2 = re:replace(Line, "#.*$", <<>>),
  Line3 = re:replace(Line2, "\\s+$", <<>>),
  iolist_to_binary(Line3).

parse_line(<<>>, C) -> C;

parse_line(Line, C = #context{in_zone = Name}) when Name =/= undefined ->
  case re:run(Line, ?IN_ZONE_RE, [{capture, all_but_first, binary}]) of
    {match, ObsData} ->
      parse_observance(Name, ObsData, C);
    nomatch -> C
  end;

parse_line(Line = <<"Rule", _/binary>>, C = #context{rules = Rules}) ->
  case re:run(Line, ?RULE_RE, [{capture, all_but_first, binary}]) of
    {match, [Name, From, To, Type, InMonth, OnDay, AtTime, Save, Letter]} ->
      NewRule = #rule{from = From, to = To, type = Type, in_month = InMonth, on_day = OnDay, at_time = AtTime, save = Save, letter = Letter},
      C#context{rules = dict:append(Name, NewRule, Rules)};
    nomatch -> C
  end;

parse_line(Line = <<"Zone", _/binary>>, C) ->
  case re:run(Line, ?ZONE_RE, [{capture, all_but_first, binary}]) of
    {match, [Name | ObsData]} ->
      parse_observance(Name, ObsData, C);
    nomatch -> C
  end;

parse_line(Line = <<"Link", _/binary>>, C = #context{zones = Zones}) ->
  case re:run(Line, ?LINK_RE, [{capture, all_but_first, binary}]) of
    {match, [To, From]} ->
      C#context{zones = dict:store(From, To, Zones)};
    nomatch -> C
  end;

parse_line(Line, _) ->
  throw({unknown_line, Line}).


parse_observance(Name, [UtcOffset, RuleSet, Format], C) ->
  NewObs = #obs{utc_offset = parse_offset(UtcOffset), rule_set = get_rules(RuleSet, C), format = Format},
  add_observance(Name, NewObs, undefined, C);

parse_observance(Name, [UtcOffset, RuleSet, Format, _, ValidUntil | _], C) ->
  NewObs = #obs{utc_offset = parse_offset(UtcOffset), rule_set = get_rules(RuleSet, C), format = Format, valid_until = ValidUntil},
  add_observance(Name, NewObs, Name, C).

add_observance(Name, Obs, InZone, C = #context{zones = Zones}) ->
  C#context{zones = dict:append(Name, Obs, Zones), in_zone = InZone}.


get_rules(Name = <<"-">>, _) -> #rule_set{name = Name};
get_rules(Name, C) ->
  case re:run(Name, "^[0-9]+:[0-9]+$") of
    {match, _} -> #rule_set{name = Name, offset = parse_offset(Name)};
    nomatch ->
      {ok, Rules} = dict:find(Name, C#context.rules),
      #rule_set{name = Name, count = length(Rules), rules = Rules}
  end.

parse_offset(Offset) ->
  case re:run(Offset, ?OFFSET_RE, [{capture, all_but_first, list}]) of
    {match, [Sign | Match]} ->
      [H, M, S] = case lists:map(fun(M) -> list_to_integer(M) end, Match) of
        [M1] -> [M1, 0, 0];
        [M1, M2] -> [M1, M2, 0];
        X -> X
      end,
      Seconds = (H * 60 + M) * 60 + S,
      case Sign of
        [] -> Seconds;
        _ -> - Seconds
      end;
    nomatch -> throw(invalid_offset)
  end.
