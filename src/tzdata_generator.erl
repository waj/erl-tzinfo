-module(tzdata_generator).
-export([generate/1]).
-include("tzdata.hrl").

generate({Rules, Zones}) ->
  dict:fold(fun (ZoneName, ZoneData, _) ->
    generate_module(Rules, ZoneName, ZoneData),
    acc
  end, acc, Zones).

generate_module(_, ZoneName, Link) when is_binary(Link) ->
  io:format("~s -> ~s~n", [binary_to_list(ZoneName), binary_to_list(Link)]);

generate_module(Rules, ZoneName, []) -> ok;
generate_module(Rules, ZoneName, Observances) ->
  find_transitions(Observances).


find_transitions(Observances) ->
  StartTime = undefined,
  UntilTime = undefined,

  find_transitions(Observances, undefined, undefined, [], 0).

find_transitions([Obs | Rest], StartTime, UntilTime, Transitions, I) ->
  ok.
  % case Obs#obs.rule_set#rule_set.count of
  %   0 ->

