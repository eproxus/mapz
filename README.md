<h1 align="center">mapz</h1>

<p align="center">
  <a href="https://github.com/eproxus/mapz/actions/workflows/continous_integration.yaml?query=branch%3Amaster">
    <img alt="continous integration" src="https://img.shields.io/github/actions/workflow/status/eproxus/mapz/continous_integration.yaml?label=build&style=flat-square&branch=master"/>
  </a>
  <a href="https://hex.pm/packages/mapz">
    <img alt="hex.pm version" src="https://img.shields.io/hexpm/v/mapz?style=flat-square"/>
  </a>
  <a href="LICENSE">
    <img alt="hex.pm license" src="https://img.shields.io/hexpm/l/mapz?style=flat-square"/>
  </a>
  <a href="https://github.com/eproxus/mapz/blob/master/.github/workflows/continous_integration.yaml#L14">
    <img alt="erlang versions" src="https://img.shields.io/badge/erlang-23+-blue.svg?style=flat-square"/>
  </a>
</p>

<p align="center">
  <a href="https://hexdocs.pm/mapz/mapz.html">Function Reference</a>
</p><br/>

`mapz` contains additions to the Erlang maps module, mostly functionality to nested map usage. It tries to stay as true as possible to the original `maps` API with the same or similar errors where applicable.

## Features

### Deep Access

#### Getting & Setting

```erlang
1> Map = #{a => #{big => #{nested => #{map => with},some => values},
..         in => it,like => "hello"}}.
#{a =>
      #{big => #{nested => #{map => with},some => values},
        in => it,like => "hello"}}
2> mapz:deep_get([a, big, nested, map], Map).
with
3> mapz:deep_search([a, big, nested, list], Map).
{error,[a,big,nested],#{map => with}}
4> mapz:deep_remove([a, like], Map).
#{a =>
      #{big => #{nested => #{map => with},some => values},
        in => it}}
```

#### Merging

```erlang
5> NewMap = mapz:deep_merge(Map, #{a => #{big => #{nested => #{list => [1,2,3]}}}}).
#{a =>
      #{big =>
            #{nested => #{list => [1,2,3],map => with},some => values},
        in => it,like => "hello"}}
6> mapz:deep_search([a, big, nested, list], NewMap).
{ok,[1,2,3]}
```

#### Inverse

```erlang
1> mapz:inverse(#{a => 1, b => 2, c => 3}).
#{1 => a,2 => b,3 => c}
```
