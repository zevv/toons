defmodule Xpeg do
  
  require Logger
  
  @type inst :: { :nop } |
                { :any, integer } |
                { :chr, integer } |
                { :set, charset } |
                { :span, charset } |
                { :choice, integer, integer } |
                { :commit } |
                { :call, atom } |
                { :capopen } |
                { :capclose } |
                { :return } |
                { :fail } 
  
  @type charset :: MapSet.t(char)

  @type patt :: list(inst)

  @type astnode :: {any, list(), list()}

  @type grammar :: %{
    start: atom,
    rules: keyword(list(inst)),
  }

  @type subject :: charlist

  @type captures :: list(String.t)

  @type back_frame :: %{
    patt_back: patt,
    patt_commit: patt,
    ret_stack: list(patt),
    s: subject,
  }

  @type matchstate :: %{
    grammar: grammar,
    back_stack: list(back_frame),
    ret_stack: list(patt),
    cap_stack: list({atom, integer, subject}),
    result: :unknown | :ok | :error,
  }

  # Emit a choice/commit pair around pattern p; off_back and off_commit are the
  # offsets to the backtrack and commit targets, relative to the commit
  # instruction
  @spec choice_commit(patt, integer, integer) :: patt
  defp choice_commit(p, off_commit, off_back) do
    [{:choice, off_back, off_commit}] ++ p ++ [{:commit}]
  end

  # Generic ordered choice
  @spec mk_choice(patt, patt) :: patt
  defp mk_choice(p1, p2) do
    choice_commit(p1, length(p1)+length(p2)+2, length(p1)+2) ++ p2
  end

  # kleene-star-operator for sets make a :span
  @spec mk_star(patt) :: patt
  defp mk_star([set: cs]) do
    [{:span, cs}]
  end

  # Generic kleene-star operator
  defp mk_star(p) do
    choice_commit(p, 0, length(p)+2)
  end

  # Generic ! 'not' predicate
  @spec mk_not(patt) :: patt
  defp mk_not(p) do
    choice_commit(p, length(p)+2, length(p)+3) ++ [{:fail}]
  end

  # Generic optional
  @spec mk_optional(patt) :: patt
  defp mk_optional(p) do
    choice_commit(p, length(p)+2, length(p)+2)
  end

  # Minus for sets is the difference between sets
  @spec mk_minus(patt, patt) :: patt
  defp mk_minus([set: cs1], [set: cs2]) do
    [set: MapSet.difference(cs1, cs2)]
  end

  # Generic minus, !p2 * p1
  defp mk_minus(p1, p2) do
    List.flatten [ mk_not(p2), p1 ]
  end
  
  # Transform AST tuples into PEG IR
  @spec parse(astnode) :: patt
  defp parse({ id, _meta, args }) do
    #IO.inspect {"parse", id, args }
    case { id, args } do

      # List of named rules
      { :__block__, ps } ->
        Enum.map(ps, &parse/1) |> List.flatten

      # Named rule
      { :<-, [{label, _, nil}, patt] }            ->
        [{label, parse(patt) ++ [{:return}]}]
      { :<-, [{:__aliases__, _, [label]}, patt] } ->
        [{label, parse(patt) ++ [{:return}]}]

      # '*' Concatenation
      { :*, [p1, p2] } ->
        parse(p1) ++ parse(p2)

      # '|' Ordered choice
      { :|, [p1, p2] } ->
        mk_choice parse(p1), parse(p2)

      # '*' zero-or-more operator
      { :star, [p] } ->
        mk_star parse(p)

      # '?' one-or-zero operator
      { :question, [p] } ->
        mk_optional(parse(p))

      # '+' one-or-more operator
      { :+, [p] } ->
        p = parse(p); p ++ mk_star(p)
        { :-, [p1, p2] } ->
        mk_minus parse(p1), parse(p2)

      # '!' 'not' operator
      { :!, [p] } ->
        mk_not parse(p)

      # Charset
      { :{}, p } ->
        [{ :set, parse_set(p) }]

      # Repetition count
      {{:., _, [Access, :get]}, [p, count]} ->
        List.flatten List.duplicate(parse(p), count)

      # Call
      { label, nil }          ->
        [{:call, label}]
      { :__aliases__, [label]} ->
        [{:call, label}]

      # Capture
      { :cap, [p] } ->
        List.flatten [{:capopen}, parse(p), {:capclose}]
    end
  end

  # Transform AST literals into PEG IR
  @spec parse(atom | boolean | integer) :: patt
  defp parse(p) do
    case p do
      0 -> [{ :nop }]
      v when is_atom(v) -> v
      v when is_number(v) -> [{ :any, v }]
      v when is_binary(v) -> to_charlist(v) |> Enum.map(fn c -> { :chr, c } end)
      [v] -> [{ :chr, v }]
      v -> raise("Unhandled lit: #{inspect(v)}")
    end
  end

  # Transform AST character set to PEG IR. `{'x','y','A'-'F','0'}`
  @spec parse_set(astnode) :: MapSet.t(char)
  defp parse_set(ps) do
    Enum.map(ps, fn p ->
      case p do
        { :-, _, [[lo], [hi]] } -> Range.new(lo, hi) |> Enum.into([])
        { :-, _, [lo, hi] } -> Range.new(lo, hi) |> Enum.into([])
        v -> v
      end
    end)
    |> List.flatten
    |> MapSet.new
  end
  
  # PEG compilation macro: takes a grammar description in Elixir-AST
  @spec peg(String.t, list()) :: astnode
  defmacro peg(start, [{:do, v}]) do
    %{
      start: start,
      rules: parse(v),
    }
    |> IO.inspect
    |> Macro.escape
  end



  # Error handling: backtrack if possible, error out otherwise
  @spec backtrack(matchstate) :: matchstate
  defp backtrack(state) do
    Logger.debug("<<<")
    case state.back_stack do
      [frame | back_stack] ->
        state = %{state | back_stack: back_stack, ret_stack: frame.ret_stack }
        match(frame.patt_back, frame.s, state)
      [] ->
        %{state | result: :error}
    end
  end

  # Execute PEG IR to match the passed subject charlist
  @spec match(patt, subject, matchstate) :: matchstate
  defp match([inst|ptail]=patt, s, state) do

    ds = s |> inspect |> String.slice(1, 15) |> String.pad_trailing(15)
    di = inst |> Tuple.to_list |> Enum.map(&inspect/1) |> Enum.join(" ")
    Logger.debug("#{ds}|#{di}")

    case inst do

      { :nop } ->
        match(ptail, s, state)

      { :any, count } ->
        if length(s) >= count do
          match(ptail, Enum.drop(s, count), state)
        else
          backtrack(state)
        end

      { :chr, c } ->
        if length(s) > 0 and c == hd(s) do
          match(ptail, tl(s), state)
        else
          backtrack(state)
        end

      { :set, cs } ->
        if length(s) > 0 and MapSet.member?(cs, hd(s)) do
          match(ptail, tl(s), state)
        else
          backtrack(state)
        end

      { :span, cs } ->
        s = Enum.drop_while(s, fn c -> MapSet.member?(cs, c) end)
        match(ptail, s, state)

      { :choice, off_back, off_commit } ->
        frame = %{
          patt_back: Enum.drop(patt, off_back),
          patt_commit: Enum.drop(patt, off_commit),
          ret_stack: state.ret_stack,
          s: s,
        }
        state = %{state | :back_stack => [frame | state.back_stack]}
        match(ptail, s, state)

      { :commit } ->
        [frame | back_stack] = state.back_stack
        state = %{state | :back_stack => back_stack}
        match(frame.patt_commit, s, state)

      { :call, label } ->
        state = %{state | :ret_stack => [ptail | state.ret_stack]}
        case state.grammar.rules[label] do
          nil -> raise "Calling unknown rule '#{label}'"
          patt -> match(patt, s, state)
        end

      { :capopen } ->
        state = %{state | :cap_stack => [ {:open, length(s), s} | state.cap_stack]}
        match(ptail, s, state)

      { :capclose } ->
        state = %{state | :cap_stack => [ {:close, length(s), s} | state.cap_stack]}
        match(ptail, s, state)

      { :return } ->
        case state.ret_stack do
          [patt | ret_stack] ->
            match(patt, s, %{state | :ret_stack => ret_stack})
          [] ->
            %{state | :result => :ok}
        end

      { :fail } ->
        backtrack(state)

    end
  end


  # Match a subject against a grammar
  @spec match(grammar, String.t) :: captures
  defp match(grammar, s) do
    state = %{
      grammar: grammar,
      back_stack: [],
      ret_stack: [],
      cap_stack: [],
      result: :unknown
    }
    case grammar.rules[grammar.start] do
      nil -> raise "could not find initial rule '#{grammar.start}'"
      patt -> match(patt, to_charlist(s), state)
    end
    |> collect_captures
  end

  # Flatten the cap stack and collect the captures
  @spec collect_captures(matchstate) :: captures
  def collect_captures(state) do
    state.cap_stack
    |> Enum.reverse
    |> Enum.reduce({[], []}, fn frame, {acc, caps} ->
      case { frame, acc } do
        {{:open, _, _ }, _} ->
          {[frame | acc], caps}
        {{:close, oc, _ }, [{:open, oo, so} | t]} ->
          {t, [Enum.take(so, oo-oc) | caps]}
      end
    end)
    |> elem(1)
    |> Enum.reverse
  end

  def go do

    p = peg :flop do
      word <- cap(star({'a'-'z'}))
      flop <- cap(word * star(',' * word))
      UnicodeEscape  <-  Xdigit[4]

    end

    match(p, "one,two,three")

  end

  def test do
    parse {{:., [], [Access, :get]}, [], ["a", 3]}
  end

  #def gojs do

  #  p = peg :JSON do

  #    S              <- star({' ','\t','\r','\n'})
  #    True           <- "true"
  #    False          <- "false"
  #    Null           <- "null"

  #    Xdigit         <- {'0'-'9','a'-'f','A'-'F'}
  #    UnicodeEscape  <- 'u' * Xdigit[4]

  #    Escape         <- '\\' * ({ '"', '\\', '/', 'b', 'f', 'n', 'r', 't' } | UnicodeEscape)
  #    StringBody     <- star(Escape) * star( +( {32-255} - {'"'} - {'\\'}) * star(Escape) )
  #    String         <- '"' * StringBody * '"'

  #    Minus          <- '-'
  #    IntPart        <- '0' | {'1'-'9'} * star{'0'-'9'}
  #    FractPart      <- "." * +{'0'-'9'}
  #    ExpPart        <- ( 'e' | 'E' ) * question( '+' | '-' ) * +{'0'-'9'}
  #    Number         <- question(Minus) * IntPart * question(FractPart) * question(ExpPart)

  #    DOC            <- Value * !1
  #    ObjPair        <- S * String * S * ":" * Value
  #    Object         <- '{' * ( ObjPair * star( "," * ObjPair ) | S ) * "}"
  #    Array          <- "[" * ( Value * star( "," * Value ) | S ) * "]"
  #    Value          <- S * ( Number | String | Object | Array | True | False | Null ) * S

  #    JSON           <- Value * !1

  #  end

  #  match(p, """
  #    [ "look", "at", "this", { "thing": "parseing", "json": 3.1415 }, true, false ]
  #  """)

  #end

end

# set ft=elixir
