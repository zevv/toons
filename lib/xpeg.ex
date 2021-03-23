defmodule Xpeg do
  
  require Logger
  
  defp mk_choice(p1, p2) do
    List.flatten [
      {:choice, length(p1)+2, length(p1)+length(p2)+2},
      p1,
      {:commit},
      p2 # <- backtrack
    ] # <- commit
  end

  defp mk_star(p) do
    List.flatten [
      {:choice, length(p)+2, 0}, # <- commit
      p,
      {:commit}
    ] # <- backtrack

  end

  defp mk_not(p) do
    List.flatten [
      {:choice, length(p)+3, length(p)+2},
      p,
      {:commit},
      {:fail} # <- commit
    ] # <- backtrack
  end

  # Transform AST tuples into PEG IR

  defp parse({ id, _meta, args }) do
    #IO.inspect {"parse", id, args }
    case { id, args } do
      # List of rules
      { :__block__, ps } ->
        Enum.map(ps, &parse/1) |> List.flatten
      # Named rules
      { :<-, [{label, _, nil}, patt] } ->
        [{label, parse(patt) ++ [{:return}]}]
      { :<-, [{:__aliases__, _, [label]}, patt] } ->
        [{label, parse(patt) ++ [{:return}]}]
      # Concatenation
      { :*, [p1, p2] } ->
        parse(p1) ++ parse(p2)
      # Ordered choice
      { :|, [p1, p2] } ->
        mk_choice(parse(p1), parse(p2))
      # Star zero-or-more operator
      { :star, [p] } ->
        mk_star(parse(p))
      # Plus one-or-more operator
      { :+, [p] } ->
        p = parse(p)
        p ++ mk_star(p)
      # 'not' operator
      { :!, [p] } ->
        mk_not parse(p)
      # Charset
      { :{}, p } ->
        [{ :set, parse_set(p) }]
      # Repetition count
      {{:., _, [Access, :get]}, [p, count]} ->
        IO.inspect {"repeat"}
        List.duplicate parse(p), count
      # Call
      { label, nil } ->
        [{:call, label}]
      {:__aliases__, [label]} ->
        [{:call, label}]
    end
  end

  # Transform AST literals into PEG IR

  defp parse(p) do
    case p do
      0 ->
        [{ :nop }]
      v when is_atom(v) ->
        v
      v when is_number(v) ->
        [{ :any, v }]
      v when is_binary(v) ->
        to_charlist(v) |> Enum.map(fn c -> { :chr, c } end)
      [v] ->
        [{ :chr, v }]
      v ->
        IO.inspect {"Unhandled lit", v}
    end
  end

  # Transform AST character set to PEG IR. `{'x','y','A'-'F','0'}`

  defp parse_set(ps) do
    Enum.map(ps, fn p ->
      case p do
        { :-, _, [ [lo], [hi] ] } -> Range.new(lo, hi) |> Enum.into([])
        v -> v
      end
    end)
    |> List.flatten
    |> MapSet.new
  end

  # Error handling: backtrack if possible, error out otherwise

  defp backtrack(state) do
    Logger.debug("backtrack")
    case state.back_stack do
      [frame | back_stack] ->
        state = %{state | back_stack: back_stack }
        #IO.inspect {"error", frame.s, state}
        match(frame.patt_back, frame.s, state)
      [] ->
        %{state | result: :error}
    end
  end

  # Match a subjects against a grammar

  defp match(grammar, s) do
    state = %{
      grammar: grammar,
      back_stack: [],
      call_stack: [],
      result: :unknown
    }
    patt = grammar.rules[grammar.start]
    state = match(patt, to_charlist(s), state)
    state.result
  end


  # Execute PEG IR to match the passed subject charlist

  defp match(patt, s, state) do

    [inst | ptail] = patt
    Logger.debug("#{inspect(inst)} - '#{s}'")

    case inst do

      { :chr, c } -> 
        if c == hd(s) do
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

      { :choice, off_back, off_commit } ->
        frame = %{
          patt_back: Enum.drop(patt, off_back),
          patt_commit: Enum.drop(patt, off_commit),
          s: s,
        }
        state = %{state | :back_stack => [ frame | state.back_stack ]}
        match(ptail, s, state)

      { :commit } ->A
        [frame | back_stack] = state.back_stack
        state = %{state | :back_stack => back_stack}
        Logger.debug "commit #{inspect(frame)}" 
        match(frame.patt_commit, s, state)

      { :any, count } ->
        if length(s) >= count do
          match(ptail, Enum.drop(s, count), state)
        else
          backtrack(state)
        end

      { :call, label } ->
        state = %{state | :call_stack => [ptail | state.call_stack]}
        match(state.grammar.rules[label], s, state)

      { :return } ->
        case state.call_stack do
          [patt | call_stack] ->
            match(patt, s, %{state | :call_stack => call_stack})
          [] ->
            Logger.debug("done, leftover: #{s}")
            %{state | :result => :ok}
        end

      { :nop } ->
        match(ptail, s, state)

      { :fail } ->
        backtrack(state)

    end
  end

  # PEG compilation macro: takes a grammar description in Elixir-AST
 
  defmacro peg(start, grammar) do
    [ {:do, v } ] = grammar
    %{
      start: start,
      rules: parse(v),
    }
    |> IO.inspect
    |> Macro.escape
  end

  # Single-pattern compilation macro
  defmacro patt(p) do
    Macro.escape parse(p).result
  end
  

  def go do

    if true do
      p = peg :JSON do

        S              <- star({' ','\t','\r','\n'})
        True           <- "true"
        False          <- "false"
        null           <- "null"

        #Xdigit         <- {'0'-'9','a'-'f','A'-'F'}
        #UnicodeEscape  <- 'u' * Xdigit[4]
       
        #Escape         <- '\\' * ({ '"', '\\', '/', 'b', 'f', 'n', 'r', 't' } | UnicodeEscape)
        #StringBody     <- *Escape * *( +( {'\x20'..'\xff'} - {'"'} - {'\\'}) * *Escape) 
        #String         <- '"' * StringBody * '"'

        #Minus          <- '-'
        #IntPart        <- '0' | {'1'..'9'} * *{'0'..'9'}
        #FractPart      <- "." * +{'0'..'9'}
        #ExpPart        <- ( 'e' | 'E' ) * ?( '+' | '-' ) * +{'0'..'9'}
        #Number         <- ?Minus * IntPart * ?FractPart * ?ExpPart

        #DOC            <- Value * !1
        #ObjPair        <- S * String * S * ":" * Value
        #Object         <- '{' * ( ObjPair * *( "," * ObjPair ) | S ) * "}"
        #Array          <- "[" * ( Value * *( "," * Value ) | S ) * "]"
        #Value          <- S * ( Number | String | Object | Array | True | False | Null ) * S
        Value          <- S * ( True | False | Null ) * S

        JSON <- 'a' * !1
        #JSON           <- Value * !1

      end

      match(p, "a")
    end

  end

end

# set ft=elixir
