defmodule Xpeg do
  
  require Logger
  
  defp mk_choice(p1, p2) do
    [{:choice, length(p1)+2}] ++ p1 ++ [{:commit, length(p2)+1}] ++ p2 
  end

  defp mk_star(p) do
    [{:choice, length(p)+2}] ++ p ++ [{:commit, -length(p)-1}]
  end

  defp mk_not(p) do
    [{:choice, length(p)+3}] ++ p ++ [{:commit, 0}, {:fail}]
  end

  # Transform AST tuples into PEG IR
  defp parse({ id, _meta, args }) do
    case { id, args } do
      { :__block__, ps } ->
        Enum.map(ps, fn p -> parse(p) end)
      { :<-, [{label, _, _}, patt] } ->
        {label, parse(patt) ++ [{:return}]}
      { :*, [p1, p2] } ->
        parse(p1) ++ parse(p2)
      { :|, [p1, p2] } ->
        mk_choice(parse(p1), parse(p2))
      { :^, [p] } ->
        mk_star(parse(p))
      { :+, [p] } ->
        p = parse(p)
        p ++ mk_star(p)
      { :{}, p } ->
        [{ :set, parse_set(p) }]
      { :!, [p] } ->
        mk_not parse(p)
      { label, _ } ->
        [{:call, label}]
    end
  end

  # Transform AST literals into PEG IR
  defp parse(p) do
    case p do
      0 ->
        [{ :nop }]
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

  defp error(state) do
    case state.back_stack do
      [frame | back_stack] ->
        state = %{state | back_stack: back_stack }
        IO.inspect {"error", frame.s, state}
        match(frame.patt, frame.s, state)
      [] ->
        %{state | result: :err}
    end
  end
  
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
    Logger.debug("#{inspect(inst)} - #{s}")

    case inst do

      { :chr, c } -> 
        if c == hd(s) do
          match(ptail, tl(s), state)
        else
          error(state)
        end

      { :set, cs } ->
        if MapSet.member?(cs, hd(s)) do
          match(ptail, tl(s), state)
        else
          error(state)
        end

      { :choice, offset } ->
        frame = %{
          patt: Enum.drop(ptail, offset-1),
          s: s,
        }
        state = %{state | :back_stack => [ frame | state.back_stack ]}
        match(ptail, s, state)

      { :commit, offset } ->
        state = %{state | :back_stack => tl(state.back_stack)}
        Logger.info "commit #{inspect(ptail)}" 
        match(Enum.drop(ptail, offset), s, state)

      { :any, count } ->
        if length(s) >= count do
          match(ptail, Enum.drop(s, count), state)
        else
          error(state)
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
        error(state)

    end
  end

  defmacro peg(start, grammar) do
    [ {:do, v } ] = grammar
    %{
      start: start,
      rules: parse(v),
    }
    |> IO.inspect
    |> Macro.escape
  end
  
  defmacro patt(p) do
    Macro.escape parse(p)
  end
  

  def go do

    if false do
      p1 = patt '0' * ('a' * 'b' | "cd") * 'e'
      IO.inspect {:patt, p1}
      IO.inspect match(p1, "0abe") == :ok
      IO.inspect match(p1, "0cde") == :ok
      IO.inspect match(p1, "0cdf") == :err
    end

    if false do
      p2 = patt 6 | 5
      IO.inspect {:patt, p2}
      IO.inspect match(p2, "abcde") == :ok
    end

    if false do
      p3 = patt ^"ab"
      IO.inspect {:patt, p3}
      IO.inspect match(p3, "ababc") == :ok
    end
    
    if false do
      #p3 = parse quote do: {'_','a'-'z','0'-'9'}
      p3 = patt "0" * !1
      IO.inspect(p3)
      IO.inspect match(p3, "01")
    end


    if true do
      p = peg :line do
        line <- open * name * close * open
        open <- '('
        close <- ')'
        name <- "a"
      end

      match(p, ")(a)(")
    end

  end

end

# set ft=elixir
