defmodule Xpeg do
  
  require Logger
  
  defp mk_choice(p1, p2) do
    List.flatten [ { :choice, length(p1) + 2 }, p1, { :commit, length(p2) + 1 }, p2 ]
  end

  defp mk_star(p) do
    List.flatten [ { :choice, length(p) + 2 }, p, { :commit, -length(p) - 1 }, ]
  end

  defp mk_not(p) do
    List.flatten [ { :choice, length(p) + 3 }, p, { :commit, 0 }, { :fail } ]
  end

  defp parse({ id, meta, args }, grammar) do
    IO.inspect {"parse", id, args}
    case { id, args } do
      { :<-, [{name, _, _}, patt] } ->
        IO.inspect({"rule", name, patt})
      { :*, [p1, p2] } ->
        List.flatten [parse(p1, grammar), parse(p2, grammar)]
      { :|, [p1, p2] } ->
        mk_choice(parse(p1, grammar), parse(p2, grammar))
      { :star, [p] } ->
        mk_star(parse(p, grammar))
      { :+, [p] } ->
        p = parse(p, grammar)
        List.flatten [p, mk_star(p)]
      { :{}, p } ->
        [{ :set, parse_set(p) }]
      { :!, [p] } ->
        mk_not parse(p, grammar)
      { _, _ } ->
        IO.inspect({"unhandled", id, meta, args})
    end
  end
  
  defp parse(p, _grammar) do
    IO.inspect {"lit", p}
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

  defp parse(p) do
    parse(p, %{})
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
  
  defp match(patt, s) do
    state = %{
      back_stack: [],
      result: :unknown
    }
    state = match(patt, to_charlist(s), state)
    state.result
  end

  defp match([], s, state) do
    Logger.debug("leftover: #{s}")
    %{state | :result => :ok}
  end

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
        state = %{ state |
          :back_stack => [ frame | state.back_stack ]
        }
        match(ptail, s, state)

      { :commit, offset } ->
        state = %{ state |
          :back_stack => tl(state.back_stack)
        }
        Logger.info "commit #{inspect(ptail)}" 
        match(Enum.drop(ptail, offset), s, state)

      { :any, count } ->
        if length(s) >= count do
          match(ptail, Enum.drop(s, count), state)
        else
          error(state)
        end

      { :nop } ->
        match(ptail, s, state)

      { :fail } ->
        error(state)

    end
  end


  def go do
    IO.puts("hello")

    if false do
      p1 = parse quote do: '0' * ('a' * 'b' | "cd") * 'e'
      IO.inspect {:patt, p1}
      IO.inspect match(p1, "0abe") == :ok
      IO.inspect match(p1, "0cde") == :ok
      IO.inspect match(p1, "0cdf") == :err
    end

    if false do
      p2 = parse quote do: 6 | 5
      IO.inspect {:patt, p2}
      IO.inspect match(p2, "abcde") == :ok
    end

    if false do
      p3 = parse quote do: +"ab"
      IO.puts("---")
      IO.inspect {:patt, p3}
      IO.inspect match(p3, "ababc") == :ok
    end
    
    if true do
      #p3 = parse quote do: {'_','a'-'z','0'-'9'}
      p3 = parse quote do: "0" * !1
      IO.inspect(p3)
      IO.inspect match(p3, "01")
    end

  end

end

# set ft=elixir
