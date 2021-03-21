defmodule Xpeg do
  
  require Logger

  defp mk_concat(n1, n2) do
    List.flatten [n1, n2]
  end
  
  defp mk_choice(p1, p2) do
    List.flatten [
      { :choice, length(p1) + 2 },
      p1,
      { :commit, length(p2) + 1 },
      p2,
    ]
  end

  defp parse(grammar, { id, meta, args } ) do
    case { id, args } do
      { :<-, [{name, _, _}, patt] } ->
        IO.inspect({"rule", name, patt})
      { :*, [k1, k2]} ->
        mk_concat(parse(grammar, k1), parse(grammar, k2))
      { :|, [k1, k2]} ->
        mk_choice(parse(grammar, k1), parse(grammar, k2))
      { _, _ } ->
        IO.inspect({"unhandled", id, meta, args})
    end
  end
  
  defp parse(_grammar,  p) do
    case p do
      v when is_number(v) ->
        { :chr, v }
      v when is_binary(v) ->
        to_charlist(v) |> Enum.map(fn c -> { :chr, c } end)
      [v] ->
        { :chr, v }
      v ->
        IO.inspect {"Unhandled lit", v}
    end
  end

  defp error(state) do
    case state.back_stack do
      [frame | back_stack] ->
        IO.inspect {"oeps", frame } 
        state = %{state | back_stack: back_stack }
        match(frame.patt, frame.s, state)
      [] ->
        throw("Error parsing")
    end
  end

  defp match([], _, _) do
    IO.inspect {"done"}
  end
  
  defp match(patt, s) do
    state = %{
      back_stack: []
    }
    match(patt, to_charlist(s), state)
  end

  defp match(patt, s, state) do

    [inst | ptail] = patt
    IO.inspect {"match", state, inst, s}

    case inst do

      { :chr, c } -> 
        if c == hd(s) do
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
        [_ | back_stack] = state.back_stack
        state = %{ state |
          :back_stack => back_stack
        }
        match(Enum.drop(ptail, offset), s, state)
    end
  end


  def go do
    IO.puts("hello")

    p = quote do: '0' * ('a' * 'b' | "cd") * 'e'

    patt = parse(%{}, p)
    IO.inspect {:patt, patt}

    match(patt, "0abe")
    match(patt, "0cde")

  end


end

# set ft=elixir
