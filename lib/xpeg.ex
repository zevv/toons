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
  
  defp parse(grammar,  p) do
    case p do
      v when is_number(v) ->
        { :chr, v }
      v when is_binary(v) ->
        to_charlist(v) |> Enum.map(fn c -> { :chr, c } end)
      [v] ->
        { :chr, v }
      v ->
        IO.inspect {"Unhandled lit"}
    end
  end

  defp error(state) do
    [back_frame | back_stack] = state.back_stack
    IO.inspect {"oeps", back_frame } 
    state = %{state | back_stack: back_stack }
    match(back_frame.patt, back_frame.s, state)
  end
  
  defp match([], _, state) do
    IO.inspect {"done"}
  end

  defp match(patt, [s | stail], state) do

    [inst | ptail] = patt
    IO.inspect {"match", state, inst, s}

    state = case inst do

      { :chr, c } -> 
        if c == s do
          match(ptail, stail, state)
        else
          error(state)
        end

      { :choice, offset } ->
        back_frame = %{
          patt: Enum.drop(ptail, offset-1),
          s: [s | stail],
        }
        state = %{ state |
          :back_stack => [ back_frame | state.back_stack ]
        }
        match(ptail, [ s | stail], state)

      { :commit, offset } ->
        [_ | back_stack] = state.back_stack
        state = %{ state |
          :back_stack => back_stack
        }
        match(Enum.drop(ptail, offset), [s | stail], state)
    end
  end


  def go do
    IO.puts("hello")

    p = quote do
      '0' * ('a' * 'b' | "cd") * 'e'
    end

    grammar = %{ }
    patt = parse(grammar, p)
    IO.inspect {:patt, patt}

    state = %{
      back_stack: []
    }
    match(patt, to_charlist("0cde"), state)

  end


end

# set ft=elixir
