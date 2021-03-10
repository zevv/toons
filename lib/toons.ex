defmodule Toons do

  require Logger

  defp find_id(props) do
    case Enum.find(props, fn {k, _} -> k == "id" end) do
      {_, id} -> id
      nil -> nil
    end
  end
  
  defp parse_props(data, prefix, props) do
    case props do
      [{k, v} | t] ->
        data
        |> Map.put(prefix <> "." <> k, v)
        |> parse_props(prefix, t)
      [] ->
        data
    end
  end

  defp parse(data, prefix, idx, { tag, props, kids} ) do
    tag = case {find_id(props), idx} do
      {nil, 0} -> tag
      {nil, idx} -> tag <> to_string(idx)
      {id, _idx} -> id
    end
    data 
    |> parse_props(prefix <> "." <> tag, props)
    |> parse(prefix <> "." <> tag, 0, kids)
  end

  defp parse(data, prefix, idx, val) do
    case val do
      [] ->
        data
      [head | tail] ->
        data
        |> parse(prefix, idx, head)
        |> parse(prefix, idx+1, tail)
      val ->
        Map.put(data, prefix, val)
    end
  end

  defp dump_debug(toon, data) do
    {:ok, file} = File.open("debug/#{toon.id}", [:write])
    Enum.each(data, fn {k, v} ->
      if is_binary(v) do
        IO.binwrite(file, "#{k} #{v}\n")
      end
    end)
    File.close(file)
  end

  defp grab(toon) do

    Logger.info("Grabbing #{toon.id}")

    case :hackney.get(toon.url, [], "", [follow_redirect: true]) do
      {:ok, 200, _, client} ->
        {:ok, body} = :hackney.body(client)
        {:ok, doc} = Floki.parse_document(body)
        data = parse(%{}, "", 0, doc)

        dump_debug(toon, data)

        res = Enum.map(toon.find, fn {k, v} ->
          if data[v] == nil do
            Logger.error("Missing #{k} in #{toon.id}")
          end
          {k, data[v]} end
        )

        Map.put(toon, :res, res)

      {:error, reason } ->
        Logger.warn("Error getting #{toon.id}: #{reason}")
        Map.put(toon, :error, reason)
    end

  end


  defp grab_all(toons) do
    toons
    |> Enum.map(&Task.async(fn -> grab(&1) end))
    |> Enum.map(&Task.await/1)
  end
  

  def go do
    {:ok, toons} = YamlElixir.read_from_file("toons.yaml", atoms: true)
    grab_all(toons)
  end

end

# set ft=elixir
