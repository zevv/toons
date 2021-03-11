defmodule Toons do

  require Logger

  defp find_id(props) do
    case Enum.find(props, fn {k, _} -> k == "id" end) do
      {_, id} -> id
      nil -> nil
    end
  end
  
  defp parse_props(data, prefix, props) do
    props = Map.new(props, fn {k, v} -> { prefix <> "." <> k, v } end)
    data |> Map.merge(props)
  end

  defp get_tag(tag, idx, props) do
    case {find_id(props), idx} do
      {nil, 0} -> tag
      {nil, idx} -> tag <> to_string(idx)
      {id, _idx} -> id
    end
  end

  defp parse(data, prefix, idx, val) do
    case val do
      [head | tail] ->
        data
        |> parse(prefix, idx, head)
        |> parse(prefix, idx+1, tail)
      [] ->
        data
      {tag, props, kids} ->
        tag = get_tag(tag, idx, props)
        data 
        |> parse_props(prefix <> "." <> tag, props)
        |> parse(prefix <> "." <> tag, 0, kids)
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

  defp grab_image(toon) do
    url = String.replace_leading(toon.res.img, "//", "http://")

    case :hackney.get(url, [], "", [follow_redirect: true]) do
      {:ok, 200, _headers, client} ->
        {:ok, img} = :hackney.body(client)
        hash = :crypto.hash(:md5, img) |> Base.encode16()
        Logger.info("Got #{toon.id} img #{hash}")
        Map.put(toon, :img_hash, hash)
      {:error, reason} ->
        Logger.warn("Error geting #{toon.res.img}: #{reason}")
        ""
    end
  end

  defp grab_html(toon) do

    case :hackney.get(toon.url, [], "", [follow_redirect: true]) do
      {:ok, 200, _headers, client} ->
        {:ok, body} = :hackney.body(client)
        {:ok, doc} = Floki.parse_document(body)
        data = parse(%{}, "", 0, doc)
        Logger.info("Got #{toon.id} html")

        dump_debug(toon, data)

        keys = [:img, :alt, :title] |> Enum.filter(&(toon |> Map.has_key?(&1)))

        toon = Map.put toon, :res, Enum.map(keys, fn k ->
          if data[toon[k]] == nil do
            Logger.error("Missing #{k} in #{toon.id}")
          end
          {k, data[toon[k]]} end
        ) |> Map.new()

        grab_image(toon)

      {:error, reason } ->
        Logger.warn("Error getting #{toon.id}: #{reason}")
        Map.put(toon, :error, reason)
    end

  end


  defp grab_all(toons) do
    toons
    |> Enum.map(&Task.async(fn -> grab_html(&1) end))
    |> Enum.map(&Task.await/1)
  end



  def go do
    case YamlElixir.read_from_file("toons.yaml", atoms: true) do
      {:ok, toons} -> 
        toons = Enum.map(toons, fn toon -> 
          toon |> Map.new(fn {k,v} -> {String.to_atom(k), v} end)
        end)
        grab_all(toons)
      {:error, e } -> IO.inspect(e)
    end
  end

end

# set ft=elixir
