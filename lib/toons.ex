defmodule Toons do

  @hackney_opts [follow_redirect: true, with_body: true]

  require Logger

  defp find_id(props) do
    Map.new(props)["id"]
  end
  
  defp get_props(data, prefix, props) do
    Enum.into(props, data, fn {k, v} -> { prefix <> "." <> k, v } end)
  end

  defp get_tag(tag, idx, props) do
    case {find_id(props), idx} do
      {nil, 0} -> tag
      {nil, idx} -> tag <> to_string(idx)
      {id, _} -> id
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
        |> get_props(prefix <> "." <> tag, props)
        |> parse(prefix <> "." <> tag, 0, kids)
      {:comment, _} ->
        data
      val ->
        Map.put(data, prefix, val)
    end
  end

  defp dump_debug(toon, data) do
    File.write("debug/#{toon.id}",
      Enum.map(data, fn {a, b} -> a <> " = " <> b end) |> Enum.join("\n"))
  end

  defp grab_image(toon) do
    url = String.replace_leading(toon.res.img, "//", "http://")

    case :hackney.get(url, [], "", @hackney_opts) do
      {:ok, 200, _, img} ->
        hash = :crypto.hash(:md5, img) |> Base.encode16()
        Logger.info("Got #{toon.id} img #{hash}")
        Map.put(toon, :img_hash, hash)
      {:ok, code, _, } ->
        Logger.warn("Error geting #{toon.res.img}: #{code}")
      {:error, reason} ->
        Logger.warn("Error geting #{toon.res.img}: #{reason}")
        ""
    end
  end

  defp parse_html(toon, body) do
    {:ok, doc} = Floki.parse_document(body)
    data = parse(%{}, "", 0, doc)
    Logger.info("Got #{toon.id} html")

    dump_debug(toon, data)

    keys = [:img, :alt, :title] |> Enum.filter(&(toon |> Map.has_key?(&1)))

    res = Enum.map(keys, fn k ->
      if ! data[toon[k]] do
        Logger.error("Missing #{k} in #{toon.id}")
      end
      {k, data[toon[k]]} end
    )

    toon
    |> Map.put(:res, Map.new(res))
    |> grab_image
  end

  defp grab_html(toon) do
    case :hackney.get(toon.url, [], "", @hackney_opts) do
      {:ok, 200, _, body} ->
        parse_html(toon, body)
      {:ok, code, _, } ->
        Logger.warn("Error geting #{toon.id}: #{code}")
      {:error, reason } ->
        Logger.warn("Error getting #{toon.id}: #{reason}")
        Map.put(toon, :error, reason)
    end
  end

  defp grab_all(toons) do
    toons
    |> Enum.map(&Task.async(fn -> grab_html(&1) end))
    |> Enum.map(fn pid -> Task.await(pid, 10000) end)
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
