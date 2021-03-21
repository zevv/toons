defmodule Toons do

  @compile if Mix.env == :test, do: :export_all

  @hackney_opts [follow_redirect: true, with_body: true]
  @keys [:img, :alt, :title]

  require Logger

  defp get_props(data, prefix, props) do
    Enum.into(props, data, fn {k, v} -> { prefix <> "." <> k, v } end)
  end

  defp get_tag(tag, idx, props) do
    { Map.new(props)["id"], idx }
    |> case do
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
        tag = prefix <> "." <> get_tag(tag, idx, props)
        data 
        |> get_props(tag, props)
        |> parse(tag, 0, kids)
      {:comment, _} ->
        data
      val ->
        Map.put(data, prefix, val)
    end
  end

  defp dump_debug(toon, data) do
    File.write("debug/#{toon.id}",
      Enum.map(data, fn {a, b} -> a <> " = " <> b end) |> Enum.join("\n"))
    toon
  end

  defp grab_image(toon) do
    toon.img
    |> String.replace_leading("//", "http://")
    |> :hackney.get([], "", @hackney_opts)
    |> case do
      {:ok, 200, _, img} ->
        hash = :crypto.hash(:md5, img) |> Base.encode16()
        Logger.info("Got #{toon.id} img #{hash}")
        Map.put(toon, :img_hash, hash)
      {:ok, code, _, } ->
        Logger.warn("Error geting #{toon.img}: #{code}")
      {:error, reason} ->
        Logger.warn("Error geting #{toon.img}: #{reason}")
        ""
    end
  end

  defp parse_html(toon, body) do
    {:ok, doc} = Floki.parse_document(body)
    data = parse(%{}, "", 0, doc)
    Logger.info("Got #{toon.id} html")

    toon
    |> dump_debug(data)
    |> Enum.into(%{}, fn {k, v} ->
      case { Enum.member?(@keys, k), data[v] } do
        {:false, _} -> {k, v}
        {:true, nil} ->
          Logger.error("Could not find #{k} in #{toon.id}")
          {k, nil}
        {:true, d} -> {k, d}
      end
    end)
    |> grab_image
  end

  defp grab_html(toon) do
    toon.url
    |> :hackney.get([], "", @hackney_opts)
    |> case do
      {:ok, 200, _, body} ->
        parse_html(toon, body)
      {:ok, code, _, } ->
        Logger.warn("Error geting #{toon.id}: #{code}")
      {:error, reason } ->
        Logger.warn("Error getting #{toon.id}: #{reason}")
    end
  end

  defp grab_all(toons) do
    toons
    |> Flow.from_enumerable(stages: 8, max_demand: 1)
    |> Flow.map(&grab_html/1)
    |> Flow.run()
  end

  def go do
    YamlElixir.read_from_file("toons.yaml")
    |> case do
      {:ok, toons} -> 
        toons
        |> Enum.map(&(&1 |> Map.new(fn {k,v} -> {String.to_atom(k), v} end)))
        |> Enum.map(&(&1 |> Map.put_new(:disabled, false)))
        |> Enum.filter(fn v -> not v.disabled end)
        |> grab_all()
      {:error, e } -> Logger.warn("Error parsing toons.yaml:#{e.line}:#{e.column}: #{e.message}")
    end
  end


  def test1 do
    a = quote do
      a <- "flop" | "flip" * "flap"

    end

    IO.inspect(a)


  end


end

# set ft=elixir
