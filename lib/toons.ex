defmodule Toons do

  require Logger

  defp find_id(props) do
    id = case Enum.find(props, fn {k, _} -> k == "id" end) do
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
    tag2 = case {find_id(props), idx} do
      {nil, 0} -> tag
      {nil, idx} -> tag <> to_string(idx)
      {id, idx} -> id
    end
    data 
    |> parse_props(prefix <> "." <> tag2, props)
    |> parse(prefix <> "." <> tag2, 0, kids)
  end

  defp parse(data, prefix, idx, list) do
    case list do
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
      {:ok, 200, _headers, client} ->
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

  defp grab(toon) do
    Map.put(toon, :data, grab(toon))
  end

  defp grab_all(toons) do
    toons
    |> Enum.map(&Task.async(fn -> grab(&1) end))
    |> Enum.map(&Task.await(&1))
  end

  def go do

    toons = [
      %{
        id: "xkcd",
        url: "http://xkcd.com",
        find: %{
          img: ".html.body1.middleContainer.comic.img.src",
          title: ".html.body1.middleContainer.ctitle",
          alt: ".html.body1.middleContainer.comic.img.title",
        }
      },
      %{
        id: "wumo",
        url: "http://www.gocomics.com/wumo/",
        find: %{
          img: ".html.body1.div9.div4.div1.div.div1.div3.div.a.div.div.picture.img.src",
        }
      },
      %{
        id: "zits",
        url: "https://www.comicskingdom.com/zits/",
        find: %{
          img: ".html.body1.div3.scrollArea.div4.div.right-column.div.div.img1.src",
        }
      },
    ]

    grab_all(toons)
  end

end

# set ft=elixir
