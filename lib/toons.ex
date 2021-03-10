defmodule Toons do

  require Logger

  def find_prop_id([{"id", v}|_t]) do
    v
  end

  def find_prop_id([_h|t]) do
    find_prop_id(t)
  end

  def find_prop_id([]) do
  end


  def parse_props(data, prefix, [{k, v} | t]) do
    data = Map.put(data, prefix <> "." <> k, v)
    parse_props(data, prefix, t)
  end

  def parse_props(data, _prefix, []) do
    data
  end


  def parse(data, prefix, idx, { tag, props, kids} ) do

    id = find_prop_id(props)

    tag = if id do
      id
    else
      tag
    end

    tag = if idx > 0 do
      tag <> to_string(idx)
    else
      tag
    end

    data = parse_props(data, prefix <> "." <> tag, props)
    parse(data, prefix <> "." <> tag, 0, kids)
  end

  def parse(data, prefix, idx, [h|t]) do
    data = parse(data, prefix, idx, h)
    parse(data, prefix, idx+1, t)
  end

  def parse(data, _prefix, _idx, []) do
    data
  end

  def parse(data, prefix, _idx, val) do
    #IO.inspect({"catch", prefix, idx, val})
    Map.put(data, prefix, val)
  end

  def dump_debug(toon, data) do
    {:ok, file} = File.open("debug/#{toon.id}", [:write])
    Enum.each(data, fn {k, v} ->
      if is_binary(v) do
        IO.binwrite(file, "#{k} #{v}\n")
      end
    end)
    File.close(file)
  end


  def grab(toon) do

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

  def grab(toon) do
    Map.put(toon, :data, grab(toon))
  end

  def grab_all(toons) do
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
          img: ".html.body1.middleContainer1.comic2.img.src",
          title: ".html.body1.middleContainer1.ctitle",
          alt: ".html.body1.middleContainer1.comic2.img.title",
        }
      },
      %{
        id: "qc",
        url: "http://questionablecontent.net/",
        find: %{
          img: ".html.body1.middleContainer1.comic2.img.src",
          title: ".html.body1.middleContainer1.ctitle",
          alt: ".html.body1.middleContainer1.comic2.img.title",
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
          img: ".html.body1.div3.scrollArea3.div4.div.right-column1.div.div.img1.src",
        }
      },
    ]

    grab_all(toons)
  end

end

# set ft=elixir
