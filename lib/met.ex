
defmodule Met do
  
  @hackney_opts [follow_redirect: true, with_body: true]

  require Logger
  require Regex

  defp get(url, headers \\ []) do

    headers = [
      { <<"User-Agent">>, <<"Mozilla/5.0 (X11; Linux x86_64; rv:87.0) Gecko/20100101 Firefox/87.0">> }
      | headers
    ]

    :hackney.get(url, headers, "", @hackney_opts)
    |> case do
      {:ok, 200, _, body} ->
        body
      {:ok, code, _, } ->
        Logger.warn("Error geting #{url}: #{code}")
      {:error, reason } ->
        Logger.warn("Error getting #{url}: #{reason}")
    end
  end

  defp parse_html(body) do
    Floki.parse_document(body)
    |> case do
      { :ok, doc } ->
        doc
    end
  end

  defp match(subject, re) do
    [_, rv] = Regex.run(re, subject)
    rv
  end

  defp get_main(url) do
    get(url)
    |> parse_html
    |> Floki.find(".cta-item")
    |> Floki.attribute("href")
    |> Enum.filter(fn s -> s =~ "videoId" end)
  end

  defp get_today(url) do
    get(url)
    |> parse_html
    |> Floki.find("script")
    |> Floki.attribute("src")
    |> Enum.filter(fn s -> s =~ "brightcove" end)
  end

  defp get_index_js(url) do
    body = get(url)
    |> IO.puts
    body |> match(~r/policy_key:"([^"]+)/)
  end

  defp get_api_js(policy_key) do
  end

  def hop() do
    url_today = get_main("https://www.metopera.org/")
    url_js = get_today(url_today)
    #policy_key = get_index_js(url_js)
    #get_api_js(policy_key)
    |> IO.puts()
  end

end

# set ft=elixir
