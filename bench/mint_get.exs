Mix.install([
  {:castore, "~> 1.0"},
  {:mint, "~> 1.0"}
])

require Logger

defmodule DoThing do
  def request(url) do
    uri = URI.parse(url)
    scheme = uri.scheme |> String.to_atom()
    {:ok, conn} = Mint.HTTP.connect(scheme, uri.host, uri.port)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/", [], "")
    stream(conn, [])
  end

  def stream(_conn, {:done, acc}), do: acc

  def stream(conn, acc) do
    receive do
      message ->
        {:ok, conn, responses} = Mint.HTTP.stream(conn, message)
        stream(conn, h(responses, acc)) |> IO.iodata_to_binary()
    end
  end

  def h([], acc), do: acc
  def h([{:status, _, code} | xs], []), do: h(xs, [Integer.to_string(code)])
  def h([{:headers, _, headers} | xs], acc), do: h(xs, [acc, ?\n, hs(headers)])
  def h([{:data, _, body} | xs], acc), do: h(xs, [acc, body])
  def h([{:done, _} | _xs], acc), do: {:done, acc}

  def hs(headers) do
    Enum.map(headers, fn {h, v} -> [h, ": ", v, ?\n] end)
  end
end

[url] = System.argv()
{time, data} = :timer.tc(fn -> DoThing.request(url) end)
Logger.info("request took #{time / 1_000_000}s")
:io.format("~s", [data])
