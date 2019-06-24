defmodule PdfServerWeb.GeneratorController do
  use PdfServerWeb, :controller

  def generate(conn, params = %{"url" => url}) do
    with {:ok, options}    <- parse_options(params),
         {:ok, pdf_binary} <- PdfServer.generate({:url, url}, options) do
      deliver_pdf(conn, pdf_binary, params["filename"])
    else
      {:error, reason} -> respond_with_error(conn, reason)
    end
  end

  def generate(conn, params = %{"html" => upload = %Plug.Upload{}}) do
    with {:ok, options}    <- parse_options(params),
         {:ok, html} <- File.read(upload.path),
         {:ok, pdf_binary} <- PdfServer.generate({:html, html}, options) do
      deliver_pdf(conn, pdf_binary, params["filename"] || upload.filename)
    end
  end

  def generate(conn, params = %{"html" => html}) do
    {:ok, pdf_binary} = PdfServer.generate({:html, html})
    deliver_pdf(conn, pdf_binary, params["filename"])
  end

  def generate(conn, _params) do
    resp(conn, 400, "missing url or html")
  end

  def deliver_pdf(conn, pdf_binary, filename \\ "generated.pdf") do
    conn
    |> put_resp_header("Content-Type", "application/pdf")
    |> put_resp_header("Content-Disposition", "attachment; filename=#{filename}")
    |> resp(200, pdf_binary)
  end

  def respond_with_error(conn, reason) do
    text_reason
    = case reason do
        {:result_ok, _} -> "pdf generator did not return a proper PDF"
        any             -> inspect(any)
      end
    conn
    |> put_resp_header("Content-Type", "application/elixir-term")
    |> put_resp_header("X-PdfGenerator-Error", "#{text_reason}")
    |> resp(500, "see X-PdfGenerator-Error header")
  end

  @doc ~s"""
  Transforms paramter map into keyword list of allowed atom keys for
  PdfGenerator. Ignores unknown key or thos not in range

  ## Example

        iex> PdfServerWeb.GeneratorController.parse_options(%{"generator" => "chrome", "page_size" => "A99"})
        [generator: :chrome]
  """
  @spec parse_options(map()) :: keyword()
  def parse_options(map = %{}) do
    options = Enum.reduce(map, [], &handle_key/2)
    {:ok, options}
  end

  defp handle_key({"generator", "wkhtmltopdf"}, list), do: [{:generator, :wkhtmltopdf} | list]
  defp handle_key({"generator", "chrome"},      list), do: [{:prefer_system_executable, true} | [{:generator, :chrome} | list]]
  defp handle_key({"paper_height", x},          list), do: [{:paper_height, x} | list]
  defp handle_key({"paper_width", x},           list), do: [{:paper_width,  x} | list]
  defp handle_key({"page_size", x}, list) when x in ~w[A4 A5], do: [{:paper_size, x} | list]
  defp handle_key({_key, _value}, list), do: list # ignore key/val

end
