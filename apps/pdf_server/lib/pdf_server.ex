defmodule PdfServer do
  @moduledoc """
  PdfServer keeps the contexts that define your domain
  and business logic.

  Contexts are also responsible for managing your data, regardless
  if it comes from the database, an external API or others.
  """
  def generate(content),                do: generate(content, [])
  def generate({:html, html}, options), do: generate_content(html,        options)
  def generate({:url,   url}, options), do: generate_content({:url, url}, options)
  def generate_content(content, options) do
    with {:ok, pdf_path}   <- PdfGenerator.generate(content, options),
         {:ok, pdf_binary} <- File.read(pdf_path) do
      {:ok, pdf_binary}
    end
  end

end
