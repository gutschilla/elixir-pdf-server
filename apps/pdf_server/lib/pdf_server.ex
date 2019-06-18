defmodule PdfServer do

  require Logger

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
    options_with_deafults = default_options(options)
    with {:ok, pdf_path}   <- PdfGenerator.generate(content, options_with_deafults),
         {:ok, pdf_binary} <- File.read(pdf_path) do
      {:ok, pdf_binary}
    end
  end

  @doc """
  default_options will look at options add shell_params for each generator
  to remove default margins.
  """
  def default_options(options) do
    generator    = options[:generator]
    shell_params = options[:shell_params] || []
    options
    |> Keyword.put(:shell_params, remove_margins_for(generator) ++ shell_params)
    |> add_extra_params_for(generator)
    |> log_and_return()
  end

  defp log_and_return(value) do
    value |> inspect() |> Logger.debug()
    value
  end

  def remove_margins_for(nil),          do: remove_margins_for(:wkhtmltopdf)
  def remove_margins_for(:wkhtmltopdf), do: ["-L", "0mm", "-R", "0mm", "-T", "0mm", "-B", "0mm"]
  def remove_margins_for(:chrome),      do: ["--no-margins"]

  def add_extra_params_for(options, :wkhtmltopdf), do: options
  def add_extra_params_for(options, :chrome),      do: options |> Keyword.put(:no_sandbox, true)

end
