defmodule PdfServer.GeneratorController do
  use PdfServer.Web, :controller

  def generate(conn, params) do
    params |> IO.inspect
    pdf_binary = PdfGenerator.generate_binary!(params["html"], delete_temporary: true)
    filename   = params |> Map.get("filename", "generated.pdf")
    conn
    |> put_resp_header("Content-Type", "application/pdf")
    |> put_resp_header("Content-Disposition", "attachment; filename=#{filename}")
    |> resp(200, pdf_binary)
  end

end
