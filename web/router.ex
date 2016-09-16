defmodule PdfServer.Router do
  use PdfServer.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api/v1", PdfServer do
    pipe_through :api
    post "/generate", GeneratorController, :generate 
  end
end
