defmodule PdfServerWeb.Router do
  use PdfServerWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", PdfServerWeb do
    pipe_through :api
    post "/generate", GeneratorController, :generate # you may post html as param
    get  "/generate", GeneratorController, :generate # or get ?url=https://foobar.com
  end
end
