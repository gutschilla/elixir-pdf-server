# Since configuration is shared in umbrella projects, this file
# should only configure the :pdf_server_web application itself
# and only for organization purposes. All other config goes to
# the umbrella root.
use Mix.Config

# General application configuration
config :pdf_server_web,
  generators: [context_app: :pdf_server]

# Configures the endpoint
config :pdf_server_web, PdfServerWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "NtjZnk9W7w36YoM39+BGPkgqH1cFv2vcjYGWALVpmonKH4Fg9+Ml76BnV1p0iJKO",
  render_errors: [view: PdfServerWeb.ErrorView, accepts: ~w(json)],
  pubsub: [name: PdfServerWeb.PubSub, adapter: Phoenix.PubSub.PG2]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
