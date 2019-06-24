# PdfServer

Making wkhtmltopdf and chrome-headless available through an API

# About

This project uses Elixir/Phoenix to expose a pretty simple API that renders PDFs
from either HTML or URLs.

# Endpoints

There's only one endpoint which can be called as GET and POST.

- `GET  /api/generate`
- `POST /api/generate`

Both will result in a HTTP response which contains a PDF in the body.

```http
HTTP/1.1 200 OK
Content-Disposition: attachment; filename=
Content-Type: application/pdf
cache-control: max-age=0, private, must-revalidate
content-length: 7557
date: Tue, 04 Jun 2019 11:17:17 GMT
server: Cowboy
x-request-id: FaT6t7TJQHHXZtcAAABD

%PDF-1.4
…
```

## Example

Send some html (or SVG in this case) as file upload and get back a PDF.

```bash
curl -F "html=@\"ticket.svg\";filename=\"boom.pdf\"" -XPOST http:/127.0.0.1:4000/api/generate\?generator=chrome --output ticket.pdf
```

## Parameters

Both `GET` and `POST` parameters are accepted. For HTML to sent directly to the
endpoint, `POST` is recommended.

- takes either parameter `html` with some HTML string or parameter `url` with
  some URL that returns a `text/html` page to be rendered as PDFs.

- `filename` if you like to have filename set for downloads. Handy for Download
  links.

- `generator`: can be either `wkhtmltopdf` (default) or `chrome` (for
  chrome-headless). wkhtmltopdf's results are usually smaller while crome
  supports more features (for JS-rendering, use chrome)

- `page_size`: "A4" and "A5" are currently supported

- `paper_height`: set a custom paper height in inches (chrome only)

- `paper_width`:  set a custom paper width in inches (chrome only)

Both wkhtmltopdf and chrome-headless support many more options but this is the
whitelisted options that I need right now. Since options are passed as
command-line paramters, I'll stick to whitelisting in fear of shell-injections.

# Installation


## Docker

```bash
# build image
docker build -t pdf-server .
# run image, listen on port 4000
docker run -p 4000:4000/tcp -d --name pdf-server-rel pdf-server
# generate simple "hello" PDF
curl 'http://127.0.0.1:4000/api/generate?html=<html><body><h1>Hello' --output hello.pdf
```

## Plain Ubuntu

See [install.bash](./install.bash) for a `cound-init.sh`-compatible script to
have a śerver configured, up and running from a _scratch_ Ubuntu 18.
