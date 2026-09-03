#!/usr/bin/env python3
"""HTTP service wrapping pdflatex/magick for etests.

Endpoints (all POST, JSON body):
  /latex-to-pdf  { "document": str, "files": { name: encoded } }
  /latex-to-png  { "latex": str, "preamble": str }
  /health        GET  -> 200 ok

Request/response bytes use CirceCodecs encoding:
  binary  -> "\U0001d539" (𝔹) + base64
  UTF-8 text -> plain JSON string
"""
import base64
import http.server
import json
import os
import shutil
import socketserver
import subprocess
import sys
import tempfile

BASE64_PREFIX = "\U0001d539"  # 𝔹


def decode_field(s):
    if s.startswith(BASE64_PREFIX):
        return base64.b64decode(s[len(BASE64_PREFIX):])
    return s.encode("utf-8")


def encode_field(b):
    if b is None:
        return None
    try:
        s = b.decode("utf-8")
        if not s.startswith(BASE64_PREFIX):
            return s
    except (UnicodeDecodeError, AttributeError):
        pass
    return BASE64_PREFIX + base64.b64encode(b).decode("ascii")


def latex_response(result, latex_log, convert_log):
    return {
        "result": encode_field(result),
        "latexLog": encode_field(latex_log),
        "convertLog": encode_field(convert_log),
    }


def read_optional(path):
    return open(path, "rb").read() if os.path.exists(path) else None


def handle_latex_to_pdf(body):
    workdir = tempfile.mkdtemp()
    try:
        with open(os.path.join(workdir, "latex.tex"), "w", encoding="utf-8") as f:
            f.write(body["document"])
        for name, content in body.get("files", {}).items():
            with open(os.path.join(workdir, name), "wb") as f:
                f.write(decode_field(content))

        subprocess.run(
            ["pdflatex", "-halt-on-error", "-interaction=batchmode", "latex.tex"],
            cwd=workdir, capture_output=True,
        )

        latex_log = read_optional(os.path.join(workdir, "latex.log"))
        pdf_path = os.path.join(workdir, "latex.pdf")
        if not os.path.exists(pdf_path):
            return latex_response(None, latex_log, None)

        return latex_response(open(pdf_path, "rb").read(), latex_log, None)
    finally:
        shutil.rmtree(workdir, ignore_errors=True)


def handle_latex_to_png(body):
    workdir = tempfile.mkdtemp()
    try:
        latex = body["latex"]
        preamble = body["preamble"]
        document = (
            f"\\documentclass[tikz,border=2mm]{{standalone}}\n"
            f"{preamble}\n"
            f"\\begin{{document}}\n"
            f"{latex}\n"
            f"\\end{{document}}"
        )
        with open(os.path.join(workdir, "latex.tex"), "w", encoding="utf-8") as f:
            f.write(document)

        subprocess.run(
            ["pdflatex", "-interaction=batchmode", "latex.tex"],
            cwd=workdir, capture_output=True,
        )

        latex_log = read_optional(os.path.join(workdir, "latex.log"))
        pdf_path = os.path.join(workdir, "latex.pdf")
        if not os.path.exists(pdf_path):
            return latex_response(None, latex_log, None)

        rc = subprocess.run(
            [
                "magick", "-density", "300", "latex.pdf",
                "+set", "date:create", "+set", "date:modify",
                "-define", "png:exclude-chunks=date,tIME", "-strip", "result.png",
            ],
            cwd=workdir, capture_output=True,
        )
        convert_log = (rc.stdout + rc.stderr) or None

        png_path = os.path.join(workdir, "result.png")
        if not os.path.exists(png_path):
            return latex_response(None, latex_log, convert_log)

        return latex_response(open(png_path, "rb").read(), None, None)
    finally:
        shutil.rmtree(workdir, ignore_errors=True)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/health":
            self._reply(200, b"ok", "text/plain")
        else:
            self._reply(404, b"not found", "text/plain")

    def do_POST(self):
        length = int(self.headers.get("Content-Length", 0))
        body = json.loads(self.rfile.read(length))
        if self.path == "/latex-to-pdf":
            result = handle_latex_to_pdf(body)
        elif self.path == "/latex-to-png":
            result = handle_latex_to_png(body)
        else:
            self._reply(404, b"not found", "text/plain")
            return
        self._reply(200, json.dumps(result).encode("utf-8"), "application/json")

    def _reply(self, code, body, content_type):
        self.send_response(code)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, fmt, *args):
        print(fmt % args, file=sys.stderr, flush=True)


class ThreadedHTTPServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
    daemon_threads = True


if __name__ == "__main__":
    server = ThreadedHTTPServer(("", 8081), Handler)
    print("latex service listening on :8081", flush=True)
    server.serve_forever()
