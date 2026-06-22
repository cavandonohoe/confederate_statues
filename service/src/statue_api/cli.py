"""CLI entrypoint: ingest, serve, migrate helpers."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from statue_api.ingest import run_ingest
from statue_api.logging_config import configure_logging, get_logger


def _ingest(args: argparse.Namespace) -> int:
    n = run_ingest(Path(args.csv))
    print(f"Ingested {n} rows.")
    return 0


def _serve(args: argparse.Namespace) -> int:
    import uvicorn

    uvicorn.run(
        "statue_api.main:app",
        host=args.host,
        port=args.port,
        reload=args.reload,
        access_log=False,
    )
    return 0


def main(argv: list[str] | None = None) -> int:
    configure_logging()
    get_logger("statue_api.cli").debug("cli_start", argv=argv or sys.argv[1:])

    parser = argparse.ArgumentParser(prog="statue-api")
    sub = parser.add_subparsers(dest="cmd", required=True)

    p_ingest = sub.add_parser("ingest", help="Load the CSV into Postgres.")
    p_ingest.add_argument(
        "--csv",
        default="../data/confederate_statue_dates.csv",
        help="Path to the upstream CSV.",
    )
    p_ingest.set_defaults(func=_ingest)

    p_serve = sub.add_parser("serve", help="Run the API server.")
    p_serve.add_argument("--host", default="0.0.0.0")  # noqa: S104
    p_serve.add_argument("--port", type=int, default=8000)
    p_serve.add_argument("--reload", action="store_true")
    p_serve.set_defaults(func=_serve)

    args = parser.parse_args(argv)
    return int(args.func(args))


if __name__ == "__main__":
    raise SystemExit(main())
