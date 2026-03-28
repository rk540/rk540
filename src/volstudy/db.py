from __future__ import annotations

import json
import sqlite3
from contextlib import contextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterator


SCHEMA_SQL = """
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS models (
    model_id INTEGER PRIMARY KEY,
    model_name TEXT NOT NULL UNIQUE,
    model_type TEXT NOT NULL,
    implementation_kind TEXT NOT NULL,
    entrypoint TEXT NOT NULL,
    description TEXT,
    is_active INTEGER NOT NULL DEFAULT 1
);

CREATE TABLE IF NOT EXISTS model_configs (
    model_config_id INTEGER PRIMARY KEY,
    model_id INTEGER NOT NULL,
    config_name TEXT NOT NULL,
    config_json TEXT NOT NULL,
    created_at TEXT NOT NULL,
    FOREIGN KEY(model_id) REFERENCES models(model_id)
);

CREATE TABLE IF NOT EXISTS fit_runs (
    run_id TEXT PRIMARY KEY,
    ticker TEXT NOT NULL,
    trade_date TEXT NOT NULL,
    model_id INTEGER NOT NULL,
    model_config_id INTEGER,
    status TEXT NOT NULL,
    started_at TEXT NOT NULL,
    finished_at TEXT,
    output_dir TEXT NOT NULL,
    notes TEXT,
    FOREIGN KEY(model_id) REFERENCES models(model_id),
    FOREIGN KEY(model_config_id) REFERENCES model_configs(model_config_id)
);

CREATE TABLE IF NOT EXISTS fit_run_expiries (
    run_id TEXT NOT NULL,
    expiry_date TEXT NOT NULL,
    tau_days INTEGER NOT NULL,
    n_points INTEGER,
    fit_ok INTEGER NOT NULL,
    fit_message TEXT,
    PRIMARY KEY(run_id, expiry_date),
    FOREIGN KEY(run_id) REFERENCES fit_runs(run_id)
);

CREATE TABLE IF NOT EXISTS fit_run_diagnostics (
    run_id TEXT NOT NULL,
    expiry_date TEXT NOT NULL,
    atm_iv REAL,
    atm_slope REAL,
    atm_curvature REAL,
    left_wing_slope REAL,
    right_wing_slope REAL,
    rmse_total_var REAL,
    mae_total_var REAL,
    n_points INTEGER,
    PRIMARY KEY(run_id, expiry_date),
    FOREIGN KEY(run_id) REFERENCES fit_runs(run_id)
);

CREATE TABLE IF NOT EXISTS fit_run_parameters (
    run_id TEXT NOT NULL,
    expiry_date TEXT NOT NULL,
    param_name TEXT NOT NULL,
    param_value REAL,
    PRIMARY KEY(run_id, expiry_date, param_name),
    FOREIGN KEY(run_id) REFERENCES fit_runs(run_id)
);

CREATE TABLE IF NOT EXISTS market_data_snapshots (
    snapshot_id TEXT PRIMARY KEY,
    ticker TEXT NOT NULL,
    trade_date TEXT NOT NULL,
    source_name TEXT NOT NULL,
    raw_chain_path TEXT,
    smile_path TEXT,
    vendor_surface_path TEXT,
    created_at TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_fit_runs_ticker_date
    ON fit_runs(ticker, trade_date);

CREATE INDEX IF NOT EXISTS idx_fit_run_diag_run
    ON fit_run_diagnostics(run_id);

CREATE INDEX IF NOT EXISTS idx_fit_run_params_run
    ON fit_run_parameters(run_id);
"""


@dataclass
class VolStudyDB:
    db_path: Path

    def __post_init__(self) -> None:
        self.db_path = Path(self.db_path).expanduser().resolve()

    @contextmanager
    def connect(self) -> Iterator[sqlite3.Connection]:
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row
        try:
            conn.execute("PRAGMA foreign_keys = ON;")
            yield conn
            conn.commit()
        except Exception:
            conn.rollback()
            raise
        finally:
            conn.close()

    def initialize(self) -> None:
        with self.connect() as conn:
            conn.executescript(SCHEMA_SQL)

    def upsert_model(
        self,
        model_name: str,
        model_type: str,
        implementation_kind: str,
        entrypoint: str,
        description: str | None = None,
        is_active: bool = True,
    ) -> None:
        sql = """
        INSERT INTO models (
            model_name, model_type, implementation_kind, entrypoint, description, is_active
        ) VALUES (?, ?, ?, ?, ?, ?)
        ON CONFLICT(model_name) DO UPDATE SET
            model_type = excluded.model_type,
            implementation_kind = excluded.implementation_kind,
            entrypoint = excluded.entrypoint,
            description = excluded.description,
            is_active = excluded.is_active
        """
        with self.connect() as conn:
            conn.execute(
                sql,
                (
                    model_name,
                    model_type,
                    implementation_kind,
                    entrypoint,
                    description,
                    int(is_active),
                ),
            )

    def get_model_id(self, model_name: str) -> int:
        with self.connect() as conn:
            row = conn.execute(
                "SELECT model_id FROM models WHERE model_name = ?",
                (model_name,),
            ).fetchone()
        if row is None:
            raise ValueError(f"Unknown model_name: {model_name}")
        return int(row["model_id"])

    def insert_model_config(
        self,
        model_id: int,
        config_name: str,
        config: dict[str, Any],
        created_at: str,
    ) -> int:
        sql = """
        INSERT INTO model_configs (model_id, config_name, config_json, created_at)
        VALUES (?, ?, ?, ?)
        """
        with self.connect() as conn:
            cur = conn.execute(
                sql,
                (model_id, config_name, json.dumps(config, sort_keys=True), created_at),
            )
            return int(cur.lastrowid)

    def insert_fit_run(
        self,
        run_id: str,
        ticker: str,
        trade_date: str,
        model_id: int,
        model_config_id: int | None,
        status: str,
        started_at: str,
        output_dir: str,
        notes: str | None = None,
    ) -> None:
        sql = """
        INSERT INTO fit_runs (
            run_id, ticker, trade_date, model_id, model_config_id,
            status, started_at, output_dir, notes
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """
        with self.connect() as conn:
            conn.execute(
                sql,
                (
                    run_id,
                    ticker,
                    trade_date,
                    model_id,
                    model_config_id,
                    status,
                    started_at,
                    output_dir,
                    notes,
                ),
            )

    def update_fit_run_status(
        self,
        run_id: str,
        status: str,
        finished_at: str | None = None,
    ) -> None:
        sql = """
        UPDATE fit_runs
        SET status = ?, finished_at = COALESCE(?, finished_at)
        WHERE run_id = ?
        """
        with self.connect() as conn:
            conn.execute(sql, (status, finished_at, run_id))

    def insert_fit_run_expiry(
        self,
        run_id: str,
        expiry_date: str,
        tau_days: int,
        n_points: int | None,
        fit_ok: bool,
        fit_message: str | None,
    ) -> None:
        sql = """
        INSERT OR REPLACE INTO fit_run_expiries (
            run_id, expiry_date, tau_days, n_points, fit_ok, fit_message
        ) VALUES (?, ?, ?, ?, ?, ?)
        """
        with self.connect() as conn:
            conn.execute(
                sql,
                (run_id, expiry_date, tau_days, n_points, int(fit_ok), fit_message),
            )

    def insert_fit_run_diagnostic(
        self,
        run_id: str,
        expiry_date: str,
        diag: dict[str, Any],
    ) -> None:
        sql = """
        INSERT OR REPLACE INTO fit_run_diagnostics (
            run_id, expiry_date, atm_iv, atm_slope, atm_curvature,
            left_wing_slope, right_wing_slope, rmse_total_var,
            mae_total_var, n_points
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """
        with self.connect() as conn:
            conn.execute(
                sql,
                (
                    run_id,
                    expiry_date,
                    diag.get("atm_iv"),
                    diag.get("atm_slope"),
                    diag.get("atm_curvature"),
                    diag.get("left_wing_slope"),
                    diag.get("right_wing_slope"),
                    diag.get("rmse_total_var"),
                    diag.get("mae_total_var"),
                    diag.get("n_points"),
                ),
            )

    def insert_fit_run_parameters(
        self,
        run_id: str,
        expiry_date: str,
        params: dict[str, float],
    ) -> None:
        sql = """
        INSERT OR REPLACE INTO fit_run_parameters (
            run_id, expiry_date, param_name, param_value
        ) VALUES (?, ?, ?, ?)
        """
        with self.connect() as conn:
            conn.executemany(
                sql,
                [(run_id, expiry_date, k, float(v)) for k, v in params.items()],
            )
