from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from datetime import datetime, timezone


@dataclass(frozen=True)
class ProjectPaths:
    root: Path

    @classmethod
    def from_root(cls, root: str | Path) -> "ProjectPaths":
        return cls(Path(root).expanduser().resolve())

    @property
    def config_dir(self) -> Path:
        return self.root / "config"

    @property
    def data_dir(self) -> Path:
        return self.root / "data"

    @property
    def db_dir(self) -> Path:
        return self.root / "db"

    @property
    def db_path(self) -> Path:
        return self.db_dir / "volstudy.sqlite"

    @property
    def wrds_dir(self) -> Path:
        return self.root / "wrds"

    @property
    def r_dir(self) -> Path:
        return self.root / "r"

    @property
    def gui_dir(self) -> Path:
        return self.root / "gui"

    @property
    def logs_dir(self) -> Path:
        return self.root / "logs"

    @property
    def studies_dir(self) -> Path:
        return self.data_dir / "studies"

    def ticker_dir(self, ticker: str) -> Path:
        return self.studies_dir / ticker.upper()

    def trade_date_dir(self, ticker: str, trade_date: str) -> Path:
        return self.ticker_dir(ticker) / trade_date

    def fits_dir(self, ticker: str, trade_date: str) -> Path:
        return self.trade_date_dir(ticker, trade_date) / "fits"

    def reports_dir(self, ticker: str, trade_date: str) -> Path:
        return self.trade_date_dir(ticker, trade_date) / "reports"

    def market_dir(self, ticker: str, trade_date: str) -> Path:
        return self.trade_date_dir(ticker, trade_date) / "market"

    def model_runs_dir(self, ticker: str, trade_date: str, model_name: str) -> Path:
        return self.fits_dir(ticker, trade_date) / model_name

    def run_dir(self, ticker: str, trade_date: str, model_name: str, run_id: str) -> Path:
        return self.model_runs_dir(ticker, trade_date, model_name) / run_id

    @staticmethod
    def new_run_id() -> str:
        return datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")

    def ensure_base_dirs(self) -> None:
        for path in [
            self.config_dir,
            self.data_dir,
            self.db_dir,
            self.wrds_dir,
            self.r_dir,
            self.gui_dir,
            self.logs_dir,
            self.studies_dir,
        ]:
            path.mkdir(parents=True, exist_ok=True)

    def ensure_run_dirs(
        self,
        ticker: str,
        trade_date: str,
        model_name: str,
        run_id: str,
    ) -> dict[str, Path]:
        run_root = self.run_dir(ticker, trade_date, model_name, run_id)
        paths = {
            "run_root": run_root,
            "plots": run_root / "plots",
            "data": run_root / "data",
            "expiries": run_root / "expiries",
            "logs": run_root / "logs",
        }
        for p in paths.values():
            p.mkdir(parents=True, exist_ok=True)
        return paths
