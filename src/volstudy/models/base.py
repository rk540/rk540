from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


@dataclass
class FitRequest:
    ticker: str
    trade_date: str
    model_name: str
    expiry: str | None = None
    expiries: list[str] | None = None
    expiry_start: str | None = None
    expiry_end: str | None = None
    all_expiries: bool = False
    config: dict[str, Any] = field(default_factory=dict)
    output_dir: Path | None = None
    notes: str | None = None


@dataclass
class FitResult:
    run_id: str
    model_name: str
    ticker: str
    trade_date: str
    output_dir: Path
    success: bool
    message: str | None = None
    stdout: str | None = None
    stderr: str | None = None


@dataclass
class ModelDefinition:
    model_name: str
    model_type: str
    implementation_kind: str
    entrypoint: str
    description: str | None = None
    default_config: dict[str, Any] = field(default_factory=dict)


class VolModelRunner:
    def fit(self, request: FitRequest) -> FitResult:
        raise NotImplementedError
