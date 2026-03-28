from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path

from volstudy.db import VolStudyDB
from volstudy.models.base import FitRequest, FitResult
from volstudy.models.registry import ModelRegistry
from volstudy.paths import ProjectPaths
from volstudy.runners.r_runner import RScriptRunner


class RunManager:
    def __init__(self, project_root: str | Path):
        self.paths = ProjectPaths.from_root(project_root)
        self.paths.ensure_base_dirs()
        self.db = VolStudyDB(self.paths.db_path)
        self.db.initialize()
        self.registry = ModelRegistry(self.paths.config_dir / "models")

    @staticmethod
    def _utc_now() -> str:
        return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    def register_models(self) -> None:
        for model_def in self.registry.load_all_models():
            self.db.upsert_model(
                model_name=model_def.model_name,
                model_type=model_def.model_type,
                implementation_kind=model_def.implementation_kind,
                entrypoint=model_def.entrypoint,
                description=model_def.description,
                is_active=True,
            )

    def run_fit(self, request: FitRequest) -> FitResult:
        self.register_models()

        model_def = self.registry.load_model(request.model_name)
        model_id = self.db.get_model_id(request.model_name)

        run_id = self.paths.new_run_id()
        run_dirs = self.paths.ensure_run_dirs(
            ticker=request.ticker,
            trade_date=request.trade_date,
            model_name=request.model_name,
            run_id=run_id,
        )

        request.output_dir = run_dirs["run_root"]

        model_config_id = self.db.insert_model_config(
            model_id=model_id,
            config_name=f"{request.model_name}_default",
            config={**model_def.default_config, **request.config},
            created_at=self._utc_now(),
        )

        self.db.insert_fit_run(
            run_id=run_id,
            ticker=request.ticker,
            trade_date=request.trade_date,
            model_id=model_id,
            model_config_id=model_config_id,
            status="RUNNING",
            started_at=self._utc_now(),
            output_dir=str(run_dirs["run_root"]),
            notes=request.notes,
        )

        if model_def.implementation_kind.upper() != "R":
            raise NotImplementedError("Only R-backed models are implemented in Stage 1.")

        runner = RScriptRunner(model_def)
        merged_request = FitRequest(
            **{
                **request.__dict__,
                "config": {**model_def.default_config, **request.config},
            }
        )

        result = runner.fit(merged_request)

        self.db.update_fit_run_status(
            run_id=run_id,
            status="SUCCESS" if result.success else "FAILED",
            finished_at=self._utc_now(),
        )

        return result
