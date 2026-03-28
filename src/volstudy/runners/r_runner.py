from __future__ import annotations

import json
import subprocess
from pathlib import Path

from volstudy.models.base import FitRequest, FitResult, ModelDefinition


class RScriptRunner:
    def __init__(self, model_def: ModelDefinition):
        self.model_def = model_def

    def fit(self, request: FitRequest) -> FitResult:
        if request.output_dir is None:
            raise ValueError("FitRequest.output_dir must be set before running.")

        output_dir = Path(request.output_dir).expanduser().resolve()
        output_dir.mkdir(parents=True, exist_ok=True)

        run_id = output_dir.name

	payload = {
    	"ticker": request.ticker,
    	"trade_date": request.trade_date,
    	"model_name": request.model_name,
    	"expiry": request.expiry,
    	"expiries": request.expiries,
    	"expiry_start": request.expiry_start,
    	"expiry_end": request.expiry_end,
    	"all_expiries": request.all_expiries,
    	"config": request.config,
    	"output_dir": str(output_dir),
    	"notes": request.notes,
    	"project_root": str(Path.cwd().resolve()),
    	"r_dir": str((Path.cwd() / "r").resolve()),
    	"wrds_dir": str((Path.cwd() / "wrds").resolve()),
	}

        cmd = [
            "Rscript",
            self.model_def.entrypoint,
            json.dumps(payload),
        ]

        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
        )

        return FitResult(
            run_id=run_id,
            model_name=request.model_name,
            ticker=request.ticker,
            trade_date=request.trade_date,
            output_dir=output_dir,
            success=(proc.returncode == 0),
            message=None if proc.returncode == 0 else "R runner failed",
            stdout=proc.stdout,
            stderr=proc.stderr,
        )
