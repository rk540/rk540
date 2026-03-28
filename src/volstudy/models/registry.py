from __future__ import annotations

from pathlib import Path
import yaml

from volstudy.models.base import ModelDefinition


class ModelRegistry:
    def __init__(self, models_dir: str | Path):
        self.models_dir = Path(models_dir).expanduser().resolve()

    def list_model_files(self) -> list[Path]:
        return sorted(self.models_dir.glob("*.yaml"))

    def load_model(self, model_name: str) -> ModelDefinition:
        model_file = self.models_dir / f"{model_name}.yaml"
        if not model_file.exists():
            raise FileNotFoundError(f"Model config not found: {model_file}")

        with open(model_file, "r", encoding="utf-8") as f:
            data = yaml.safe_load(f)

        return ModelDefinition(
            model_name=data["model_name"],
            model_type=data["model_type"],
            implementation_kind=data["implementation_kind"],
            entrypoint=data["entrypoint"],
            description=data.get("description"),
            default_config=data.get("default_config", {}),
        )

    def load_all_models(self) -> list[ModelDefinition]:
        out: list[ModelDefinition] = []
        for path in self.list_model_files():
            with open(path, "r", encoding="utf-8") as f:
                data = yaml.safe_load(f)
            out.append(
                ModelDefinition(
                    model_name=data["model_name"],
                    model_type=data["model_type"],
                    implementation_kind=data["implementation_kind"],
                    entrypoint=data["entrypoint"],
                    description=data.get("description"),
                    default_config=data.get("default_config", {}),
                )
            )
        return out
