from __future__ import annotations

from volstudy.paths import ProjectPaths
from volstudy.db import VolStudyDB


def main() -> None:
    paths = ProjectPaths.from_root(".")
    paths.ensure_base_dirs()

    db = VolStudyDB(paths.db_path)
    db.initialize()

    print(f"Initialized database at: {paths.db_path}")


if __name__ == "__main__":
    main()
