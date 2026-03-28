from volstudy.models.base import FitRequest
from volstudy.run_manager import RunManager


def main() -> None:
    mgr = RunManager(".")
    result = mgr.run_fit(
        FitRequest(
            ticker="SPY",
            trade_date="2024-01-05",
            model_name="svi_raw",
            expiry="2024-01-19",
            config={},
            notes="Stage 1 Python->R test run",
        )
    )

    print("Success:", result.success)
    print("Run ID :", result.run_id)
    print("Outdir :", result.output_dir)
    if result.stderr:
        print("\nSTDERR:\n", result.stderr)
    if result.stdout:
        print("\nSTDOUT:\n", result.stdout)


if __name__ == "__main__":
    main()
