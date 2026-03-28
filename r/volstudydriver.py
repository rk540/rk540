import streamlit as st
from volstudy.services import FitService, RunRepository

st.set_page_config(layout="wide")

st.title("Volatility Surface Research Platform")

tab1, tab2, tab3, tab4 = st.tabs([
    "Run Experiments",
    "Browse Runs",
    "Compare Models",
    "Stability"
])

# ------------------------
# TAB 1: RUN EXPERIMENTS
# ------------------------
with tab1:
    st.header("Run New Fit")

    ticker = st.text_input("Ticker", "SPY")
    trade_date = st.date_input("Trade Date")

    models = st.multiselect(
        "Models",
        options=["svi_raw", "quadratic_logmoneyness"]
    )

    if st.button("Run Fit"):
        for model in models:
            run_id = FitService.run_fit(
                ticker=ticker,
                trade_date=str(trade_date),
                model=model
            )
            st.success(f"Started run: {run_id}")

# ------------------------
# TAB 2: BROWSE RUNS
# ------------------------
with tab2:
    st.header("Run History")

    runs = RunRepository.list_runs()

    st.dataframe(runs)

# ------------------------
# TAB 3: COMPARE
# ------------------------
with tab3:
    st.header("Model Comparison")

    ticker = st.text_input("Ticker", "SPY", key="cmp_ticker")
    date = st.date_input("Date", key="cmp_date")

    models = st.multiselect(
        "Models",
        options=["svi_raw", "quadratic_logmoneyness"]
    )

    if st.button("Compare"):
        fig = AnalysisService.compare_models(
            ticker, str(date), models
        )
        st.pyplot(fig)

# ------------------------
# TAB 4: STABILITY
# ------------------------
with tab4:
    st.header("Model Stability")

    ticker = st.text_input("Ticker", "SPY", key="stab_ticker")
    model = st.selectbox("Model", ["svi_raw"])

    start = st.date_input("Start Date")
    end = st.date_input("End Date")

    if st.button("Analyze Stability"):
        fig = AnalysisService.compute_stability(
            ticker, model, str(start), str(end)
        )
        st.pyplot(fig)
