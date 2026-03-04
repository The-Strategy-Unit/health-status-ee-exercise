# README
# Fit metalog distributions to expert params [p10, mode, p90]. Requires assumption that
# mode is equivalent to p50. This is not how it was framed to experts in the workshop.

import jax.numpy as jnp
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from metalog_jax.base import (
    MetalogBoundedness,
    MetalogFitMethod,
    MetalogInputData,
    MetalogParameters,
    MetalogRandomVariableParameters,
)
from metalog_jax.metalog import fit
from metalog_jax.utils import JaxUniformDistributionParameters
from scipy.stats import gaussian_kde

# read expert params ----
df = pd.read_csv("data/paper_table-1.csv")
print(df.head())


# wrangle expert params ----
def pivot_longer(df, id_vars="id"):
    df = df[
        ["id", "p10_f_r2", "mode_f_r2", "p90_f_r2", "p10_m_r2", "mode_m_r2", "p90_m_r2"]
    ]

    df = df[df["id"] != "Pooled"]

    df_long = df.melt(id_vars=id_vars, var_name="variable", value_name="value")

    df_long[["stat", "sex", "round"]] = df_long["variable"].str.extract(
        r"(p10|mode|p90)_(f|m)_(r\d+)"
    )

    df_long = (
        df_long.drop(columns="variable")
        .pivot_table(index=[id_vars, "sex", "round"], columns="stat", values="value")
        .reset_index()
    )

    df_long.columns.name = None
    df_long = df_long.rename(columns={"mode": "p50"})
    return df_long


df_triplet = pivot_longer(df, id_vars="id")

# fit metalogs to params and sample from metalogs ----
ml_params = MetalogParameters(
    boundedness=MetalogBoundedness.BOUNDED,
    method=MetalogFitMethod.OLS,
    lower_bound=0.0,
    upper_bound=100.0,
    num_terms=3,
)

uniform_prng_params = JaxUniformDistributionParameters(seed=42)

rv_params = MetalogRandomVariableParameters(
    prng_params=uniform_prng_params,
    size=1000,
)


def fit_and_sample(row):
    input_data = MetalogInputData.from_values(
        x=jnp.array([row["p10"], row["p50"], row["p90"]]),
        y=jnp.array([0.10, 0.50, 0.90]),
        precomputed_quantiles=True,
    )
    m = fit(input_data, ml_params)
    return m.rvs(rv_params)


df_triplet["samples"] = df_triplet.apply(fit_and_sample, axis=1)


# plot fitted metalogs ----
def plot_distributions(df):
    ids = df["id"].unique()
    colors = dict(zip(ids, [plt.get_cmap("tab10")(i) for i in range(len(ids))]))

    fig, axes = plt.subplots(1, 2, figsize=(12, 5), sharey=True)

    for ax, sex, title in zip(axes, ["f", "m"], ["Female", "Male"]):
        subset = df[df["sex"] == sex]

        for _, row in subset.iterrows():
            samples = np.array(row["samples"])
            kde = gaussian_kde(samples)
            x = np.linspace(0, 100, 300)
            ax.plot(x, kde(x), label=row["id"], color=colors[row["id"]], alpha=0.8)

        ax.set_xlim(0, 100)
        ax.set_title(title)
        ax.set_xlabel("Value")
        ax.set_ylabel("Density")
        ax.legend(title="ID")

    plt.suptitle("Metalog Distributions by Sex", y=1.02)
    plt.savefig("figures/metalog_workshop_r2_pdfs.png", bbox_inches="tight")
    plt.show()


plot_distributions(df_triplet)


# compare supplied p50 with empirical mode ----
def kde_mode(samples):
    s = np.array(samples)
    kde = gaussian_kde(s)
    x = np.linspace(0, 100, 10000)
    return x[np.argmax(kde(x))]


df_triplet["mode"] = df_triplet["samples"].apply(kde_mode)
df_triplet[["id", "sex", "p50", "mode"]].round(2)
