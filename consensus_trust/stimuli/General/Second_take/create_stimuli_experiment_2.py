import numpy as np
import plotly.express as px
import matplotlib
import matplotlib.pyplot as plt
import os

# This time, we want to have the same means between different conditions.
# We want to make a different draw between condition / each sample. But we have X iteration per condition, with predefined means

plt.rcParams["figure.figsize"] = (18, 9)
plt.style.use("ggplot")


def create_normal_distrib(mean, std, size, seed):
    rng = np.random.default_rng(seed)
    normal_distrib = rng.normal(loc=mean, scale=std, size=size)
    return normal_distrib


def create_draw_experiment(sample_to_draw, seed, npoints=3, mean_factor=1, y=1000):
    rng = np.random.default_rng(seed)
    mean_sample_x = list(rng.choice(sample_to_draw, size=npoints * mean_factor))
    sample_x = [0] * npoints
    i = 0
    j = 0
    while j < npoints:
        k = 0
        while k < mean_factor:
            sample_x[j] = sample_x[j] + mean_sample_x[i]
            i += 1
            k += 1

        sample_x[j] = sample_x[j] / mean_factor
        j += 1
    sample_y = [y] * npoints

    return (sample_x, sample_y)


def create_one_plot(
    sample_x,
    sample_y,
    x_line,
    y_line,
    xmin,
    xmax,
    ymin,
    ymax,
    mean,
    seed,
    highlight_arrow=False,
):

    plt.rc("font", size=25)
    plt.title(f"mean = {mean}")
    figure, axis = plt.subplots(1, 1, figsize=(20, 3))
    axis.set_xlim(xmin=xmin, xmax=xmax)
    plt.setp(axis, xticks=[1000, 1250, 1500, 1750, 2000])
    axis.set_ylim(ymin=ymin, ymax=ymax)
    axis.get_yaxis().set_visible(False)
    axis.grid(False)
    if highlight_arrow:
        rng = np.random.default_rng(seed)
        highlighted_arrow_index = rng.integers(low=0, high=len(sample_x))
        for index_arrow in range(len(sample_x)):
            if index_arrow == highlighted_arrow_index:
                colorVal = "red"
            else:
                colorVal = "blue"
            axis.arrow(
                x=sample_x[index_arrow],
                y=1500,
                dx=0,
                dy=-500,
                width=4,
                head_length=150,
                length_includes_head=True,
                color=colorVal,
            )
    elif not highlight_arrow:
        for x_arrow in sample_x:
            axis.arrow(
                x=x_arrow,
                y=1500,
                dx=0,
                dy=-500,
                width=4,
                head_length=150,
                length_includes_head=True,
                color="blue",
            )

    axis.errorbar(x_line, y_line, yerr=75, alpha=0.7)

    return figure


def create_one_plot_from_scratch(
    mean,
    std,
    npoints,
    mean_factor,
    seed,
    x_line,
    y_line,
    xmin,
    xmax,
    ymin,
    ymax,
    highlight_arrow=False,
):

    sample_x = create_normal_distrib(mean=mean, std=std, size=npoints, seed=seed)

    sample_y = [1000] * len(sample_x)
    figure = create_one_plot(
        sample_x=sample_x,
        sample_y=sample_y,
        x_line=x_line,
        y_line=y_line,
        xmin=xmin,
        xmax=xmax,
        ymin=ymin,
        ymax=ymax,
        mean=mean,
        seed=seed,
        highlight_arrow=highlight_arrow,
    )

    return figure


def create_stimuli_experiment_same_mean_between_conditions(
    start_mean=1300,
    end_mean=1700,
    list_std=[20, 150],
    list_npoints=[2, 7],
    n_iteration_per_condition=2,
    n_samples=5,
    mean_factor=1,
    seed=0,
    xmin=1000,
    xmax=2000,
    ymin=800,
    ymax=1500,
    highlight_arrow=False,
):

    """
    Final Function to call to save the figures on your computer
    """

    list_mean = list(np.linspace(start_mean, end_mean, n_iteration_per_condition))
    list_conditions = [(std, npoints) for std in list_std for npoints in list_npoints]

    # Plotting
    x_line = [1000, 1250, 1500, 1750, 2000]
    y_line = [1000] * len(x_line)
    for sample in range(n_samples):
        data_dir = os.path.join(
            os.getcwd(),
            f"experiment_2_seed_{seed}_sample{sample+1}_npoints_{list_npoints}_std_{list_std}_highlight_{highlight_arrow}",
        )

        try:
            os.mkdir(data_dir)

        except FileExistsError:
            pass

        for iteration in range(len(list_mean)):
            for icondition in range(len(list_conditions)):
                figure = create_one_plot_from_scratch(
                    mean=list_mean[iteration],
                    std=list_conditions[icondition][0],
                    npoints=list_conditions[icondition][1],
                    mean_factor=mean_factor,
                    seed=seed,
                    x_line=x_line,
                    y_line=y_line,
                    xmin=xmin,
                    xmax=xmax,
                    ymin=ymin,
                    ymax=ymax,
                    highlight_arrow=highlight_arrow,
                )

                figure.savefig(
                    fname=f"{data_dir}\\mean_{list_mean[iteration]}_std_{list_conditions[icondition][0]}_sample_{sample+1}_seed_{seed}_npoints_{list_conditions[icondition][1]}_highlight_{highlight_arrow}_mean_factor_{mean_factor}.png",
                    bbox_inches="tight",
                    transparent=True,
                )


res = create_stimuli_experiment_same_mean_between_conditions(
    start_mean=1300,
    end_mean=1700,
    list_std=[20, 150],
    list_npoints=[3],
    n_iteration_per_condition=2,
    n_samples=1,
    mean_factor=1,
    seed=0,
    xmin=999,
    xmax=2001,
    ymin=800,
    ymax=1500,
    highlight_arrow=False,
)
