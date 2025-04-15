import numpy as np
import plotly.express as px
import matplotlib
import matplotlib.pyplot as plt
import os

plt.rcParams["figure.figsize"] = (18, 9)
plt.style.use("ggplot")


def create_normal_distrib(mean, std, size, seed):
    rng = np.random.default_rng(seed)
    normal_distrib = rng.normal(loc=mean, scale=std, size=size)
    return normal_distrib


def create_draw_experiment(sample_to_draw, seed, size_sample=6, mean_factor=5, y=1000):
    rng = np.random.default_rng(seed)
    mean_small_sample_x = list(
        rng.choice(sample_to_draw, size=size_sample * mean_factor)
    )
    small_sample_x = [0] * size_sample
    i = 0
    j = 0
    while j < size_sample:
        k = 0
        while k < mean_factor:
            small_sample_x[j] = small_sample_x[j] + mean_small_sample_x[i]
            i += 1
            k += 1

        small_sample_x[j] = small_sample_x[j] / mean_factor
        j += 1
    small_sample_y = [y] * size_sample

    return (small_sample_x, small_sample_y)


def create_one_plot(sample_x, sample_y, x_line, y_line, xmin, xmax, ymin, ymax):
    plt.rc("font", size=25)
    figure, axis = plt.subplots(1, 1, figsize=(20, 3))
    axis.set_xlim(xmin=xmin, xmax=xmax)
    plt.setp(axis, xticks=[1000, 1250, 1500, 1750, 2000])
    axis.set_ylim(ymin=ymin, ymax=ymax)
    axis.get_yaxis().set_visible(False)
    axis.grid(False)
    for x_arrow in sample_x:
        axis.arrow(
            x=x_arrow,
            y=1500,
            dx=0,
            dy=-500,
            width=4,
            head_length=150,
            length_includes_head=True,
        )

    axis.errorbar(x_line, y_line, yerr=75, alpha=0.7)

    return figure


def create_one_plot_arrows_diff_colors(
    sample_x, sample_y, x_line, y_line, xmin, xmax, ymin, ymax, seed
):
    plt.rc("font", size=25)
    figure, axis = plt.subplots(1, 1, figsize=(20, 3))
    axis.set_xlim(xmin=xmin, xmax=xmax)
    plt.setp(axis, xticks=[1000, 1250, 1500, 1750, 2000])
    axis.set_ylim(ymin=ymin, ymax=ymax)
    axis.get_yaxis().set_visible(False)
    axis.grid(False)
    rng = np.random.default_rng(seed)
    highlighted_arrow_index = rng.integers(low = 0, high = len(sample_x))

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
            color = colorVal
        )

    axis.errorbar(x_line, y_line, yerr=75, alpha=0.7)
    return figure


def create_pairings(list_mean, list_std, list_npoints, n_iteration_per_condition, seed):

    res = np.empty((n_iteration_per_condition, len(list_npoints) + len(list_std)))
    rng = np.random.default_rng(seed)
    temp_list_mean = list_mean.copy()
    for i_iter in range(n_iteration_per_condition):
        i_res = 0
        for i_std in range(len(list_std)):
            for i_npoints in range(len(list_npoints)):
                i_mean = rng.choice(len(temp_list_mean))
                mean = temp_list_mean.pop(i_mean)
                res[i_iter, i_res] = mean
                i_res += 1
    return res


def create_one_plot_from_scratch(
    mean,
    std,
    size_sample,
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

    sample = create_normal_distrib(mean=mean, std=std, size=10000, seed=seed)

    sample_x, sample_y = create_draw_experiment(
        sample_to_draw=sample,
        size_sample=size_sample,
        mean_factor=mean_factor,
        seed=seed,
    )
    if not highlight_arrow:
        figure = create_one_plot(
            sample_x=sample_x,
            sample_y=sample_y,
            x_line=x_line,
            y_line=y_line,
            xmin=xmin,
            xmax=xmax,
            ymin=ymin,
            ymax=ymax,
        )
    elif highlight_arrow:
        figure = create_one_plot_arrows_diff_colors(
            sample_x=sample_x,
            sample_y=sample_y,
            x_line=x_line,
            y_line=y_line,
            xmin=xmin,
            xmax=xmax,
            ymin=ymin,
            ymax=ymax,
            seed=seed,
        )

    return figure


def create_stimuli_experiment(
    start_mean=1200,
    end_mean=1800,
    list_std=[20, 150],
    list_npoints=[2, 7],
    n_iteration_per_condition=2,
    n_samples=10,
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

    # Parameters
    small_std = list_std[0]
    big_std = list_std[1]
    small_size_sample = list_npoints[0]
    big_size_sample = list_npoints[1]

    n_conditions = (len(list_std) * len(list_npoints)) * n_iteration_per_condition

    list_mean = list(np.linspace(start_mean, end_mean, n_conditions))

    # Plotting
    # x_line = list(range(xmin, xmax + 250, 250))
    x_line = [750, 1000, 1250, 1500, 1750, 2000]
    y_line = [1000] * len(x_line)

    dict_conditions = {
        0: "convergent_small",
        1: "convergent_large",
        2: "nonconvergent_small",
        3: "nonconvergent_large",
    }

    final_result = np.empty(
        shape=(4, n_iteration_per_condition, n_samples), dtype="object"
    )

    for sample in range(n_samples):
        list_of_means = create_pairings(
            list_mean, list_std, list_npoints, n_iteration_per_condition, seed + sample
        )
        fig_list = np.empty((4, n_iteration_per_condition), dtype="object")
        for iteration in range(n_iteration_per_condition):

            fig_list[0, iteration] = create_one_plot_from_scratch(
                mean=list_of_means[iteration][0],
                std=small_std,
                size_sample=small_size_sample,
                mean_factor=mean_factor,
                seed=seed + sample + iteration,
                x_line=x_line,
                y_line=y_line,
                xmin=xmin,
                xmax=xmax,
                ymin=ymin,
                ymax=ymax,
                highlight_arrow=highlight_arrow
            )
            fig_list[1, iteration] = create_one_plot_from_scratch(
                mean=list_of_means[iteration][1],
                std=small_std,
                size_sample=big_size_sample,
                mean_factor=mean_factor,
                seed=seed + sample + iteration,
                x_line=x_line,
                y_line=y_line,
                xmin=xmin,
                xmax=xmax,
                ymin=ymin,
                ymax=ymax,
                highlight_arrow=highlight_arrow
            )
            fig_list[2, iteration] = create_one_plot_from_scratch(
                mean=list_of_means[iteration][2],
                std=big_std,
                size_sample=small_size_sample,
                mean_factor=mean_factor,
                seed=seed + sample + iteration,
                x_line=x_line,
                y_line=y_line,
                xmin=xmin,
                xmax=xmax,
                ymin=ymin,
                ymax=ymax,
                highlight_arrow=highlight_arrow
            )
            fig_list[3, iteration] = create_one_plot_from_scratch(
                mean=list_of_means[iteration][3],
                std=big_std,
                size_sample=big_size_sample,
                mean_factor=mean_factor,
                seed=seed + sample + iteration,
                x_line=x_line,
                y_line=y_line,
                xmin=xmin,
                xmax=xmax,
                ymin=ymin,
                ymax=ymax,
                highlight_arrow=highlight_arrow
            )

            final_result[:, iteration, sample] = fig_list[:, iteration]

    for sample in range(n_samples):
        data_dir = os.path.join(
            os.getcwd(), f"seed_{seed}_sample{sample+1}_npoints_{list_npoints}_highlight_{highlight_arrow}"
        )

        try:
            os.mkdir(data_dir)

        except FileExistsError:
            pass

        for iteration in range(n_iteration_per_condition):

            for i_figure in range(len(final_result[:, iteration, sample])):
                figure = final_result[i_figure, iteration, sample]
                condition = dict_conditions[i_figure]
                figure.savefig(
                    fname=f"{data_dir}\\{condition}_iteration{iteration+1}_sample{sample+1}_seed{seed}_npoints_{list_npoints}_highlight_{highlight_arrow}.png",
                    bbox_inches="tight",
                    transparent=True,
                )
    return final_result


res = create_stimuli_experiment(
    start_mean=1300,
    end_mean=1700,
    list_std=[20,150],
    list_npoints=[3,10],
    n_iteration_per_condition=2,
    n_samples=3,
    mean_factor=1,
    seed=0,
    xmin=999,
    xmax=2001,
    ymin=800,
    ymax=1500,
    highlight_arrow=True
)
