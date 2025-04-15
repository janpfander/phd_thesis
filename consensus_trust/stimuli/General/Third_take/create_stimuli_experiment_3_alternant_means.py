import numpy as np
import plotly.express as px
import matplotlib
import matplotlib.pyplot as plt
import os

# This time, we do not use a normal distribution but instead a uniform draw within a given span
# We want to make a different draw between condition / each sample. But we have X iteration per condition, with predefined means

plt.rcParams["figure.figsize"] = (18, 9)
plt.style.use("ggplot")


def create_uniform_draw(mean, span, npoints, seed):
    rng = np.random.default_rng(seed)
    list_values = np.arange(start=mean - span / 2, stop=(mean + span / 2), step=5)
    uniform_draw = rng.choice(list_values, size=npoints, replace=False)

    return uniform_draw


def create_normal_draw(mean, std, npoints, seed):
    rng = np.random.default_rng(seed)
    normal_draw = rng.normal(mean, std, size=npoints)

    return normal_draw


def create_one_plot(
    sample_x, x_line, y_line, xmin, xmax, ymin, ymax, mean, seed, highlight_arrow=False,
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
    span,
    npoints,
    seed,
    x_line,
    y_line,
    xmin,
    xmax,
    ymin,
    ymax,
    highlight_arrow=False,
    distrib_type="uniform",
):

    if distrib_type == "uniform":

        sample_x = create_uniform_draw(mean=mean, span=span, npoints=npoints, seed=seed)

    elif distrib_type == "normal":

        sample_x = create_normal_draw(mean=mean, std=span, npoints=npoints, seed=seed)

    apparent_mean_sample = np.mean(sample_x)

    figure = create_one_plot(
        sample_x=sample_x,
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

    return figure, int(apparent_mean_sample)

def push_values(l):
    newlist = [0] * len(l)
    i = 1
    while i < len(l) -1:
        newlist[i] = l[i+1]
        i+=1
    newlist[0] = l[len(l)]
    newlist[len(l)] = l[0]

    return(newlist)

def create_stimuli_experiment_uniform_draw(
    start_mean=1300,
    end_mean=1700,
    list_span=[20, 150],
    list_npoints=[2, 7],
    n_iteration_per_condition=2,
    n_samples=5,
    global_seed=0,
    xmin=1000,
    xmax=2000,
    ymin=800,
    ymax=1500,
    highlight_arrow=False,
    distrib_type="normal",
):

    """
    Final Function to call to save the figures on your computer
    """

    list_conditions = [
        (span, npoints) for span in list_span for npoints in list_npoints
    ]
    list_mean = list(
        np.linspace(
            start_mean,
            end_mean,
            len(list_conditions) * n_iteration_per_condition,
            dtype=int,
        )
    )

    generator_list_seed = np.random.default_rng(global_seed)
    list_seeds = np.arange(0, (len(list_mean) * n_samples + 1) * 1000)
    list_seeds = generator_list_seed.choice(
        list_seeds, size=len(list_mean) * n_samples + 1, replace=False,
    )



    # Plotting
    x_line = [1000, 1250, 1500, 1750, 2000]
    y_line = [1000] * len(x_line)
    seed_counter = 0

    for sample in range(n_samples):
        data_dir = os.path.join(
            os.getcwd(),
            f"experiment_3_alternant_means_globalseed_{global_seed}_sample{sample+1}_npoints_{list_npoints}_span_{list_span}_highlight_{highlight_arrow}_distrib_{distrib_type}",
        )

        try:
            os.mkdir(data_dir)

        except FileExistsError:
            pass

        mean_counter = 0


        for iteration in range(n_iteration_per_condition):
            for icondition in range(len(list_conditions)):
                figure, apparent_mean = create_one_plot_from_scratch(
                    mean=list_mean[mean_counter],
                    span=list_conditions[icondition][0],
                    npoints=list_conditions[icondition][1],
                    seed=list_seeds[seed_counter],
                    x_line=x_line,
                    y_line=y_line,
                    xmin=xmin,
                    xmax=xmax,
                    ymin=ymin,
                    ymax=ymax,
                    highlight_arrow=highlight_arrow,
                    distrib_type=distrib_type,
                )
                seed_counter += 1

                figure.savefig(
                    fname=f"{data_dir}\\mean_{list_mean[mean_counter]}_span_{list_conditions[icondition][0]}_sample_{sample+1}_seed_{list_seeds[seed_counter]}_npoints_{list_conditions[icondition][1]}_apparent_mean_{apparent_mean}.png",
                    bbox_inches="tight",
                    transparent=True,
                )
                mean_counter += 1
        list_mean = push_values(list_mean)


res = create_stimuli_experiment_uniform_draw(
    start_mean=1300,
    end_mean=1700,
    list_npoints=[3],
    list_span=[60, 600],
    n_iteration_per_condition=2,
    n_samples=2,
    global_seed=42,
    xmin=999,
    xmax=2001,
    ymin=800,
    ymax=1500,
    highlight_arrow=False,
    distrib_type="uniform",
)
