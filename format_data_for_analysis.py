"""
This script formats the data into the format it is used in the analysis. Outputs a csv file.
Additionally, it prints some statistics on the data if wanted
"""

import pandas as pd
from matplotlib import pyplot as plt
import numpy as np

INPUT_DATA = "data/projectFinalFile.csv"
OUTPUT_FILE = "data/SQ_cohort_data_in_days.csv"
DEPENDENT_VAR = "end_velocity_mean_days"
INDEPENDENT_VAR = "SQ/NonSQ"
INCLUDED_COLUMNS = ["full_name", "language", "#commitsBeforeFollowUp", "SQ/NonSQ", "#commitsDuringFollowUp",
                    "age_in_ASF", "root_project", "#DevsBeforeFollowUp", "end_mean", "start_mean_velocity"]
PRINTS = {"languages": True, "repo_descriptive_statistics": True, "df_info": True}


def timedelta_to_days_decimal(data, n_decimals=3):
    """
    The column is originally saved as timedelta (e.g. 14 days 19:47:42.594827586).
    This function converts it to decimal number (e.g. 14.792)
    :param data: Column of the data to be transformed
    :param n_decimals: Number of decimals used to save the data
    :return: The transformed data
    """
    timedelta_data = pd.to_timedelta(data)
    return timedelta_data.apply(lambda x: round(x.days + x.components.hours/24, n_decimals) if x.days >= 0 else round(x.days - x.components.hours/24, n_decimals))


def timedelta_to_days_int(data):
    """
    The column is originally saved as timedelta (e.g. 14 days 19:47:42.594827586).
    This function converts it to an integer (e.g. 14)
    :param data: Column of the data to be transformed
    :return: The transformed data
    """
    timedelta_data = pd.to_timedelta(data)
    return timedelta_data.apply(lambda x: x.days)


def print_descriptive_statistics(df_desc, outcome_var):

    print("---------------------------------------------")
    print(df_desc[INDEPENDENT_VAR].value_counts())
    print("All:   mean {:.2f} median {:.2f} std {:.2f}".format(df_desc[outcome_var].mean(),
                                                               df_desc[outcome_var].median(),
                                                               df_desc[outcome_var].std()))
    print("SQ:    mean {:.2f} median {:.2f} std {:.2f}".format(
        df_desc[df_desc[INDEPENDENT_VAR] == "SQ"][outcome_var].mean(),
        df_desc[df_desc[INDEPENDENT_VAR] == "SQ"][outcome_var].median(),
        df_desc[df_desc[INDEPENDENT_VAR] == "SQ"][outcome_var].std()))
    print("NonSQ: mean {:.2f} median {:.2f} std {:.2f}".format(
        df_desc[df_desc[INDEPENDENT_VAR] == "NonSQ"][outcome_var].mean(),
        df_desc[df_desc[INDEPENDENT_VAR] == "NonSQ"][outcome_var].median(),
        df_desc[df_desc[INDEPENDENT_VAR] == "NonSQ"][outcome_var].std()))
    print()


def main():

    # Read input data to a dataframe
    df = pd.read_csv(INPUT_DATA,  parse_dates=["age_in_ASF"])

    # Remove repositories which miss velocity information either at the start of end
    print("Number of repos with missing issues", len(df[df["difference_of_means"] == "NaT"]))
    df = df[df["difference_of_means"] != "NaT"]
    print("Number of repositories with velocity data", len(df))
    print()

    # Format some columns into a more usable format
    df["age"] = timedelta_to_days_int(df["age_in_ASF"])
    df["age_months"] = round(df["age"] / 30, 2)
    df["end_velocity_mean_days"] = timedelta_to_days_decimal(df["end_mean"])
    df["start_velocity_mean_days"] = timedelta_to_days_decimal(df["start_mean_velocity"])
    df["overall_issues"] = df["open_issues_at_start"] + df["n_closed_issues_start"]
    df["exposed"] = np.where(df[INDEPENDENT_VAR] == "SQ", 1, 0)

    if PRINTS["df_info"]:
        print(df.info())

    # Print information about the language distribution of the projects
    if PRINTS["languages"]:
        print("\nTotal number of different languages", len(df["language"].unique()))
        print(df[["language", INDEPENDENT_VAR]].value_counts())

    if PRINTS["repo_descriptive_statistics"]:
        # Print statistics for projects not using Java
        print("\nDescriptive statistics for non-Java projects")
        print_descriptive_statistics(df[df["language"] != "Java"], DEPENDENT_VAR)

        # Print statistics for projects using Java
        print("Descriptive statistics for Java projects")
        print_descriptive_statistics(df[df["language"] == "Java"], DEPENDENT_VAR)

    # Exclude non-Java projects from the data
    df = df[df["language"] == "Java"]

    # Clean the data set a bit
    df["full_name"] = df["full_name"].str.replace("apache/", "")
    df.rename(columns={"#commitsBeforeFollowUp": "commits", "#DevsBeforeFollowUp": "devs"}, inplace=True)

    # Save data to a csv file
    # If you want more of the available information, add it to the list or remove the columns parameter from the
    # to_csv function
    included_columns = ["full_name", "SQ/NonSQ", "exposed", "end_velocity_mean_days", "commits", "devs", "age",
                        "overall_issues", "start_velocity_mean_days"]
    df.to_csv(OUTPUT_FILE, index=False, columns=included_columns)


if __name__ == "__main__":
    main()
