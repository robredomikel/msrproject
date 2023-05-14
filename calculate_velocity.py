import pandas as pd
from datetime import datetime
import numpy as np
import os
import csv

STATUS_COL = "state"
OPENED_COL = "created_at"
CLOSED_COL = "closed_at"
DATE_FORMAT = "%Y-%m-%dT%H:%M:%SZ"  # In case pandas doesn't figure out the dates automatically, specify format to it

VELOCITY_START_1 = datetime.strptime("1-7-2020", '%d-%m-%Y')
VELOCITY_START_2 = datetime.strptime("31-12-2020", '%d-%m-%Y')
VELOCITY_END_1 = datetime.strptime("1-7-2022", '%d-%m-%Y')
VELOCITY_END_2 = datetime.strptime("31-12-2022", '%d-%m-%Y')
INPUT_FOLDER = "commonIssueFolder"


def get_velocity(start_time, end_time, df):

    # Add a boolean column in the dataframe which indicates whether the issue was fixed within time frame
    df["fixed_during"] = np.where((start_time <= df[CLOSED_COL]) & (df[CLOSED_COL] <= end_time), True, False)
    # Calculate number of issues created during the time frame
    n_issues_created_during = np.count_nonzero((start_time <= df[OPENED_COL]) & (df[OPENED_COL] <= end_time))
    # Calculate number of issues open at the start of the time frame
    n_open_at_start = np.count_nonzero(
        (df[OPENED_COL] <= start_time) & ((pd.isnull(df[CLOSED_COL])) | (df[CLOSED_COL] > start_time)))

    closed_issues = df.loc[df["fixed_during"] == True]
    fixed_mean = closed_issues["open_time"].mean()
    fixed_median = closed_issues["open_time"].median()
    n_closed_issues = len(closed_issues)

    return fixed_mean, fixed_median, n_open_at_start, n_closed_issues, n_issues_created_during


if __name__ == "__main__":

    # Not sure if we need all columns as it seemed like something we might need. Feel free to add/remove columns
    # Project name: name of the project
    # n_closed_issues_start: number of issues closed during the start velocity time frame
    # start_mean_velocity: mean time to close issues fixed within the start velocity time frame
    # start_median_velocity: median time to close issues fixed in the start velocity time frame
    # open_issues_at_at_start: number of open issues at the start of start velocity time frame
    # n_created_issues_start: number of issues created during the start velocity time frame
    # The rest of the variables are the same but for the end velocity time frame
    output = [["full_name", "n_closed_issues_start", "start_mean_velocity", "start_median_velocity",
               "open_issues_at_start", "n_created_issues_start", "n_closed_issues_end", "end_mean", "end_median",
               "open_issues_at_end", "difference_of_means", "difference_of_medians", "n_created_issues_end"]]

    # iterate issue csv files in the input folder
    for filename in os.listdir(INPUT_FOLDER):

        if filename[-4:] != ".csv":
            print("File not a csv file", filename)
            continue

        project_name = 'apache/' + filename[:-4]  # I assumed the project name is the name of the csv file
        df = pd.read_csv(os.path.join(INPUT_FOLDER, filename), usecols=[STATUS_COL, OPENED_COL, CLOSED_COL],
                         parse_dates=[OPENED_COL, CLOSED_COL])

        # Calculate time the issue was opened. Value is NaT if it's still open.
        df["open_time"] = df[CLOSED_COL] - df[OPENED_COL]

        # Calculate velocity at the start and end of follow-up period
        start_mean, start_median, start_n_issues, n_closed_issues_start, n_created_issues_start = get_velocity(
            VELOCITY_START_1, VELOCITY_START_2, df)
        end_mean, end_median, end_n_issues, n_closed_issues_end, n_created_issues_end = get_velocity(
            VELOCITY_END_1, VELOCITY_END_2, df)
        mean_difference = end_mean - start_mean
        median_difference = end_median - start_median

        output.append([project_name, n_closed_issues_start, start_mean, start_median, start_n_issues,
                       n_created_issues_start, n_closed_issues_end, end_mean, end_median, end_n_issues, mean_difference,
                       median_difference, n_created_issues_end])

    # write velocities to a csv file
    with open("resultFiles/velocity_data(githubIssues)30032023.csv", "w") as f:
        write = csv.writer(f)
        write.writerows(output)

    #df.to_csv("data/issues/output.csv", index=False)
