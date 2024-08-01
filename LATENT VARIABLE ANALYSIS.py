# By Sajid Farook for CS109; 6/4/2023

import pandas as pd
import numpy as np
import tkinter as tk
from tkinter import messagebox, simpledialog
import sys
from PIL import ImageTk, Image


def read_data():
    df = pd.read_csv('latent_data.csv')
    return df

# Converting input for opposition difficulty into composite score.
def convertOppToLatScore(O):
    breaks = [0.5, 0.8, 1.1, 1.3, 1.5, 1.7, 2.1, 2.8]
    labels = [1, 2, 3, 4, 5, 6, 7]
    compVal = pd.cut([O], bins=breaks, labels=labels, include_lowest=True)
    return float(str(compVal[0]))

# Converting input for form into composite score.
def convertFormToLatScore(F_val):
    breaks = [1.0, 3.0, 4.0, 6.0, 8.0, 10.0, 17.0]
    labels = [1, 2, 3, 4, 5, 6]
    compVal = pd.cut([F_val], bins=breaks, labels=labels, include_lowest=True)
    return float(str(compVal[0]))

# Converting input for quality into composite score.
def convertTPToLatScore(Q):
    breaks = [1.0, 3.0, 4.0, 6.0, 8.0, 10.0, 17.0]
    labels = [1, 2, 3, 4, 5, 6]
    compVal = pd.cut([Q], bins=breaks, labels=labels, include_lowest=True)
    return float(str(compVal[0]))

# Converting input for xGi into composite score.
def convertXToLatScore(X):
    breaks = [0, 0.15, 0.35, 0.5, 0.7, 1, 1.75]
    labels = [1, 2, 3, 4, 5, 6]
    compVal = pd.cut([X], bins=breaks, labels=labels, include_lowest=True)
    return float(str(compVal[0]))

# Converting composite scores for form and xGi into one variable
def convertXandFToLatScore(X, F_val):
    sum_val = X + F_val
    breaks = [1, 3, 5, 7, 10, 13, 18.75]
    labels = [1, 2, 3, 4, 5, 6]
    compVal = pd.cut([sum_val], bins=breaks, labels=labels, include_lowest=True)
    return float(str(compVal[0]))

# Converting training data into those composite scores
def convertDataToLat(data):
    data_lat = data.copy()
    data_lat['Opp_GC_90'] = data['Opp_GC_90'].apply(convertOppToLatScore)
    data_lat['form'] = data['form'].apply(convertFormToLatScore)
    data_lat['prev_tp_avg'] = data['prev_tp_avg'].apply(convertTPToLatScore)
    data_lat['xGipg_last7'] = data['xGipg_last7'].apply(convertXToLatScore)
    data_lat['comp_form'] = data.apply(lambda row: convertXandFToLatScore(row['form'], row['xGipg_last7']), axis=1)
    return data_lat

# Counts entries in dataset to calculate P(Y|O,Q,F,X) using definition of conditional probability
def calc_prob_YgOQR(Y, O, Q, F_val, X, data_lat):
    O = convertOppToLatScore(O)
    Q = convertTPToLatScore(Q)
    R_temp1 = convertFormToLatScore(F_val)
    R_temp2 = convertXToLatScore(X)
    R = convertXandFToLatScore(R_temp1, R_temp2)

    subset_Y = data_lat[data_lat['total_points'] == Y]
    subset_YOQR = subset_Y[(subset_Y['Opp_GC_90'] == O) & (subset_Y['prev_tp_avg'] == Q) & (subset_Y['comp_form'] == R)]
    subset_OQR = data_lat[(data_lat['Opp_GC_90'] == O) & (data_lat['prev_tp_avg'] == Q) & (data_lat['comp_form'] == R)]

    numerator = subset_YOQR.shape[0]
    denominator = subset_OQR.shape[0]

    if denominator == 0:
        return 'null'
    return numerator / denominator

# Iterates through all possible values of Y to calculate E(Y|O,Q,F,X) using definition of expectation and above
# function for P(Y|O,Q,F,X)
def E_YgOQR(O, F_val, Q, X, data_lat):
    count = 0
    for y in range(1, 31):
        prob = calc_prob_YgOQR(y, O, F_val, Q, X, data_lat)
        if prob == 'null':
            return "Parameters are not possible given the training data - try modifying inputs to be less extreme"
        count += y * prob
    if np.isfinite(count):
        return count
    else:
        return "Parameters are not possible given the training data - try modifying inputs to be less extreme"

# Configures pop-up window that receives inputs from client and outputs prediction
def show_input_dialog(data_lat):
    root = tk.Tk()
    root.withdraw()
    icon_image = ImageTk.PhotoImage(Image.open("soccerballpic.png"))
    root.iconphoto(True, icon_image)


    messagebox.showinfo("Welcome", "Welcome to the Fantasy Premier League Football Points Predictor Tool!\n\n"
                                   "To make a prediction, we'll need some information about the player "
                                   "you are thinking about selecting.")

    opponent_goals = simpledialog.askfloat("Input", "Input the average number of goals per game conceded by the "
                                                   "opponent team:")
    player_points = simpledialog.askfloat("Input", "Now, input the average points per game scored by the player:")
    player_form = simpledialog.askfloat("Input", "Now, input the average points per game scored by the player in the past 5 games only:")
    player_xGi = simpledialog.askfloat("Input",
                                        "Now, input the average xGi of the player in the past 7 games:")

    expectation = E_YgOQR(opponent_goals, player_form, player_points, player_xGi, data_lat)

    if isinstance(expectation, str):
        messagebox.showinfo("Result", f"The parameters you entered are not possible given the training data. Try modifying your inputs to be less extreme.")
    else:
        messagebox.showinfo("Result", f"You entered:\n\nOpponent goals conceded: {opponent_goals}\n"
                                  f"Player points per game: {player_points}\n"
                        f"Form: {player_form}\n"
                        f"Recent xGi per game: {player_xGi}\n\n"
                        f"Based on this, we predict the player to score:\n {round(expectation, 2)} points.")

    root.destroy()
    sys.exit()
    root.mainloop()

# Loads in data and then runs the pop-up to accept inputs from client
def main():
    try:
        data_lat = pd.read_csv('data_lat.csv')
        print("Loaded pre-converted data_lat from file.")
    except FileNotFoundError:
        print("Pre-converted data_lat file not found. Building data_lat...")
        df_raw = read_data()
        data_lat = convertDataToLat(df_raw)
        data_lat.to_csv('data_lat.csv', index=False)

    show_input_dialog(data_lat)

if __name__ == '__main__':
    main()