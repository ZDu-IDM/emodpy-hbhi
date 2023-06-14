import pandas as pd
import json
import os
import sys
sys.path.append('../')
from simulation.load_paths import load_box_paths

data_path, project_path = load_box_paths(country_name='Nigeria')
admin_names = ['Bindawa', 'Fakai', 'Markafi', 'Ikot Abasi']
round_numbers = [5, 4, 10, 3]   # v5: [5, 3, 4, 4]
constant_habitat = 4  # must match what was used for seasonality calibration
suffix = ['_maxInc100', '_maxInc100', '_maxInc100', '_maxInc100']  # v5: ['', '', '', '']
version = [6, 6, 6, 6]  # v5: [5, 5, 5, 5]

# save the best-fit vector habitat parameters for each archetype across all calibration simulations
#   version using the CalibManager.json file
def save_best_seasonality_fit(project_path, experiment_names, admin_names):

    # create data frame to store results for all admins and seasonality parameters
    seasonality_fits = pd.DataFrame()
    for aa in range(len(admin_names)):
        calib_result_filepath = os.path.join(project_path, 'simulation_output', 'seasonality_calibration', experiment_names[aa], 'CalibManager.json')

        f = open(calib_result_filepath)
        calib_dict = json.load(f)
        calib_df = pd.DataFrame.from_dict(calib_dict['final_samples'])
        calib_df['archetype'] = admin_names[aa]
        calib_df['Constant'] = constant_habitat
        # merge with existing dataframe
        seasonality_fits = pd.concat([seasonality_fits, calib_df])

    larval_habitat_dir = os.path.join(project_path, 'simulation_inputs', 'larval_habitats')
    if not os.path.exists(larval_habitat_dir):
        os.mkdir(larval_habitat_dir)
    seasonality_fits.to_csv(os.path.join(larval_habitat_dir, 'monthly_habitats_1.csv'),
                            index=False)


# save the best-fit vector habitat parameters for each archetype across all calibration simulations
#   version using the LL_all.csv file
def save_best_seasonality_fit_LL(project_path, experiment_names, admin_names):
    # create data frame to store results for all admins and seasonality parameters
    seasonality_fits = pd.DataFrame()
    for aa in range(len(admin_names)):
        calib_result_filepath = os.path.join(project_path, 'simulation_output', 'seasonality_calibration',
                                             experiment_names[aa], '_plots', 'LL_all.csv')
        calib_result = pd.read_csv(calib_result_filepath)
        # use row with best match to reference dataset
        calib_df = calib_result[calib_result.total == calib_result.total.max()].reset_index().iloc[[0]]
        calib_df['archetype'] = admin_names[aa]
        calib_df['Constant'] = constant_habitat
        # merge with existing dataframe
        seasonality_fits = pd.concat([seasonality_fits, calib_df])

    larval_habitat_dir = os.path.join(project_path, 'simulation_inputs', 'larval_habitats')
    if not os.path.exists(larval_habitat_dir):
        os.mkdir(larval_habitat_dir)
    seasonality_fits.to_csv(os.path.join(larval_habitat_dir, 'monthly_habitats_1.csv'),
                            index=False)

if __name__ == "__main__":
    experiment_names = ['seasonality_calibration_NoMassITN_%s_NGA2019_v%i_round%i%s' % (admin_names[ii], version[ii], round_numbers[ii], suffix[ii]) for ii in range(len(admin_names))]
    # save_best_seasonality_fit(project_path, experiment_names, admin_names)
    save_best_seasonality_fit_LL(project_path, experiment_names, admin_names)

