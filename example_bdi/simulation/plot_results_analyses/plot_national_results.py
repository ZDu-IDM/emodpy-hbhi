import pandas as pd
import numpy as np
import os
import sys
sys.path.append('../')
from simulation.load_paths import load_box_paths
import matplotlib.pyplot as plt
import matplotlib as mpl
import seaborn as sns
from simulation.plot_results_analyses.helpers_plotting import plot_trend_and_bars_separate_pdfs, plot_future_trend_only_separate_pdfs, plot_trend_with_min_max_pdfs, plot_trend_with_min_max_one_pdf

data_path, project_path = load_box_paths(country_name='Burundi')

mpl.rcParams['pdf.fonttype'] = 42

# subset of seeds to include
seed_subset = 'mortLog'  # options: 'all', 'mortLog', 'mortLogLog'  Note: need to run subset_sim_output on future and past simulations before using anything but 'all'
# assumption set used for plotting
chw_coverage = 'higher'  # options: 'higher', 'lower'
pyr_mort = 64  # options: 64, 40
# scenarios and colors
scenario_endings = ['%ipyr' % pyr_mort, 'GF_%sCHWCoverage_%ipyr' % (chw_coverage, pyr_mort), 'GF_%sCHWCoverage_PBO%ipyr' % (chw_coverage, pyr_mort), 'GF_80Coverage_%ipyr' % pyr_mort, 'GF_80CoveragePBO_%ipyr' % pyr_mort]
palette = [(0/255,120/255,0/255), (55/255,80/255,220/255), (90/255,180/255,238/255),
           (170/255,51/255,119/255), (255/255,140/255,175/255)]


plot_scenarios = ['BDI_projection_%s' % x for x in scenario_endings]
palette_by_scenario = { x : y for x, y in zip(plot_scenarios, palette[:len(plot_scenarios)])}
baseline_scenario = ''
first_projection_year = 2021
last_plot_year = 2030
baseline_year = 2021
comparison_year = 2024
past_scenario_name = 'BDI_2010_2020_allInter'
first_past_year = 2011


if seed_subset == 'all':
    scenario_dir = os.path.join(project_path, 'simulation_output', 'simulations_future')
    previous_trend_dir = os.path.join(project_path, 'simulation_output', 'simulations_to_present', past_scenario_name)
elif seed_subset == 'mortLog':
    scenario_dir = os.path.join(project_path, 'simulation_output', 'simulations_future', 'simulation_subsets', 'mortLog')
    previous_trend_dir = os.path.join(project_path, 'simulation_output', 'simulations_to_present', 'simulation_subsets', 'mortLog', past_scenario_name)
elif seed_subset == 'mortLogLog':
    scenario_dir = os.path.join(project_path, 'simulation_output', 'simulations_future', 'simulation_subsets', 'mortLogLog')
    previous_trend_dir = os.path.join(project_path, 'simulation_output', 'simulations_to_present', 'simulation_subsets', 'mortLogLog', past_scenario_name)

if __name__ == '__main__':
    # plot_trend_and_bars_separate_pdfs(project_path=project_path, scenario_dir=scenario_dir, plot_scenarios=plot_scenarios, baseline_scenario=baseline_scenario,
    #                                   palette_by_scenario=palette_by_scenario, subset='all', first_projection_year=first_projection_year,
    #                                   last_plot_year=last_plot_year, baseline_year=baseline_year, comparison_year=comparison_year,
    #                                   previous_trend_dir=previous_trend_dir, past_scenario_name=past_scenario_name, first_past_year=first_past_year)

    # plot_trend_with_min_max_pdfs(project_path=project_path, scenario_dir=scenario_dir, plot_scenarios=plot_scenarios, scenario_endings=scenario_endings,
    #                              palette_by_scenario=palette_by_scenario, subset='all', first_projection_year=first_projection_year,
    #                              last_plot_year=last_plot_year,
    #                              previous_trend_dir=previous_trend_dir, past_scenario_name=past_scenario_name, first_past_year=first_past_year)

    plot_trend_with_min_max_one_pdf(project_path=project_path, scenario_dir=scenario_dir, plot_scenarios=plot_scenarios, scenario_endings=scenario_endings,
                                    palette_by_scenario=palette_by_scenario, subset='all', first_projection_year=first_projection_year,
                                    last_plot_year=last_plot_year,
                                    previous_trend_dir=previous_trend_dir, past_scenario_name=past_scenario_name, first_past_year=first_past_year)


    # plot_future_trend_only_separate_pdfs(project_path=project_path, scenario_dir=scenario_dir, plot_scenarios=plot_scenarios, palette_by_scenario=palette_by_scenario, subset='all',
    #                                      first_projection_year=first_projection_year, last_plot_year=last_plot_year)

    # plot_trend_and_bars_separate_pdfs(plot_scenarios, subset='smc')
    # plot_trend_and_bars_separate_pdfs(plot_scenarios, subset='ipti')

    plt.show()
    # # set plot order
    # scenario_fname = os.path.join(simoutdir, 'scenario_adjustment_info.csv')
    # scen_df = pd.read_csv(scenario_fname)
    # scen_df_subset = scen_df[scen_df['plot_order'].notna()]
    # scen_df_subset_sorted = scen_df_subset.sort_values(by=['plot_order'], ascending=True)
    # scen_df_subset_sorted = scen_df_subset_sorted.reset_index()
    # scenario_dirs = list(scen_df_subset_sorted['ScenarioName'])
