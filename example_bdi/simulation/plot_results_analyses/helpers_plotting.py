import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import seaborn as sns



def load_adjusted(project_path, full_path_sdir, scenario_name, maxyear, subset='all') :

    # get admin names and population sizes
    admin_pop = pd.read_csv(os.path.join(project_path, 'admin_pop_archetype.csv'))

    # check whether annual indicators have already been calculated. If not, calculate.
    try :
        sum_df = pd.read_csv(os.path.join(full_path_sdir, 'annual_indicators_adjusted_%s.csv' % subset))
    except IOError :

        fname = os.path.join(full_path_sdir, 'malariaBurden_withAdjustments.csv')
        df = pd.read_csv(fname)
        df['date'] = pd.to_datetime(df['date'])
        df['year'] = df['date'].apply(lambda x: x.year)
        df = df[df['year'] <= maxyear]
        if 'Received_Severe_Treatment' not in df.columns.values:
            df['Received_Severe_Treatment'] = 0
        df = df.groupby(['month', 'year', 'admin_name', 'Run_Number']).agg(np.mean).reset_index()
        df = df.groupby(['year', 'admin_name', 'Run_Number']).agg({'Statistical_Population': np.mean,
                                                                'New_Clinical_Cases': np.sum,
                                                                'PfPR_MiP_adjusted': np.mean,
                                                                'mLBW_births': np.sum,
                                                                'MiP_stillbirths': np.sum,
                                                                'total_mortality_1': np.sum,
                                                                'total_mortality_2': np.sum,
                                                                'Pop_U5': np.mean,
                                                                'PfPR_U5': np.mean,
                                                                'New_clinical_cases_U5': np.sum,
                                                                'total_mortality_U5_1': np.sum,
                                                                'total_mortality_U5_2': np.sum,
                                                                # 'Pop_U1': np.mean,
                                                                # 'PfPR_U1': np.mean,
                                                                # 'New_clinical_cases_U1': np.sum,
                                                                # 'total_mortality_U1_1': np.sum,
                                                                # 'total_mortality_U1_2': np.sum
                                                                }).reset_index()
        data_channels = ['Statistical_Population', 'New_Clinical_Cases', 'PfPR_MiP_adjusted', 'mLBW_births',
                         'MiP_stillbirths',
                         'total_mortality_1', 'total_mortality_2', 'Pop_U5', 'PfPR_U5', 'New_clinical_cases_U5',
                         'total_mortality_U5_1', 'total_mortality_U5_2']
                         # , 'Pop_U1', 'PfPR_U1', 'New_clinical_cases_U1',
                         #  'total_mortality_U1_1', 'total_mortality_U1_2']
        df = df.groupby(['year', 'admin_name'])[data_channels].agg(np.mean).reset_index()
        df = pd.merge(left=df, right=admin_pop, on=['admin_name'])
        # all age metrics - rescaled to full population
        df['positives_all_ages'] = df['PfPR_MiP_adjusted'] * df['pop_size']
        df['cases_all_ages'] = df['New_Clinical_Cases'] * df['pop_size'] / df['Statistical_Population']
        df['deaths_1_all_ages'] = df['total_mortality_1'] * df['pop_size'] / df['Statistical_Population']
        df['deaths_2_all_ages'] = df['total_mortality_2'] * df['pop_size'] / df['Statistical_Population']
        df['num_mLBW'] = df['mLBW_births'] * df['pop_size'] / df['Statistical_Population']
        df['num_mStillbirths'] = df['MiP_stillbirths'] * df['pop_size'] / df['Statistical_Population']
        # U5 metrics - rescaled to full population
        df['pop_size_U5'] = df['pop_size'] * (df['Pop_U5'] / df[
            'Statistical_Population'])  # assumes fraction of individual U5 in simulation is same as fraction in full population
        df['positives_U5'] = df['PfPR_U5'] * df['pop_size_U5']
        df['cases_U5'] = df['New_clinical_cases_U5'] * df['pop_size_U5'] / df['Pop_U5']
        df['deaths_1_U5'] = df['total_mortality_U5_1'] * df['pop_size_U5'] / df['Pop_U5']
        df['deaths_2_U5'] = df['total_mortality_U5_2'] * df['pop_size_U5'] / df['Pop_U5']
        # # U1 metrics - rescaled to full population
        # df['pop_size_U1'] = df['pop_size'] * (df['Pop_U1'] / df[
        #     'Statistical_Population'])  # assumes fraction of individual U1 in simulation is same as fraction in full population
        # df['positives_U1'] = df['PfPR_U1'] * df['pop_size_U1']
        # df['cases_U1'] = df['New_clinical_cases_U1'] * df['pop_size_U1'] / df['Pop_U1']
        # df['deaths_1_U1'] = df['total_mortality_U1_1'] * df['pop_size_U1'] / df['Pop_U1']
        # df['deaths_2_U1'] = df['total_mortality_U1_2'] * df['pop_size_U1'] / df['Pop_U1']

        subset_admin_names = subset_ds(project_path, scenario_name, subset_type=subset)
        df = df[df['admin_name'].isin(subset_admin_names)]

        sum_df = df.groupby('year')[
            ['Statistical_Population', 'cases_all_ages', 'positives_all_ages', 'deaths_1_all_ages', 'deaths_2_all_ages',
             'num_mLBW', 'num_mStillbirths', 'Pop_U5', 'cases_U5', 'positives_U5', 'deaths_1_U5', 'deaths_2_U5',
             'pop_size', 'pop_size_U5']].agg(np.sum).reset_index()
             #, 'Pop_U1', 'cases_U1', 'positives_U1', 'deaths_1_U1', 'deaths_2_U1',
             # 'pop_size_U1']].agg(np.sum).reset_index()
        sum_df['PfPR_all_ages'] = sum_df['positives_all_ages'] / sum_df['pop_size']
        sum_df['incidence_all_ages'] = sum_df['cases_all_ages'] / sum_df['pop_size'] * 1000
        sum_df['death_rate_1_all_ages'] = sum_df['deaths_1_all_ages'] / sum_df['pop_size'] * 1000
        sum_df['death_rate_2_all_ages'] = sum_df['deaths_2_all_ages'] / sum_df['pop_size'] * 1000
        sum_df['death_rate_mean_all_ages'] = (sum_df['death_rate_1_all_ages'] + sum_df['death_rate_2_all_ages']) / 2
        sum_df['PfPR_U5'] = sum_df['positives_U5'] / sum_df['pop_size_U5']
        sum_df['incidence_U5'] = sum_df['cases_U5'] / sum_df['pop_size_U5'] * 1000
        sum_df['death_rate_1_U5'] = sum_df['deaths_1_U5'] / sum_df['pop_size_U5'] * 1000
        sum_df['death_rate_2_U5'] = sum_df['deaths_2_U5'] / sum_df['pop_size_U5'] * 1000
        sum_df['death_rate_mean_U5'] = (sum_df['death_rate_1_U5'] + sum_df['death_rate_2_U5']) / 2
        # sum_df['PfPR_U1'] = sum_df['positives_U1'] / sum_df['pop_size_U1']
        # sum_df['incidence_U1'] = sum_df['cases_U1'] / sum_df['pop_size_U1'] * 1000
        # sum_df['death_rate_1_U1'] = sum_df['deaths_1_U1'] / sum_df['pop_size_U1'] * 1000
        # sum_df['death_rate_2_U1'] = sum_df['deaths_2_U1'] / sum_df['pop_size_U1'] * 1000
        # sum_df['death_rate_mean_U1'] = (sum_df['death_rate_1_U1'] + sum_df['death_rate_2_U1']) / 2
        sum_df['num_mStillbirths'] = sum_df['num_mStillbirths'] / 1000  # thousands of stillbirths
        sum_df['num_mLBW'] = sum_df['num_mLBW'] / 1000  # thousands of mLBWs

        sum_df.to_csv(os.path.join(full_path_sdir, 'annual_indicators_adjusted_%s.csv' % subset), index=False)
    return sum_df




def load_adjusted_extremes(project_path, full_path_sdir, scenario_name, maxyear, subset='all') :

    # get admin names and population sizes
    admin_pop = pd.read_csv(os.path.join(project_path, 'admin_pop_archetype.csv'))

    # check whether annual indicators have already been calculated. If not, calculate.
    try :
        sum_min_df = pd.read_csv(os.path.join(full_path_sdir, 'annual_indicators_min_adjusted_%s.csv' % subset))
        sum_max_df = pd.read_csv(os.path.join(full_path_sdir, 'annual_indicators_max_adjusted_%s.csv' % subset))
        df_sums = [sum_min_df, sum_max_df]
    except IOError:

        fname = os.path.join(full_path_sdir, 'malariaBurden_withAdjustments.csv')
        df = pd.read_csv(fname)
        df['date'] = pd.to_datetime(df['date'])
        df['year'] = df['date'].apply(lambda x: x.year)
        df = df[df['year'] <= maxyear]
        if 'Received_Severe_Treatment' not in df.columns.values:
            df['Received_Severe_Treatment'] = 0
        df = df.groupby(['month', 'year', 'admin_name', 'Run_Number']).agg(np.mean).reset_index()
        df = df.groupby(['year', 'admin_name', 'Run_Number']).agg({'Statistical_Population': np.mean,
                                                                'New_Clinical_Cases': np.sum,
                                                                'PfPR_MiP_adjusted': np.mean,
                                                                'mLBW_births': np.sum,
                                                                'MiP_stillbirths': np.sum,
                                                                'total_mortality_1': np.sum,
                                                                'total_mortality_2': np.sum,
                                                                'Pop_U5': np.mean,
                                                                'PfPR_U5': np.mean,
                                                                'New_clinical_cases_U5': np.sum,
                                                                'total_mortality_U5_1': np.sum,
                                                                'total_mortality_U5_2': np.sum,
                                                                # 'Pop_U1': np.mean,
                                                                # 'PfPR_U1': np.mean,
                                                                # 'New_clinical_cases_U1': np.sum,
                                                                # 'total_mortality_U1_1': np.sum,
                                                                # 'total_mortality_U1_2': np.sum
                                                                }).reset_index()


        df = pd.merge(left=df, right=admin_pop, on=['admin_name'])

        # all age metrics - rescaled to full population
        df['positives_all_ages'] = df['PfPR_MiP_adjusted'] * df['pop_size']
        df['cases_all_ages'] = df['New_Clinical_Cases'] * df['pop_size'] / df['Statistical_Population']
        df['deaths_1_all_ages'] = df['total_mortality_1'] * df['pop_size'] / df['Statistical_Population']
        df['deaths_2_all_ages'] = df['total_mortality_2'] * df['pop_size'] / df['Statistical_Population']
        df['num_mLBW'] = df['mLBW_births'] * df['pop_size'] / df['Statistical_Population']
        df['num_mStillbirths'] = df['MiP_stillbirths'] * df['pop_size'] / df['Statistical_Population']
        # U5 metrics - rescaled to full population
        df['pop_size_U5'] = df['pop_size'] * (df['Pop_U5'] / df[
            'Statistical_Population'])  # assumes fraction of individual U5 in simulation is same as fraction in full population
        df['positives_U5'] = df['PfPR_U5'] * df['pop_size_U5']
        df['cases_U5'] = df['New_clinical_cases_U5'] * df['pop_size_U5'] / df['Pop_U5']
        df['deaths_1_U5'] = df['total_mortality_U5_1'] * df['pop_size_U5'] / df['Pop_U5']
        df['deaths_2_U5'] = df['total_mortality_U5_2'] * df['pop_size_U5'] / df['Pop_U5']
        # # U1 metrics - rescaled to full population
        # df['pop_size_U1'] = df['pop_size'] * (df['Pop_U1'] / df[
        #     'Statistical_Population'])  # assumes fraction of individual U1 in simulation is same as fraction in full population
        # df['positives_U1'] = df['PfPR_U1'] * df['pop_size_U1']
        # df['cases_U1'] = df['New_clinical_cases_U1'] * df['pop_size_U1'] / df['Pop_U1']
        # df['deaths_1_U1'] = df['total_mortality_U1_1'] * df['pop_size_U1'] / df['Pop_U1']
        # df['deaths_2_U1'] = df['total_mortality_U1_2'] * df['pop_size_U1'] / df['Pop_U1']

        data_channels = ['pop_size', 'pop_size_U5', 'positives_all_ages', 'cases_all_ages', 'deaths_1_all_ages', 'deaths_2_all_ages',
                         'positives_U5', 'cases_U5', 'deaths_1_U5', 'deaths_2_U5',
                         'num_mStillbirths', 'num_mLBW']
                         # , 'Pop_U1', 'PfPR_U1', 'New_clinical_cases_U1',
                         #  'total_mortality_U1_1', 'total_mortality_U1_2']

        subset_admin_names = subset_ds(project_path, scenario_name, subset_type=subset)
        df = df[df['admin_name'].isin(subset_admin_names)]

        # get sums over all included admins (keeping runs separate)
        sum_df = df.groupby(['year', 'Run_Number'])[data_channels].agg(np.sum).reset_index()
        # get rates over all included admins (keeping runs separate)
        sum_df['PfPR_all_ages'] = sum_df['positives_all_ages'] / sum_df['pop_size']
        sum_df['incidence_all_ages'] = sum_df['cases_all_ages'] / sum_df['pop_size'] * 1000
        sum_df['death_rate_1_all_ages'] = sum_df['deaths_1_all_ages'] / sum_df['pop_size'] * 1000
        sum_df['death_rate_2_all_ages'] = sum_df['deaths_2_all_ages'] / sum_df['pop_size'] * 1000
        sum_df['death_rate_mean_all_ages'] = (sum_df['death_rate_1_all_ages'] + sum_df['death_rate_2_all_ages']) / 2
        sum_df['PfPR_U5'] = sum_df['positives_U5'] / sum_df['pop_size_U5']
        sum_df['incidence_U5'] = sum_df['cases_U5'] / sum_df['pop_size_U5'] * 1000
        sum_df['death_rate_1_U5'] = sum_df['deaths_1_U5'] / sum_df['pop_size_U5'] * 1000
        sum_df['death_rate_2_U5'] = sum_df['deaths_2_U5'] / sum_df['pop_size_U5'] * 1000
        sum_df['death_rate_mean_U5'] = (sum_df['death_rate_1_U5'] + sum_df['death_rate_2_U5']) / 2
        # sum_df['PfPR_U1'] = sum_df['positives_U1'] / sum_df['pop_size_U1']
        # sum_df['incidence_U1'] = sum_df['cases_U1'] / sum_df['pop_size_U1'] * 1000
        # sum_df['death_rate_1_U1'] = sum_df['deaths_1_U1'] / sum_df['pop_size_U1'] * 1000
        # sum_df['death_rate_2_U1'] = sum_df['deaths_2_U1'] / sum_df['pop_size_U1'] * 1000
        # sum_df['death_rate_mean_U1'] = (sum_df['death_rate_1_U1'] + sum_df['death_rate_2_U1']) / 2
        sum_df['num_mStillbirths'] = sum_df['num_mStillbirths'] / 1000  # thousands of stillbirths
        sum_df['num_mLBW'] = sum_df['num_mLBW'] / 1000  # thousands of mLBWs

        # get minimum and maximum national (or subset) burden values among all seeds
        sum_min_df = sum_df.groupby(['year']).agg(np.min).reset_index()
        sum_max_df = sum_df.groupby(['year']).agg(np.max).reset_index()

        sum_min_df.to_csv(os.path.join(full_path_sdir, 'annual_indicators_min_adjusted_%s.csv' % subset), index=False)
        sum_max_df.to_csv(os.path.join(full_path_sdir, 'annual_indicators_max_adjusted_%s.csv' % subset), index=False)

        df_sums = [sum_min_df, sum_max_df]
    return df_sums






def subset_ds(project_path, scenario_name, subset_type='all'):

    # get admin names and population sizes
    admin_pop = pd.read_csv(os.path.join(project_path, 'admin_pop_archetype.csv'))

    if subset_type=='all':
        ds = admin_pop['admin_name'].unique()
    else:
        scenario_fname = os.path.join(project_path, 'simulation_inputs', '_intervention_file_references',
                                      'Interventions_for_projections.csv')
        scen_df = pd.read_csv(scenario_fname)
        scen_index = 0

        if subset_type == 'smc':
            smc_df = pd.read_csv(
                os.path.join(project_path, 'simulation_inputs', '%s.csv' % scen_df.at[scen_index, 'SMC_filename']))
            df = pd.read_csv(smc_df)
            ds = df['admin_name'].unique()
        # elif subset_type == 'ipti' :
        #     ipti_fname = os.path.join(project_path, 'Scenarios', '210315_GIN_scenarios.xlsx')
        #     df = pd.read_excel(ipti_fname, sheet_name='210315')
        #     ds = df[df['ipti_nsp'] == 'TPIn']['adm2'].unique()
        # elif subset_type == 'pbo':
        #     itn_dir = os.path.join(project_path, 'simulation_inputs', 'DS_inputs_files',
        #                            'projection_csvs', 'projection', 'ITN')
        #     itn_fname = os.path.join(itn_dir, '210302_itn_scenario1a.csv')
        #     df = pd.read_csv(itn_fname)
        #     ds = df[df['llins1'] == 'PBO']['NAME_2'].unique()
        else:
            ds = []
    return ds


def load_previous_trend(project_path, previous_trend_dir, past_scenario_name, first_projection_year, subset='all'):
    try:
        df_past = pd.read_csv(os.path.join(previous_trend_dir, 'annual_indicators_adjusted_%s.csv' % subset))
    except IOError :
        df_past = load_adjusted(project_path=project_path, full_path_sdir=previous_trend_dir, scenario_name=past_scenario_name, maxyear=(first_projection_year-1), subset=subset)
    for age_group in ['all_ages', 'U5'] :
        df_past['death_rate_mean_%s' % age_group] = (df_past['death_rate_1_%s' % age_group] + df_past['death_rate_2_%s' % age_group]) / 2
    return df_past

def load_previous_trend_extremes(project_path, previous_trend_dir, past_scenario_name, first_projection_year, subset='all'):
    try:
        sum_min_df = pd.read_csv(os.path.join(previous_trend_dir, 'annual_indicators_min_adjusted_%s.csv' % subset))
        sum_max_df = pd.read_csv(os.path.join(previous_trend_dir, 'annual_indicators_max_adjusted_%s.csv' % subset))
        df_sums = [sum_min_df, sum_max_df]
    except IOError:
        df_sums = load_adjusted_extremes(project_path=project_path, full_path_sdir=previous_trend_dir, scenario_name=past_scenario_name, maxyear=(first_projection_year-1), subset=subset)
    for ii in range(len(df_sums)):
        for age_group in ['all_ages', 'U5']:
            df_sums[ii]['death_rate_mean_%s' % age_group] = (df_sums[ii]['death_rate_1_%s' % age_group] + df_sums[ii]['death_rate_2_%s' % age_group]) / 2
    return df_sums


def plot_trend_and_bars_separate_pdfs(project_path, scenario_dir, plot_scenarios, baseline_scenario, palette_by_scenario, subset='all', first_projection_year=2021, last_plot_year=2030, baseline_year=2021, comparison_year=2024, previous_trend_dir=None, past_scenario_name=None, first_past_year=2010) :

    if (previous_trend_dir is not None) & (past_scenario_name is not None):
        df_past = load_previous_trend(project_path=project_path, subset=subset, previous_trend_dir=previous_trend_dir, past_scenario_name=past_scenario_name, first_projection_year=first_projection_year)
        df_past = df_past[df_past['year'] >= (1+first_past_year)]
    else:
        df_past = pd.DataFrame()

    # plot_channels = ['PfPR_U5', 'incidence_U5', 'death_rate_mean_U5', 'PfPR_all_ages', 'incidence_all_ages',
    #                  'death_rate_mean_all_ages']
    # plot_y_labels = ['U5 PfPR', 'U5 incidence', 'U5 death rate', 'all age PfPR',
    #                  'all age incidence', 'all age death rate']
    plot_channels = ['PfPR_U5', 'incidence_U5', 'death_rate_mean_U5', 'num_mStillbirths', 'PfPR_all_ages',
                     'incidence_all_ages', 'death_rate_mean_all_ages', 'num_mLBW']#,
                     # 'PfPR_U1', 'incidence_U1', 'death_rate_mean_U1']
    plot_y_labels = ['U5 PfPR', 'U5 incidence', 'U5 death rate', 'stillbirths (1000s)', 'all age PfPR',
                     'all age incidence', 'all age death rate', 'mLBW (1000s)']#,
                     # 'U1 PfPR', 'U1 incidence', 'U1 death rate']

    sns.set_style('whitegrid', {'axes.linewidth': 0.5})

    figs = [plt.figure(plot_y_labels[y], figsize=(8,2.55)) for y in range(len(plot_channels))]
    axes = [[fig.add_subplot(1,2,x+1) for x in range(2)] for fig in figs]

    comp_year_range = range(first_projection_year, (last_plot_year+1))

    comparison_table = { x : [] for x in plot_y_labels}
    comparison_table['scenario'] = plot_scenarios

    for si, sdir in enumerate(plot_scenarios) :

        print(sdir)
        scen_name = sdir.split(' ')[-1]
        plot_label = scen_name
        sum_df = load_adjusted(project_path=project_path, full_path_sdir=os.path.join(scenario_dir, sdir), scenario_name=plot_scenarios[si], maxyear=last_plot_year, subset=subset)

        for ai, channel in enumerate(plot_channels):
            if si == 1:
                axes[ai][0].plot(df_past['year'], df_past[channel], color='#58595B')
            plot_df = pd.concat([df_past[df_past['year'] == (first_projection_year-1)], sum_df], sort=True)
            plot_df = plot_df.sort_values(by='year')
            axes[ai][0].plot(plot_df['year'], plot_df[channel], color=palette_by_scenario[scen_name], label=plot_label)
            axes[ai][0].set_ylabel(plot_y_labels[ai])
            axes[ai][0].set_xlim((first_past_year+1), last_plot_year)
            # axes[ai][0].set_ylim(0,ymax[channel])
            axes[ai][0].set_xticks(range((first_past_year+1), last_plot_year, 5))  # was (2020, 2026, 2)
            if si == len(plot_scenarios)-1:
                # axes[ai][0].legend(loc='upper right', bbox_to_anchor=(1.4, 1))
                axes[ai][0].legend(loc='upper right', bbox_to_anchor=(1.5, 1))

        sum_df = sum_df.set_index('year')

        baseline = dict.fromkeys(plot_channels, -9)
        for ai, channel in enumerate(plot_channels):
            for comp_year in comp_year_range:
                if (comp_year == baseline_year) and (baseline_scenario in sdir):
                    baseline[channel] = sum_df.at[comp_year, channel]
                burden_red = (sum_df.at[comp_year, channel] - baseline[channel])/baseline[channel]
                if comp_year == comparison_year :
                    axes[ai][1].bar([si], [burden_red], color=palette_by_scenario[scen_name], label=plot_label)
                    comparison_table[plot_y_labels[ai]].append(burden_red)
            axes[ai][1].set_ylim(-1, 0.2)
            axes[ai][1].set_xticklabels([])
            axes[ai][1].set_ylabel('(change %d - 2021 BAU)/(2021 BAU)' % comparison_year)

    # plt.tight_layout(pad=3.0)
    if not os.path.exists(os.path.join(scenario_dir, '_plots')):
        os.makedirs(os.path.join(scenario_dir, '_plots'))
    for ff, fig in enumerate(figs):
        fig.subplots_adjust(wspace=0.7, right=0.97)
        fig.savefig(os.path.join(scenario_dir, '_plots', '%s_%s.png' % (plot_channels[ff], subset)), dpi=200)
        fig.savefig(os.path.join(scenario_dir, '_plots', '%s_%s.pdf' % (plot_channels[ff], subset)), format='PDF')
        plt.close(fig)

    comparison_df = pd.DataFrame(comparison_table)
    comparison_df = comparison_df[['scenario'] + plot_y_labels]
    comparison_df.to_csv(os.path.join(scenario_dir, 'compare_to_%s_%i_%s.csv' % (baseline_scenario, baseline_year, subset)), index=False)





def plot_trend_with_min_max_pdfs(project_path, scenario_dir, plot_scenarios, scenario_endings, palette_by_scenario, subset='all', first_projection_year=2021, last_plot_year=2030, previous_trend_dir=None, past_scenario_name=None, first_past_year=2010) :

    if (previous_trend_dir is not None) & (past_scenario_name is not None):
        df_past = load_previous_trend(project_path=project_path, subset=subset, previous_trend_dir=previous_trend_dir, past_scenario_name=past_scenario_name, first_projection_year=first_projection_year)
        df_past = df_past[df_past['year'] >= (1+first_past_year)]
        df_past_min_max = load_previous_trend_extremes(project_path=project_path, subset=subset, previous_trend_dir=previous_trend_dir, past_scenario_name=past_scenario_name, first_projection_year=first_projection_year)
        df_past_min = df_past_min_max[0]
        df_past_min = df_past_min[df_past_min['year'] >= (1+first_past_year)]
        df_past_max = df_past_min_max[1]
        df_past_max = df_past_max[df_past_max['year'] >= (1+first_past_year)]
    else:
        df_past = pd.DataFrame()
        df_past_min = pd.DataFrame()
        df_past_max = pd.DataFrame()

    # plot_channels = ['PfPR_U5', 'incidence_U5', 'death_rate_mean_U5', 'PfPR_all_ages', 'incidence_all_ages',
    #                  'death_rate_mean_all_ages']
    # plot_y_labels = ['U5 PfPR', 'U5 incidence', 'U5 death rate', 'all age PfPR',
    #                  'all age incidence', 'all age death rate']
    plot_channels = ['PfPR_U5', 'incidence_U5', 'death_rate_mean_U5', 'num_mStillbirths', 'PfPR_all_ages',
                     'incidence_all_ages', 'death_rate_mean_all_ages', 'num_mLBW']#,
                     # 'PfPR_U1', 'incidence_U1', 'death_rate_mean_U1']
    plot_y_labels = ['U5 PfPR', 'U5 incidence', 'U5 death rate', 'stillbirths (1000s)', 'all age PfPR',
                     'all age incidence', 'all age death rate', 'mLBW (1000s)']#,
                     # 'U1 PfPR', 'U1 incidence', 'U1 death rate']

    sns.set_style('whitegrid', {'axes.linewidth': 0.5})

    figs = [plt.figure(plot_y_labels[y], figsize=(4,2.55)) for y in range(len(plot_channels))]
    axes = [[fig.add_subplot(1,1,x+1) for x in range(1)] for fig in figs]

    comp_year_range = range(first_projection_year, (last_plot_year+1))

    for si, sdir in enumerate(plot_scenarios) :

        print(sdir)
        scen_name = sdir.split(' ')[-1]
        plot_label = scenario_endings[si]
        sum_df = load_adjusted(project_path=project_path, full_path_sdir=os.path.join(scenario_dir, sdir), scenario_name=plot_scenarios[si], maxyear=last_plot_year, subset=subset)
        sum_df_min_max = load_adjusted_extremes(project_path=project_path, full_path_sdir=os.path.join(scenario_dir, sdir), scenario_name=plot_scenarios[si], maxyear=last_plot_year, subset=subset)
        sum_df_min = sum_df_min_max[0]
        sum_df_max = sum_df_min_max[1]

        for ai, channel in enumerate(plot_channels):
            if si == 1:
                axes[ai][0].plot(df_past['year'], df_past[channel], color='#58595B')
                axes[ai][0].fill_between(df_past_min['year'], df_past_min[channel], df_past_max[channel], facecolor='#58595B', alpha=0.25)
            plot_df = pd.concat([df_past[df_past['year'] == (first_projection_year-1)], sum_df], sort=True)
            plot_df_min = pd.concat([df_past_min[df_past_min['year'] == (first_projection_year-1)], sum_df_min], sort=True)
            plot_df_max = pd.concat([df_past_max[df_past_max['year'] == (first_projection_year-1)], sum_df_max], sort=True)
            plot_df = plot_df.sort_values(by='year')
            plot_df_min = plot_df_min.sort_values(by='year')
            plot_df_max = plot_df_max.sort_values(by='year')
            axes[ai][0].plot(plot_df['year'], plot_df[channel], color=palette_by_scenario[scen_name], label=plot_label)
            axes[ai][0].fill_between(plot_df_min['year'], plot_df_min[channel], plot_df_max[channel], facecolor=palette_by_scenario[scen_name], alpha=0.25)
            axes[ai][0].set_ylabel(plot_y_labels[ai])
            axes[ai][0].set_xlim((first_past_year+1), last_plot_year)
            # axes[ai][0].set_ylim(0,ymax[channel])
            axes[ai][0].set_xticks(range((first_past_year+1), last_plot_year, 5))  # was (2020, 2026, 2)
            # if si == len(plot_scenarios)-1:
            #     # axes[ai][0].legend(loc='upper right', bbox_to_anchor=(1.4, 1))
            #     axes[ai][0].legend(loc='upper left')#, bbox_to_anchor=(1, 1))

    # plt.tight_layout(pad=3.0)
    if not os.path.exists(os.path.join(scenario_dir, '_plots')):
        os.makedirs(os.path.join(scenario_dir, '_plots'))
    for ff, fig in enumerate(figs):
        fig.subplots_adjust(right=1, left=0.2)
        fig.savefig(os.path.join(scenario_dir, '_plots', 'trend_with_min_max_%s_%s.png' % (plot_channels[ff], subset)), dpi=200)
        fig.savefig(os.path.join(scenario_dir, '_plots', 'trend_with_min_max_%s_%s.pdf' % (plot_channels[ff], subset)), format='PDF')
        plt.close(fig)







def plot_trend_with_min_max_one_pdf(project_path, scenario_dir, plot_scenarios, scenario_endings, palette_by_scenario, subset='all', first_projection_year=2021, last_plot_year=2030, previous_trend_dir=None, past_scenario_name=None, first_past_year=2010) :

    if (previous_trend_dir is not None) & (past_scenario_name is not None):
        df_past = load_previous_trend(project_path=project_path, subset=subset, previous_trend_dir=previous_trend_dir, past_scenario_name=past_scenario_name, first_projection_year=first_projection_year)
        df_past = df_past[df_past['year'] >= (1+first_past_year)]
        df_past_min_max = load_previous_trend_extremes(project_path=project_path, subset=subset, previous_trend_dir=previous_trend_dir, past_scenario_name=past_scenario_name, first_projection_year=first_projection_year)
        df_past_min = df_past_min_max[0]
        df_past_min = df_past_min[df_past_min['year'] >= (1+first_past_year)]
        df_past_max = df_past_min_max[1]
        df_past_max = df_past_max[df_past_max['year'] >= (1+first_past_year)]
    else:
        df_past = pd.DataFrame()
        df_past_min = pd.DataFrame()
        df_past_max = pd.DataFrame()

    # plot_channels = ['PfPR_U5', 'incidence_U5', 'death_rate_mean_U5', 'PfPR_all_ages', 'incidence_all_ages',
    #                  'death_rate_mean_all_ages']
    # plot_y_labels = ['U5 PfPR', 'U5 incidence', 'U5 death rate', 'all age PfPR',
    #                  'all age incidence', 'all age death rate']
    plot_channels = ['PfPR_U5', 'incidence_U5', 'death_rate_mean_U5', 'num_mStillbirths', 'PfPR_all_ages',
                     'incidence_all_ages', 'death_rate_mean_all_ages', 'num_mLBW']#,
                     # 'PfPR_U1', 'incidence_U1', 'death_rate_mean_U1']
    plot_y_labels = ['U5 PfPR', 'U5 incidence', 'U5 death rate', 'stillbirths (1000s)', 'all age PfPR',
                     'all age incidence', 'all age death rate', 'mLBW (1000s)']#,
                     # 'U1 PfPR', 'U1 incidence', 'U1 death rate']

    sns.set_style('whitegrid', {'axes.linewidth': 0.5})

    figs = [plt.figure('', figsize=(4*np.ceil(len(plot_channels)/2), 2*2.55))]
    axes = [[fig.add_subplot(2,np.ceil(len(plot_channels)/2),x+1) for x in range(len(plot_channels))] for fig in figs]

    comp_year_range = range(first_projection_year, (last_plot_year+1))

    for si, sdir in enumerate(plot_scenarios) :

        print(sdir)
        scen_name = sdir.split(' ')[-1]
        plot_label = scenario_endings[si]
        sum_df = load_adjusted(project_path=project_path, full_path_sdir=os.path.join(scenario_dir, sdir), scenario_name=plot_scenarios[si], maxyear=last_plot_year, subset=subset)
        sum_df_min_max = load_adjusted_extremes(project_path=project_path, full_path_sdir=os.path.join(scenario_dir, sdir), scenario_name=plot_scenarios[si], maxyear=last_plot_year, subset=subset)
        sum_df_min = sum_df_min_max[0]
        sum_df_max = sum_df_min_max[1]

        for ai, channel in enumerate(plot_channels):
            if si == 1:
                axes[0][ai].plot(df_past['year'], df_past[channel], color='#58595B')
                axes[0][ai].fill_between(df_past_min['year'], df_past_min[channel], df_past_max[channel], facecolor='#58595B', alpha=0.1)
            plot_df = pd.concat([df_past[df_past['year'] == (first_projection_year-1)], sum_df], sort=True)
            plot_df_min = pd.concat([df_past_min[df_past_min['year'] == (first_projection_year-1)], sum_df_min], sort=True)
            plot_df_max = pd.concat([df_past_max[df_past_max['year'] == (first_projection_year-1)], sum_df_max], sort=True)
            plot_df = plot_df.sort_values(by='year')
            plot_df_min = plot_df_min.sort_values(by='year')
            plot_df_max = plot_df_max.sort_values(by='year')
            axes[0][ai].plot(plot_df['year'], plot_df[channel], color=palette_by_scenario[scen_name], label=plot_label)
            axes[0][ai].fill_between(plot_df_min['year'], plot_df_min[channel], plot_df_max[channel], facecolor=palette_by_scenario[scen_name], alpha=0.1)
            axes[0][ai].set_ylabel(plot_y_labels[ai])
            axes[0][ai].set_xlim((first_past_year+1), last_plot_year)
            # axes[ai][0].set_ylim(0,ymax[channel])
            axes[0][ai].set_xticks(range((first_past_year+1), last_plot_year, 5))  # was (2020, 2026, 2)
            # if si == len(plot_scenarios)-1:
            #     # axes[ai][0].legend(loc='upper right', bbox_to_anchor=(1.4, 1))
            #     axes[ai][0].legend(loc='upper left')#, bbox_to_anchor=(1, 1))

    plt.tight_layout(pad=3.0)
    if not os.path.exists(os.path.join(scenario_dir, '_plots')):
        os.makedirs(os.path.join(scenario_dir, '_plots'))
    for ff, fig in enumerate(figs):
        fig.subplots_adjust(right=1, left=0.2)
        fig.savefig(os.path.join(scenario_dir, '_plots', 'trend_with_min_max_%s.png' % (subset)), dpi=200)
        fig.savefig(os.path.join(scenario_dir, '_plots', 'trend_with_min_max_%s.pdf' % (subset)), format='PDF')
        plt.close(fig)






def plot_future_trend_only_separate_pdfs(project_path, scenario_dir, plot_scenarios, palette_by_scenario, subset='all', first_projection_year=2021, last_plot_year=2030):


    # plot_channels = ['PfPR_U5', 'incidence_U5', 'death_rate_mean_U5', 'PfPR_all_ages', 'incidence_all_ages',
    #                  'death_rate_mean_all_ages']
    # plot_y_labels = ['U5 PfPR', 'U5 incidence', 'U5 death rate', 'all age PfPR',
    #                  'all age incidence', 'all age death rate']
    plot_channels = ['PfPR_U5', 'incidence_U5', 'death_rate_mean_U5', 'num_mStillbirths', 'PfPR_all_ages',
                     'incidence_all_ages', 'death_rate_mean_all_ages', 'num_mLBW']#,
                     # 'PfPR_U1', 'incidence_U1', 'death_rate_mean_U1']
    plot_y_labels = ['U5 PfPR', 'U5 incidence', 'U5 death rate', 'stillbirths (1000s)', 'all age PfPR',
                     'all age incidence', 'all age death rate', 'mLBW (1000s)']#,
                     # 'U1 PfPR', 'U1 incidence', 'U1 death rate']

    sns.set_style('whitegrid', {'axes.linewidth': 0.5})

    figs = [plt.figure(plot_y_labels[y], figsize=(4, 2.55)) for y in range(len(plot_channels))]
    axes = [[fig.add_subplot(1,1,x+1) for x in range(1)] for fig in figs]

    for si, sdir in enumerate(plot_scenarios) :

        print(sdir)
        scen_name = sdir.split(' ')[-1]
        plot_label = scen_name
        sum_df = load_adjusted(project_path=project_path, full_path_sdir=os.path.join(scenario_dir, sdir), scenario_name=plot_scenarios[si], maxyear=last_plot_year, subset=subset)

        for ai, channel in enumerate(plot_channels):

            plot_df = sum_df
            plot_df = plot_df.sort_values(by='year')
            axes[ai][0].plot(plot_df['year'], plot_df[channel], color=palette_by_scenario[scen_name], label=plot_label)
            axes[ai][0].set_ylabel(plot_y_labels[ai])
            axes[ai][0].set_xlim((first_projection_year+1), last_plot_year)
            # axes[ai][0].set_ylim(0,ymax[channel])
            axes[ai][0].set_xticks(range((first_projection_year+1), last_plot_year, 2))  # was (2020, 2026, 2)
            # if si == len(plot_scenarios)-1:
                # # axes[ai][0].legend(loc='upper right', bbox_to_anchor=(1.4, 1))
                # axes[ai][0].legend(loc='upper right', bbox_to_anchor=(1, 1))

    # plt.tight_layout(pad=3.0)
    if not os.path.exists(os.path.join(scenario_dir, '_plots')):
        os.makedirs(os.path.join(scenario_dir, '_plots'))
    for ff, fig in enumerate(figs):
        fig.subplots_adjust(right=1, left=0.2)
        fig.savefig(os.path.join(scenario_dir, '_plots', 'future_trend_%s_%s.png' % (plot_channels[ff], subset)), dpi=200)
        fig.savefig(os.path.join(scenario_dir, '_plots', 'future_trend_%s_%s.pdf' % (plot_channels[ff], subset)), format='PDF')
        plt.close(fig)
