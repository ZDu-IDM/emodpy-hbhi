from simtools.Analysis.SSMTAnalysis import SSMTAnalysis
from simtools.SetupParser import SetupParser
from simulation.analyzers.analyze_helpers import monthlyU1PfPRAnalyzer, monthlyU5PfPRAnalyzer, monthlyTreatedCasesAnalyzer, monthlySevereTreatedByAgeAnalyzer, MonthlyNewInfectionsAnalyzer, monthlyEventAnalyzer, MonthlyNewInfectionsAnalyzer_withU5, monthlyUsageLLIN
from simulation.analyzers.analyze_vector_numbers import VectorNumbersAnalyzer
from simtools.Utilities.COMPSUtilities import get_most_recent_experiment_id_by_name

wi_name_base = "ssmt_analyzer_"
working_dir = '.'
# start_year = 2010  # simulation starts in January of this year
# end_year = 2021  # simulation ends in December of this year
start_year = 2022  # simulation starts in January of this year
end_year = 2029  # simulation ends in December of this year


# can specify group of experiments by name...
experiment_name_stem = ''
experiment_name_tail = ''
experiment_numbers = list(range(5))
experiments = {}
# ... or can specify specific set of experiments by id
experiments = {
    # 2010-toPresent counterfactuals
    'BDI_toPresent_allInter_v4': 'a979516f-eef4-ed11-aa06-b88303911bc1',

    # future projections
    # 'BDI_BAU_v1': 'f122cb17-6ef4-ed11-aa06-b88303911bc1',
    # 'BDI_NSP_v1': '0eebde37-73f4-ed11-aa06-b88303911bc1',
    # 'BDI_NSP_noIRS': '77814e73-78f4-ed11-aa06-b88303911bc1',
    # 'BDI_pri1_v1': 'e029bed1-7df4-ed11-aa06-b88303911bc1',
    # 'BDI_pri2_v1': 'e029bed1-7df4-ed11-aa06-b88303911bc1',
    # 'BDI_pri2_noVacc_v1': '3fe16a33-88f4-ed11-aa06-b88303911bc1',
    # 'BDI_pri2_noPMC_v1': '161e745a-8df4-ed11-aa06-b88303911bc1',
}



itn_comparison_flag = False
climate_only_flag = False

if __name__ == "__main__":

    SetupParser.default_block = 'HPC'
    SetupParser.init()

    if not bool(experiments):
        for ee in range(len(experiment_numbers)):
            cur_exp_name = '%s%i%s' % (experiment_name_stem, experiment_numbers[ee], experiment_name_tail)
            cur_exp_id = get_most_recent_experiment_id_by_name(cur_exp_name)
            experiments[cur_exp_name] = str(cur_exp_id)

    if end_year > 2022:
        analyzers = [
                     monthlyU1PfPRAnalyzer,
                     monthlyU5PfPRAnalyzer,
                     monthlyTreatedCasesAnalyzer,
                     monthlySevereTreatedByAgeAnalyzer,
                     monthlyUsageLLIN,
                     monthlyEventAnalyzer,
                     MonthlyNewInfectionsAnalyzer,
                     MonthlyNewInfectionsAnalyzer_withU5,
                     VectorNumbersAnalyzer
        ]
    else:
        analyzers = [
            monthlyU5PfPRAnalyzer,
            monthlyTreatedCasesAnalyzer,
            monthlySevereTreatedByAgeAnalyzer,
            monthlyUsageLLIN,
            monthlyEventAnalyzer,
            MonthlyNewInfectionsAnalyzer,
            MonthlyNewInfectionsAnalyzer_withU5,
        ]

    for expt_name, exp_id in experiments.items():
        wi_name = '%s_%s' % (wi_name_base, expt_name)

        if itn_comparison_flag or climate_only_flag:
            sweep_variables = ["Run_Number", "admin_name", "Habitat_Multiplier"]
        else:
            sweep_variables = ["Run_Number", "admin_name"]

        args_each = {'expt_name': expt_name,
                     'sweep_variables': sweep_variables,
                     'working_dir': working_dir,
                     'start_year': start_year,
                     'end_year': end_year}
        args_new_infect = {'expt_name': expt_name,
                           'sweep_variables': sweep_variables,
                           'working_dir': working_dir,
                           'start_year': start_year,
                           'end_year': end_year,
                           'input_filename_base': 'MalariaSummaryReport_Monthly',
                           'output_filename': 'newInfections_PfPR_cases_monthly_byAgeGroup.csv'}
        args_new_infect_withU5 = {'expt_name': expt_name,
                                  'sweep_variables': sweep_variables,
                                  'working_dir': working_dir,
                                  'start_year': start_year,
                                  'end_year': end_year,
                                  'input_filename_base': 'MalariaSummaryReport_Monthly',
                                  'output_filename': 'newInfections_PfPR_cases_monthly_byAgeGroup_withU5.csv'}
        args_no_u1 = {'expt_name': expt_name,
                     'sweep_variables': sweep_variables,
                     'working_dir': working_dir,
                     'start_year': start_year,
                     'end_year': end_year,
                     'agebins': [5, 200]}
        if itn_comparison_flag:
            args_treat_case = {'expt_name': expt_name,
                               'channels': [],
                               'sweep_variables': sweep_variables,
                               'working_dir': working_dir,
                               'start_year': start_year,
                               'end_year': end_year}
        elif 'no_IRS_SMC_ITN_CM' in expt_name:
            args_treat_case = {'expt_name': expt_name,
                               'channels': ['Received_NMF_Treatment'],
                               'sweep_variables': sweep_variables,
                               'working_dir': working_dir,
                               'start_year': start_year,
                               'end_year': end_year}
        else:
            args_treat_case = args_each
        if end_year > 2022:
            analysis = SSMTAnalysis(experiment_ids=[exp_id],
                                    analyzers=analyzers,
                                    analyzers_args=[
                                                    args_each,
                                                    args_each,
                                                    args_treat_case,
                                                    args_each,
                                                    args_each,
                                                    args_each,
                                                    args_new_infect,
                                                    args_new_infect_withU5,
                                                    args_each
                                                    ],
                                    analysis_name=wi_name)
        else:
            analysis = SSMTAnalysis(experiment_ids=[exp_id],
                                    analyzers=analyzers,
                                    analyzers_args=[
                                        args_each,
                                        args_treat_case,
                                        args_no_u1,
                                        args_each,
                                        args_each,
                                        args_new_infect,
                                        args_new_infect_withU5,
                                    ],
                                    analysis_name=wi_name)
        analysis.analyze()
