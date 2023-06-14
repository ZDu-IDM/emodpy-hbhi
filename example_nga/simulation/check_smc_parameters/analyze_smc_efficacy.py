import pandas as pd
from simtools.Analysis.BaseAnalyzers import BaseAnalyzer
import os
import sys
sys.path.append('../')
from simtools.Analysis.AnalyzeManager import AnalyzeManager
from simtools.SetupParser import SetupParser

user_path = os.path.expanduser('~')
projectpath = os.path.join(user_path, 'Dropbox (IDM)', 'NU_collaboration', 'hbhi_nigeria', 'snt_2022', 'simulation_output', 'check_smc_efficacy')

class DailyNewInfectionsAnalyzer(BaseAnalyzer):

    def __init__(self, expt_name, sweep_variables=None, working_dir=".",
                 input_filename_base='InsetChart',
                 output_filename='dailyNewInfections.csv'):
        super(DailyNewInfectionsAnalyzer, self).__init__(working_dir=working_dir,
                                                         filenames=["output/%s.json" % input_filename_base]
                                                         )
        self.sweep_variables = sweep_variables or ["Run_Number"]
        self.expt_name = expt_name
        self.output_filename = output_filename

    def filter(self, simulation):
        return simulation.status.name == 'Succeeded'

    def select_simulation_data(self, data, simulation):

        fname = self.filenames[0]

        # population size
        pop = data[fname]['Channels']['Statistical Population']['Data']

        # new infections
        new_infect = data[fname]['Channels']['New Infections']['Data']

        # true PfPR
        true_pfpr = data[fname]['Channels']['True Prevalence']['Data']

        # microscopy PfPR
        micros_pfpr = data[fname]['Channels']['Blood Smear Parasite Prevalence']['Data']


        # clinical cases
        new_clinical_cases = data[fname]['Channels']['New Clinical Cases']['Data']

        adf = pd.DataFrame({'Day': list(range(len(pop))),
                           'Pop': pop,
                            'True_Prevalence': true_pfpr,
                            'Micros_Prevalence': micros_pfpr,
                            'New_Infections': new_infect,
                            'New_Clinical_Cases': new_clinical_cases
                            })
        # adf = pd.DataFrame()
        # adf = pd.concat([adf, simdata])

        for sweep_var in self.sweep_variables:
            if sweep_var in simulation.tags.keys():
                adf[sweep_var] = simulation.tags[sweep_var]
        return adf

    def finalize(self, all_data):

        selected = [data for sim, data in all_data.items()]
        if len(selected) == 0:
            print("No data have been returned... Exiting...")
            return

        if not os.path.exists(os.path.join(self.working_dir, self.expt_name)):
            os.mkdir(os.path.join(self.working_dir, self.expt_name))

        adf = pd.concat(selected).reset_index(drop=True)
        adf.to_csv((os.path.join(self.working_dir, self.expt_name, self.output_filename)), index=False)


if __name__ == "__main__":


    SetupParser.default_block = 'HPC'
    SetupParser.init()

    working_dir = projectpath

    expt_ids = {
        # 'SMC_effect_treat_control_v6': 'e8e3cc7e-acac-ed11-aa02-b88303911bc1',
        'SMC_efficacy_treat_SPAQ_v6b': '78cdd5e6-5cae-ed11-aa02-b88303911bc1',
        'SMC_efficacy_treat_SP_v6b': '7acdd5e6-5cae-ed11-aa02-b88303911bc1',
        'SMC_efficacy_treat_AQ_v6b': '79cdd5e6-5cae-ed11-aa02-b88303911bc1',

        # 'SMC_efficacy_prevent_control_v5a': '1cb243b0-edab-ed11-aa02-b88303911bc1',
        # 'SMC_efficacy_prevent_SPAQ_v5j': '5c77a28e-57ae-ed11-aa02-b88303911bc1',
        'SMC_efficacy_prevent_SP_v5b': 'dcf4f3ef-5cae-ed11-aa02-b88303911bc1',
        'SMC_efficacy_prevent_AQ_v5b': 'ebf4f3ef-5cae-ed11-aa02-b88303911bc1',
        # 'SMC_efficacy_prevent_vaccSMC_v5j': '8c8b93aa-4aae-ed11-aa02-b88303911bc1',
        # 'SMC_efficacy_prevent_vaccSMC_v5i': 'd6d9f18b-46ae-ed11-aa02-b88303911bc1',

    }

    for expname, expid in expt_ids.items() :
        print('running expt %s' % expname)
        analyzers = [
            # DailyNewInfectionsAnalyzer(expt_name=expname,
            #                            sweep_variables=["Run_Number", "challenge_interval", "Sulf_C50", "Sulf_kill"],
            #                            working_dir=working_dir,
            #                            input_filename_base='InsetChart',
            #                            output_filename='dailyNewInfections.csv'),
            # DailyNewInfectionsAnalyzer(expt_name=expname,
            #                            sweep_variables=["Run_Number", "Sulf_C50", "Sulf_kill"],
            #                            working_dir=working_dir,
            #                            input_filename_base='InsetChart',
            #                            output_filename='dailyNewInfections.csv'),
            DailyNewInfectionsAnalyzer(expt_name=expname,
                                       sweep_variables=["Run_Number", "challenge_interval", 'SP_C50',  'SP_kill',
                                                        'Amod_C50', 'Amod_kill', 'drug_combo'],
                                       working_dir=working_dir,
                                       input_filename_base='InsetChart',
                                       output_filename='dailyNewInfections.csv'),
        ]
        am = AnalyzeManager(expid, analyzers=analyzers, force_analyze=True)
        am.analyze()

