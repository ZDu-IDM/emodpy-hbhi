# running the SSMT analyzer will create output files on COMPS in the Work Items section.

from simtools.Analysis.SSMTAnalysis import SSMTAnalysis
from simtools.SetupParser import SetupParser
from simulation.analyzers.analyze_monthly_pfpr_u5 import MonthlyPfPRU5Analyzer
import os
import sys
sys.path.append('../../')

experiments = {
    'PfPR_calibration_main_sweep_NGA_v6_part1': 'cf23124c-b6be-ed11-aa03-b88303911bc1',  # v4: 'e98a16d1-9b92-ed11-aa01-b88303911bc1',  # v3: 'a655e47e-b78b-ed11-aa00-b88303911bc1',
    'PfPR_calibration_main_sweep_NGA_v6_part2': '504cc10c-cebe-ed11-aa03-b88303911bc1'  # v4: '115851ce-ab92-ed11-aa01-b88303911bc1',  # v3: 'd8203b75-cd8b-ed11-aa00-b88303911bc1'
}
start_year = 2010
end_year = 2021

working_dir = "."

if __name__ == "__main__":
    SetupParser.default_block = 'HPC'
    SetupParser.init()

    analyzers = [
        MonthlyPfPRU5Analyzer
                 ]

    sweep_variables = ["Run_Number",
                       "Habitat_Multiplier",
                       "admin_name",
                       ]

    for expt_name, exp_id in experiments.items():
        # if not os.path.exists(os.path.join(working_dir, expt_name)):
        #     os.mkdir(os.path.join(working_dir, expt_name))
        # working_dir = os.path.join(working_dir, expt_name)

        wi_name = "ssmt_analyzer_%s" % expt_name

        args_each = {'expt_name': expt_name,
                     'sweep_variables': sweep_variables,
                     'working_dir': working_dir,
                     'start_year': start_year,
                     'end_year': end_year
                     }
        analysis = SSMTAnalysis(experiment_ids=[exp_id],
                                analyzers=analyzers,
                                analyzers_args=[args_each]*len(analyzers),
                                analysis_name=wi_name)

        analysis.analyze()
