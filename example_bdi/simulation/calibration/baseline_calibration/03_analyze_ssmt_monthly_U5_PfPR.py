# running the SSMT analyzer will create output files on COMPS in the Work Items section.

from simtools.Analysis.SSMTAnalysis import SSMTAnalysis
from simtools.SetupParser import SetupParser
from simulation.analyzers.analyze_monthly_pfpr_u5 import MonthlyPfPRU5Analyzer
import os
import sys
sys.path.append('../../')

experiments = {
    # 'PfPR_sweep_main_BDI_v1': 'b7b6503f-d4f0-ed11-aa06-b88303911bc1'
    'PfPR_sweep_main_BDI_v2': '070bd4a8-eff2-ed11-aa06-b88303911bc1'
}
start_year = 2010
end_year = 2017

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
