import pandas as pd
import numpy as np
from simtools.Analysis.BaseAnalyzers import BaseAnalyzer
import datetime
import os
import sys
sys.path.append('../')
from simulation.load_paths import load_box_paths
import matplotlib.pyplot as plt
import matplotlib as mpl

mpl.rcParams['pdf.fonttype'] = 42

datapath, projectpath = load_box_paths()


class SeasonalityPlotter(BaseAnalyzer):

    @classmethod
    def monthparser(self, x):
        if x == 0:
            return 12
        else:
            return datetime.datetime.strptime(str(x), '%j').month

    def __init__(self, expt_name, sample, hfca, sweep_variables=None, working_dir="."):
        super(SeasonalityPlotter, self).__init__(working_dir=working_dir,
                                                   filenames=['output/ReportEventCounter.json',
                                                              'output/ReportMalariaFiltered.json']
                                                   )
        self.sweep_variables = sweep_variables or ["Run_Number", '__sample_index__']
        self.expt_name = expt_name
        self.sample = sample
        self.hfca = hfca
        self.population_channel = 'Statistical Population'
        self.case_channel = 'Received_Treatment'
        self.prev_channel = 'PfHRP2 Prevalence'
        self.nmf_channel = 'Received_NMF_Treatment'
        self.comparison_channel = 'Treated Cases NMF Adjusted'

        reference_fname = os.path.join(projectpath, 'simulation_inputs', 'incidence', 'archetype_incidence.csv')
        ref_df = pd.read_csv(reference_fname)
        ref_df = ref_df[ref_df['seasonality_archetype'] == hfca]
        ref_df = ref_df.rename(columns={'month': 'Month',
                                        'population': 'Trials'})
        ref_df['Observations'] = ref_df['incidence'] * ref_df['Trials'] / 1000
        self.ref = ref_df[['Month', 'Trials', 'Observations']]

    def filter(self, simulation):
        return simulation.tags['__sample_index__'] == self.sample

    def select_simulation_data(self, data, simulation):

        # Load data from simulation
        simdata = {self.case_channel: data[self.filenames[0]]['Channels'][self.case_channel]['Data'][-365:],
                   self.nmf_channel: data[self.filenames[0]]['Channels'][self.nmf_channel]['Data'][-365:],
                   self.population_channel: data[self.filenames[1]]['Channels'][self.population_channel]['Data'][-365:],
                   self.prev_channel: data[self.filenames[1]]['Channels'][self.prev_channel]['Data'][-365:]}

        simdata = pd.DataFrame(simdata)
        simdata[self.comparison_channel] = simdata[self.case_channel] + simdata[self.nmf_channel]
        # inflate pop for undercounted denom
        # simdata[self.population_channel] = simdata[self.population_channel]  # *1.2


        simdata = simdata[-365:].reset_index(drop=True)
        simdata['Time'] = simdata.index
        simdata['Day'] = simdata['Time'] % 365
        simdata['Month'] = simdata['Day'].apply(lambda x: self.monthparser((x+1) % 365))

        simdata = simdata.rename(columns={ self.population_channel : 'Trials',
                                           self.comparison_channel : 'Observations'})

        s1 = simdata.groupby('Month')['Trials'].agg(np.mean).reset_index()
        s2 = simdata.groupby('Month')['Observations'].agg(np.sum).reset_index()
        simdata = pd.merge(left=s1, right=s2, on='Month')
        simdata = simdata[['Month', 'Trials', 'Observations']]

        for sweep_var in self.sweep_variables:
            if sweep_var in simulation.tags.keys():
                simdata[sweep_var] = simulation.tags[sweep_var]
        return simdata

    def finalize(self, all_data):

        selected = [data for sim, data in all_data.items()]
        if len(selected) == 0:
            print("No data have been returned... Exiting...")
            return

        adf = pd.concat(selected).reset_index(drop=True)
        self.ref['incidence'] = self.ref['Observations']/self.ref['Trials']*1000

        fig = plt.figure()
        ax = fig.gca()

        for s, plot_df in adf.groupby('Run_Number') :
            plot_df['incidence'] = plot_df['Observations'] / plot_df['Trials']*1000
            ax.plot(plot_df['Month'], plot_df['incidence'], '-', color='r', linewidth=0.5, alpha=0.3)

        plot_df = adf.groupby('Month').agg(np.mean).reset_index()
        plot_df['incidence'] = plot_df['Observations'] / plot_df['Trials']*1000
        ax.plot(plot_df['Month'], plot_df['incidence'],  '-o', color='r', label='sim')
        ax.plot(self.ref['Month'], self.ref['incidence'],
                '-o', color='#7AC4CD', label='reference')
        ax.set_xlabel('Month')
        ax.set_ylabel('Treated Clinical Case incidence')
        ax.set_ylim(0,)
        ax.legend()

        fig.savefig(os.path.join(self.working_dir, '%s_sample_%d_case_counts.png' % (self.hfca, self.sample)))
        fig.savefig(os.path.join(self.working_dir, '%s_sample_%d_case_counts.pdf' % (self.hfca, self.sample)),
                    format='PDF')
        plt.show()


if __name__ == "__main__":

    from simtools.Analysis.AnalyzeManager import AnalyzeManager
    from simtools.SetupParser import SetupParser

    SetupParser.default_block = 'HPC'
    SetupParser.init()

    hfca = 'Gitega'
    analyzer = SeasonalityPlotter(expt_name='%s_seasonality_fit' % hfca,
                                  sweep_variables=["Run_Number",  "__sample_index__"],
                                  sample=14,
                                  hfca=hfca,
                                  working_dir=os.path.join(projectpath, 'simulation_output', 'seasonality_calibration'))

    am = AnalyzeManager('d7fca2d4-e196-eb11-a2ce-c4346bcb1550', analyzers=analyzer)
    am.analyze()
