import os
import warnings
from dtk.utils.core.DTKConfigBuilder import DTKConfigBuilder
from simtools.ExperimentManager.ExperimentManagerFactory import ExperimentManagerFactory
from simtools.SetupParser import SetupParser
from simtools.ModBuilder import ModBuilder, ModFn
from malaria.reports.MalariaReport import add_filtered_report, add_summary_report
from dtk.utils.builders.sweep import GenericSweepBuilder
from dtk.generic.climate import set_climate_constant
from malaria.interventions.adherent_drug import configure_adherent_drug
from malaria.interventions.malaria_challenge import add_challenge_trial
from malaria.interventions.malaria_drug_campaigns import add_drug_campaign
from malaria.interventions.malaria_vaccdrug_campaigns import add_vaccdrug_campaign
from simulation.sim_check_params.update_drug_params import update_drug_config_from_list

num_seeds = 5
years = 3
drug_day = (years-1)*365
include_MDAs = False

drug_combo = 'AQ'  # options: 'none', 'SPAQ', 'SP', 'AQ', 'vaccSMC'
challenge_trial = True  # if this is to see whether drugs prevent new infections; alternative is to see how well drug clears existing infections
# Note that there is a delay between the challenge date and when symptoms appear (which is the day used in the reference dataset). Here, the delay between challenge and symptom onset is 12-13 days, plus a delay for seeking treatment.
#     So a child who appears in a clinic on day 20 with symptoms was most likely infected sometime between 4-8 days after treatment.
challenge_interval_list = [1, 7, 15, 20, 27, 100]  # [7, 14, 21, 30, 35, 40, 100] # [10, 20, 40, 60]  # [10, 20, 30, 40, 50, 60, 80]  # only used if challenge_trial = True
treatment_day_interval = 25  # days after final challenge / infection that drugs are given

# set whether this is the control or intervention arm
if drug_combo == 'none':
    include_smc = False
    sp_C50_list = [0]
    sp_kill_list = [0]
    amod_C50_list = [0]
    amod_kill_list = [0]
else:
    include_smc = True
    sp_C50_list = [0.8, 0.9]
    sp_kill_list = [0.28]  # [0.5, 0.6, 0.7, 0.8, 0.9, 0.99]  # [0.7, 0.8, 0.9, 0.99]
    amod_C50_list = [55]  # [20, 100]
    amod_kill_list = [0.2]  # [0.6, 0.75, 0.9]


include_vaccSMC = False
if drug_combo == 'none':
    include_A = False
    include_SP = False
elif drug_combo == 'vaccSMC':
    include_vaccSMC = True
    include_A = False
    include_SP = False
    sp_C50_list = [0]
    sp_kill_list = [0]
    amod_C50_list = [0]
    amod_kill_list = [0]
elif drug_combo == 'SP':
    include_A = False
    include_SP = True
    amod_C50_list = [0]
    amod_kill_list = [0]
elif drug_combo == 'AQ':
    include_A = True
    include_SP = False
    sp_C50_list = [0]
    sp_kill_list = [0]
elif drug_combo == 'SPAQ':
    include_A = True
    include_SP = True
else:
    warnings.warn('drug_combo name not recognized. Assuming full SPAQ.')

if __name__ == "__main__":

    if include_smc:
        if challenge_trial:
            expname = 'SMC_efficacy_prevent_%s_v5' % drug_combo
        else:
            expname = 'SMC_efficacy_treat_%s_v6' % drug_combo
    else:
        if challenge_trial:
            expname = 'SMC_efficacy_prevent_control_v5'
        else:
            expname = 'SMC_efficacy_treat_control_v6'

    cb = DTKConfigBuilder.from_defaults('MALARIA_SIM')
    cb.update_params({'Vector_Species_Names': [],
                      'Simulation_Duration': 365*years,
                      'Demographics_Filenames': [os.path.join('demographics_and_climate', '_entire_country', 'Malariatherapy_demographics2.json')],  # everyone starts as 2 years (730 days) old
                      'Report_Detection_Threshold_Blood_Smear_Parasites': 1,  # for PCR(?)#!50,
                      "Parasite_Smear_Sensitivity": 1,  # for PCR(?) #0.1,  # 0.02,  # 50/uL  # number of microliters of blood tested
                      'RDT_Sensitivity': 0.1,

                      'Incubation_Period_Distribution': 'CONSTANT_DISTRIBUTION',
                      "Incubation_Period_Constant": 3,
                      "Immunity_Acquisition_Factor": 1,
                      "Immunity_Initialization_Distribution_Type": "DISTRIBUTION_OFF",
                      "Immunity_Mortality_Factor": 1,
                      "Immunity_Transmission_Factor": 1,
                      'Serialized_Population_Writing_Type': 'NONE',
                      'Serialized_Population_Reading_Type': 'NONE',
                      })
    set_climate_constant(cb)

    # add a few infectious challenge at beginning of simulation (since children will likely not be naive at exposure)
    if challenge_trial:
        infectious_exposure_days = [1, 100, 200, 300, 400, 500]
    else:  # exposures keep happening until a couple weeks before drug administration
        infectious_exposure_days = [1, 100, 200, 300, 400, 500, 600, (drug_day - treatment_day_interval)]
    for dd in infectious_exposure_days:
        add_challenge_trial(cb, start_day=dd)

    # # clear infections from initial challenges, though more exposures may follow final drug campaign
    # if include_MDAs:
    #     add_drug_campaign(cb, 'MDA', drug_code='AL', start_days=[drug_day-365+180],
    #                       coverage=1,
    #                       target_group={'agemin': 0, 'agemax': 5},
    #                       receiving_drugs_event_name='Received_MDA')
    #     add_drug_campaign(cb, 'MDA', drug_code='AL', start_days=[drug_day-365+240],
    #                       coverage=1,
    #                       target_group={'agemin': 0, 'agemax': 5},
    #                       receiving_drugs_event_name='Received_MDA')

    if include_smc:
        # account for adherence and resistance
        default_adherence = 1
        # resist_freq = 0.89  # approx for triple dhfr mutation from Wang et al. 2022  https://doi.org/10.1128/spectrum.00528-22
        # chance_of_resist_infect = 1 - (1 - resist_freq) ** 2  # assume two chances to be infected with resistant parasites
        # failure_from_resist = 0.144
        # sp_resist_day1_multiply = 1 - (chance_of_resist_infect * failure_from_resist)
        sp_resist_day1_multiply = 1  # assume no impact from resistance

        # add SP, AQ, or SPAQ for all children under 5, use 100% coverage for all doses and a single round
        if include_SP:
            sp_adherent_config = configure_adherent_drug(cb, doses=[['SulfadoxinePyrimethamine']],
                                                         dose_interval=1,
                                                         non_adherence_options=['Stop'],
                                                         non_adherence_distribution=[1],
                                                         adherence_config={
                                                             "class": "WaningEffectMapCount",
                                                             "Initial_Effect": 1,
                                                             "Durability_Map": {
                                                                 "Times": [1.0],
                                                                 "Values": [1]
                                                             }
                                                         }
                                                         )
            add_drug_campaign(cb, 'SMC', start_days=[drug_day],
                              coverage=1,
                              target_group={'agemin': 0.25, 'agemax': 5},
                              listening_duration=2,
                              adherent_drug_configs=[sp_adherent_config])

        if include_A:
            aq_adherent_config = configure_adherent_drug(cb, doses=[['Amodiaquine'],
                                                                    ['Amodiaquine'],
                                                                    ['Amodiaquine']],
                                                         dose_interval=1,
                                                         non_adherence_options=['Stop'],
                                                         non_adherence_distribution=[1],
                                                         adherence_config={
                                                             "class": "WaningEffectMapCount",
                                                             "Initial_Effect": 1,
                                                             "Durability_Map": {
                                                                 "Times": [
                                                                     1.0,
                                                                     2.0,
                                                                     3.0
                                                                 ],
                                                                 "Values": [
                                                                     1,  # for day 1
                                                                     default_adherence,  # day 2
                                                                     default_adherence  # day 3
                                                                 ]
                                                             }
                                                         }
                                                         )
            add_drug_campaign(cb, 'SMC', start_days=[drug_day],
                              coverage=1,
                              target_group={'agemin': 0.25, 'agemax': 5},
                              listening_duration=2,
                              adherent_drug_configs=[aq_adherent_config])

        if include_vaccSMC:
            add_vaccdrug_campaign(cb, campaign_type='SMC', start_days=[drug_day],
                                  coverages=[1],
                                  target_group={'agemin': 0.25, 'agemax': 5},
                                  receiving_drugs_event=False)  ## If False uses vaccSMC with automatic offset of 17 days, if True, uses vaccDrugSMC

    # CUSTOM REPORTS
    add_filtered_report(cb, start=0, end=years * 365)
    for year in range(years):
        add_summary_report(cb, start=365 * year, age_bins=[0.25, 5, 125], interval=30, duration_days=365,
                           description='Monthly%d' % (year + 2020), parasitemia_bins=[10, 50, 1e9])


    # BUILDER
    if challenge_trial:
        builder = ModBuilder.from_list([[
                                         ModFn(update_drug_config_from_list,
                                               # note: this call updates all other drug params to the values specified in the update_drug_config file
                                               drug_nested_param_value_list=[[['SulfadoxinePyrimethamine', 'Drug_PKPD_C50'], sp_C50],
                                                                             [['SulfadoxinePyrimethamine', 'Max_Drug_IRBC_Kill'], sp_kill],
                                                                             [['Amodiaquine', 'Drug_PKPD_C50'], amod_C50],
                                                                             [['Amodiaquine', 'Max_Drug_IRBC_Kill'], amod_kill]
                                                                             ]
                                               ),
                                         ModFn(add_challenge_trial, start_day=(drug_day + challenge_interval)),
                                         ModFn(DTKConfigBuilder.set_param, 'Run_Number', x),
                                         ModFn(DTKConfigBuilder.set_param, 'challenge_interval', challenge_interval),
                                         ModFn(DTKConfigBuilder.set_param, 'SP_C50', sp_C50),
                                         ModFn(DTKConfigBuilder.set_param, 'SP_kill', sp_kill),
                                         ModFn(DTKConfigBuilder.set_param, 'Amod_C50', amod_C50),
                                         ModFn(DTKConfigBuilder.set_param, 'Amod_kill', amod_kill),
                                         ModFn(DTKConfigBuilder.set_param, 'drug_combo', drug_combo),
                                         ]
                                        for challenge_interval in challenge_interval_list
                                        for sp_C50 in sp_C50_list
                                        for sp_kill in sp_kill_list
                                        for amod_C50 in amod_C50_list
                                        for amod_kill in amod_kill_list
                                        for x in range(num_seeds)
                                        ])
    else:
        builder = ModBuilder.from_list([[
            ModFn(update_drug_config_from_list,
                  # note: this call updates all other drug params to the values specified in the update_drug_config file
                  drug_nested_param_value_list=[[['SulfadoxinePyrimethamine', 'Drug_PKPD_C50'], sp_C50],
                                                [['SulfadoxinePyrimethamine', 'Max_Drug_IRBC_Kill'], sp_kill],
                                                [['Amodiaquine', 'Drug_PKPD_C50'], amod_C50],
                                                [['Amodiaquine', 'Max_Drug_IRBC_Kill'], amod_kill]
                                                ]
                  ),
            ModFn(DTKConfigBuilder.set_param, 'Run_Number', x),
            ModFn(DTKConfigBuilder.set_param, 'SP_C50', sp_C50),
            ModFn(DTKConfigBuilder.set_param, 'SP_kill', sp_kill),
            ModFn(DTKConfigBuilder.set_param, 'Amod_C50', amod_C50),
            ModFn(DTKConfigBuilder.set_param, 'Amod_kill', amod_kill),
            ModFn(DTKConfigBuilder.set_param, 'drug_combo', drug_combo),
        ]
            for sp_C50 in sp_C50_list
            for sp_kill in sp_kill_list
            for amod_C50 in amod_C50_list
            for amod_kill in amod_kill_list
            for x in range(num_seeds)
        ])


    run_sim_args = {
        'exp_name': expname,
        'config_builder': cb,
        'exp_builder': builder
    }

    SetupParser.default_block = 'HPC'
    SetupParser.init()
    exp_manager = ExperimentManagerFactory.init()
    exp_manager.run_simulations(**run_sim_args)
