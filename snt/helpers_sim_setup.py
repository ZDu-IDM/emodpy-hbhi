import os
import copy
import sys

import pandas as pd
import numpy as np

import emod_api.config.default_from_schema_no_validation as dfs
from emodpy_malaria.vector_config import set_species_param
from emodpy_malaria.vector_config import add_species
from emod_api.interventions.migration import add_migration_event

# from COMPS.Data import Simulation, QueryCriteria
# from simtools.Utilities.COMPSUtilities import COMPS_login
# from simtools.Utilities.Experiments import retrieve_experiment, retrieve_simulation

# from dtk.interventions.outbreakindividual import recurring_outbreak  # moved

# from dtk.vector.species import update_species_param, set_species
# from dtk.vector.species import set_larval_habitat
# from dtk.interventions.property_change import change_individual_property

# from dtk.utils.reports.CustomReport import BaseReport
from emod_api.interventions.common import PropertyValueChanger, ScheduledCampaignEvent

manifest = sys.modules['manifest']


# class ReportSimulationStats(BaseReport):
#     def __init__(self, type="ReportSimulationStats"):
#         BaseReport.__init__(self, type=type)
#
#     def to_dict(self):
#         d = super(ReportSimulationStats, self).to_dict()
#         # d.update({"Report_File_Name": 'ReportSimulationStats' + '.json'})
#         return d


def update_basic_params(config, project_path):
    vector_species = ['arabiensis', 'funestus', 'gambiae']
    # set_species(config, vector_species)       # TODO: do we need these? Have done it by default already?

    from emodpy_malaria.vector_config import add_species
    manifest = sys.modules['manifest']  # TODO: make sure
    add_species(config, manifest, ["arabiensis", "funestus", "gambiae", "minimus"])

    # cb.update_params({
    #     "Birth_Rate_Dependence": "FIXED_BIRTH_RATE",
    #     "Age_Initialization_Distribution_Type": 'DISTRIBUTION_COMPLEX',
    #     'Disable_IP_Whitelist': 1,
    #     'logLevel_JsonConfigurable': 'ERROR',
    #     'logLevel_VectorHabitat': 'ERROR',
    #     'logLevel_StandardEventCoordinator': 'ERROR',
    #
    #     "Vector_Species_Names": vector_species,
    #     'Enable_Default_Reporting': 0,
    #     'Enable_Vector_Species_Report': 0,
    #     'Enable_Property_Output': 0,
    #
    #     'Enable_Demographics_Risk': 1,
    #     'Report_Detection_Threshold_Blood_Smear_Parasites': 0,
    #     "Parasite_Smear_Sensitivity": 0.01,  # number of microliters of blood examined
    #     'RDT_Sensitivity': 0.1,
    #
    #     'Incubation_Period_Distribution': 'CONSTANT_DISTRIBUTION',
    #     "Incubation_Period_Constant": 3,  # parameter shortened from 7 to yield a 12-13 day incubation period
    #     "Immunity_Acquisition_Factor": 1,
    #     "Immunity_Initialization_Distribution_Type": "DISTRIBUTION_OFF",
    #     "Immunity_Mortality_Factor": 1,
    #     "Immunity_Transmission_Factor": 1,
    # })

    config.parameters.Birth_Rate_Dependence = "FIXED_BIRTH_RATE"
    config.parameters.Age_Initialization_Distribution_Type = 'DISTRIBUTION_COMPLEX'
    # config.parameters.Disable_IP_Whitelist = 1      # TODO: NOT FOUND
    # config.parameters.logLevel_JsonConfigurable = 'ERROR'     # TODO: NOT FOUND
    # config.parameters.logLevel_VectorHabitat = 'ERROR'  # TODO: NOT FOUND
    # config.parameters.logLevel_StandardEventCoordinator = 'ERROR'   # TODO: NOT FOUND

    # config.parameters.Vector_Species_Names = vector_species     # TODO: NOT FOUND
    config.parameters.Enable_Default_Reporting = 0
    config.parameters.Enable_Vector_Species_Report = 0
    config.parameters.Enable_Property_Output = 0

    config.parameters.Enable_Demographics_Risk = 1
    config.parameters.Report_Detection_Threshold_Blood_Smear_Parasites = 0
    # config.parameters.Parasite_Smear_Sensitivity = 0.01  # TODO: NOT FOUND   # number of microliters of blood examined
    # config.parameters.RDT_Sensitivity = 0.1     # TODO: NOT FOUND

    config.parameters.Incubation_Period_Distribution = 'CONSTANT_DISTRIBUTION'
    config.parameters.Incubation_Period_Constant = 3
    # config.parameters.Immunity_Acquisition_Factor = 1       # TODO: NOT FOUND
    # config.parameters.Immunity_Initialization_Distribution_Type = "DISTRIBUTION_OFF"    # TODO: NOT FOUND
    # config.parameters.Immunity_Mortality_Factor = 1     # TODO: NOT FOUND
    # config.parameters.Immunity_Transmission_Factor = 1      # TODO: NOT FOUND

    # read in and set vector bionomics
    vector_bionomics = pd.read_csv(os.path.join(project_path, 'simulation_inputs', 'vector_bionomics.csv'))
    habitat = dfs.schema_to_config_subnode(manifest.schema_file, ["idmTypes", "idmType:VectorHabitat"])
    for vector in vector_species:
        bionomics_row = vector_bionomics[vector_bionomics['species'] == vector].reset_index()
        # update_species_param(config, vector, 'Anthropophily', bionomics_row['Anthropophily'][0], overwrite=True)
        # update_species_param(config, vector, 'Indoor_Feeding_Fraction', bionomics_row['Indoor_Feeding_Fraction'][0],
        #                      overwrite=True)
        # update_species_param(config, vector, 'Days_Between_Feeds', bionomics_row['Days_Between_Feeds'][0],
        #                      overwrite=True)

        new_habitat1 = copy.deepcopy(habitat)
        set_species_param(config, vector, 'Anthropophily', bionomics_row['Anthropophily'][0], overwrite=True)
        set_species_param(config, vector, 'Indoor_Feeding_Fraction', bionomics_row['Indoor_Feeding_Fraction'][0],
                          overwrite=True)
        set_species_param(config, vector, 'Days_Between_Feeds', bionomics_row['Days_Between_Feeds'][0],
                          overwrite=True)


def habitat_scales(project_path):
    rel_abundance_fname = os.path.join(project_path, 'ento', 'DS_vector_rel_abundance.csv')
    rdf = pd.read_csv(rel_abundance_fname)
    rdf = rdf.rename(columns={'Anopheles_arabiensis': 'arabiensis_scale_factor',
                              'Anopheles_coluzzii_gambiae': 'gambiae_scale_factor',
                              'Anopheles_funestus_subgroup': 'funestus_scale_factor'})
    rdf = rdf.set_index('DS')
    return rdf


def set_input_files(config, hfca, archetype_hfca, population_size=1000):
    # cb.update_params({
    #     'DS': hfca,
    #     'Archeype': archetype_hfca,
    #     # 'Demographics_Filenames': [os.path.join('demographics_and_climate', hfca, '%s_demographics_wVaxSMC_IPTi.json' % hfca)],
    #     # 'Demographics_Filenames': [os.path.join('demographics_and_climate', hfca, '%s_demographics.json' % hfca)],
    #     'Demographics_Filenames': [os.path.join('demographics_and_climate', '_entire_country',
    #                                             'demographics_each_admin_%i.json' % population_size)],
    #     # 'season_calib_deomgraphics.json')],
    #     "Climate_Model": "CLIMATE_CONSTANT",
    #     # "Air_Temperature_Filename": os.path.join('demographics_and_climate',archetype_hfca, '%s_air_temperature_daily_2016.bin' % archetype_hfca),
    #     # "Land_Temperature_Filename": os.path.join('demographics_and_climate',archetype_hfca, '%s_air_temperature_daily_2016.bin' % archetype_hfca),
    #     # "Rainfall_Filename": os.path.join('demographics_and_climate',archetype_hfca, '%s_rainfall_daily_2016.bin' % archetype_hfca),
    #     # "Relative_Humidity_Filename": os.path.join('demographics_and_climate',archetype_hfca, '%s_relative_humidity_daily_2016.bin' % archetype_hfca)
    # })

    # config.parameters.DS = hfca     # NOT FOUND in Schema
    # config.parameters.Archeype = archetype_hfca     # NOT FOUND in Schema
    config.parameters.Demographics_Filenames = [os.path.join('demographics_and_climate', '_entire_country',
                                                     'demographics_each_admin_%i.json' % population_size)]
    config.parameters.Climate_Model = "CLIMATE_CONSTANT"

    return {'DS': hfca}


def get_burnin_exp(platform, burnin_id='', burnin_fname=''):
    # COMPS_login('https://comps.idmod.org')
    if burnin_id:
        print("building from pickup: identifying burnin addresses from %s" % burnin_id)
        # # expt = retrieve_experiment(burnin_id, force_update=True)  # use this if the experiment address is not found even though the experiment succeeded on COMPS
        # expt = retrieve_experiment(burnin_id)
        # # creating data with all the simulation tags
        # ser_df = pd.DataFrame([x.tags for x in expt.simulations])
        # # getting paths for all the sims
        # # ser_df["outpath"] = pd.Series([sim.get_path() for sim in expt.simulations])  # old version that no longer works when initial simulation attempts fail
        # ser_df["outpath"] = pd.Series(
        #     [Simulation.get(sim.id, QueryCriteria().select_children('hpc_jobs')).hpc_jobs[-1].working_directory for
        #      sim in expt.simulations])
        ser_df = platform.create_sim_directory_df(burnin_id)
    else:
        print("building from pickup: identifying burnin addresses from %s" % burnin_fname)
        ser_df = pd.read_csv(burnin_fname)
    return ser_df


def set_up_hfca(simulation, hfca, archetype_hfca=None,
                pull_from_serialization=False, ser_date=50 * 365,
                hdf=None, lhdf=None, population_size=1000,
                hab_multiplier=-1, run_number=-1, use_arch_burnin=False, ser_df=pd.DataFrame()):
    set_input_files(simulation.task.config, hfca, archetype_hfca, population_size)
    if not archetype_hfca:
        archetype_hfca = hfca

    set_habitats(simulation.task.config, hfca, hdf, lhdf, archetype_hfca, abs(hab_multiplier))

    if pull_from_serialization:
        hab_scale_factor_param_name = 'Habitat_Multiplier'
        if use_arch_burnin:
            if hab_multiplier >= 0 and run_number >= 0:
                ser_df[hab_scale_factor_param_name] = ser_df[hab_scale_factor_param_name].apply(
                    lambda x: np.round(x, 5))
                sdf = ser_df[(ser_df[hab_scale_factor_param_name] >= (np.round(hab_multiplier, 5) - 0.00001)) &
                             (ser_df[hab_scale_factor_param_name] <= (np.round(hab_multiplier, 5) + 0.00001)) &
                             (ser_df['Run_Number'] == run_number) &
                             (ser_df['admin_name'] == archetype_hfca)]
                # cb.update_params({
                #     hab_scale_factor_param_name: hab_multiplier
                # })
                simulation.task.config.parameters.hab_scale_factor_param_name = hab_multiplier

                ser_path = sdf['outpath'].values[0]
            else:
                ser_path = ser_df['outpath'].values[0]
        elif 'admin_name' in ser_df.columns.values:
            ser_df = ser_df[ser_df['Run_Number'] == run_number]
            ser_df = ser_df.set_index('admin_name')
            ser_path = ser_df.at[hfca, 'outpath']
        else:
            ser_path = ser_df['outpath'].values[0]

        # cb.update_params({
        #     'Serialized_Population_Reading_Type': 'READ',
        #     'Serialized_Population_Path': os.path.join(ser_path, 'output'),
        #     'Serialized_Population_Filenames': ['state-%05d.dtk' % ser_date],
        #     'Enable_Random_Generator_From_Serialized_Population': 0,
        #     'Serialization_Mask_Node_Read': 0
        #     # 0 corresponds to the previous version default: the same larval habitat parameters will be used in the burnin and pickup (from the burnin config)
        # })

        simulation.task.config.parameters.Serialized_Population_Reading_Type = 'READ'
        simulation.task.config.parameters.Serialized_Population_Path = os.path.join(ser_path, 'output')
        simulation.task.config.parameters.Serialized_Population_Filenames = ['state-%05d.dtk' % ser_date]
        simulation.task.config.parameters.Enable_Random_Generator_From_Serialized_Population = 0
        simulation.task.config.parameters.Serialization_Mask_Node_Read = 0
    else:
        # cb.update_params({
        #     'Serialized_Population_Reading_Type': 'NONE',
        # })
        simulation.task.config.parameters.Serialized_Population_Reading_Type = 'NONE'

    # ZDU: this is general not depends on individual simulation, campaign: moved to right location
    # if set_up_hfca gets called, we need to add the following
    # recurring_outbreak(cb, start_day=35, outbreak_fraction=0.002, tsteps_btwn=73)

    return {'admin_name': hfca}


def set_habitats(config, hfca, hdf, lhdf, archetype_hfca, hab_multiplier):
    from emodpy_malaria.vector_config import add_species
    manifest = sys.modules['manifest']  # TODO: make sure
    add_species(config, manifest, ["arabiensis", "funestus", "gambiae", "minimus"])  # TODO: really need?

    a = hdf.at[hfca, 'arabiensis_scale_factor']
    f = hdf.at[hfca, 'funestus_scale_factor']
    g = hdf.at[hfca, 'gambiae_scale_factor']
    tot = a + f + g
    a /= tot
    f /= tot
    g /= tot
    fraction = (max(0.00001, a), max(0.00001, f), max(0.00001, g))

    ls_hab_ref = {'Capacity_Distribution_Number_Of_Years': 1,
                  'Capacity_Distribution_Over_Time': {
                      'Times': [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334],
                      'Values': []
                  },
                  'Max_Larval_Capacity': 800000000}

    my_spline, maxvalue, const = load_spline_and_scale_factors(lhdf, archetype_hfca)
    const_mult = 1 if hab_multiplier >= 1 else hab_multiplier

    habitat = dfs.schema_to_config_subnode(manifest.schema_file, ["idmTypes", "idmType:VectorHabitat"])
    for (s, sp) in zip(fraction, ['arabiensis', 'funestus', 'gambiae']):
        hab = copy.copy(ls_hab_ref)
        hab['Capacity_Distribution_Over_Time']['Values'] = my_spline
        hab['Max_Larval_Capacity'] = pow(10, maxvalue) * s * hab_multiplier
        # this function updates EMOD parameters to what is requested based on the calibration parameter sampling
        # TODO: make sure value has the right format!
        # set_larval_habitat(config, {sp: {'LINEAR_SPLINE': hab,
        #                                  'CONSTANT': pow(10, const) * s * const_mult}})

        capacity_dist_per_year = {
            "Times": [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334],
            "Values": []
        }

        ls_hab_ref = {'Capacity_Distribution_Number_Of_Years': 1,
                      'Capacity_Distribution_Over_Time': {
                          'Times': [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334],
                          'Values': []
                      },
                      'Max_Larval_Capacity': 800000000}

        hab = copy.copy(ls_hab_ref)
        hab['Capacity_Distribution_Over_Time']['Values'] = my_spline
        hab['Max_Larval_Capacity'] = pow(10, maxvalue)*s*hab_multiplier
        hab["Habitat_Type"] = "LINEAR_SPLINE"
        hab["CONSTANT"] = pow(10, const)*s*const_mult
        # this function updates EMOD parameters to what is requested based on the calibration parameter sampling
        # set_larval_habitat(cb, {sp: {'LINEAR_SPLINE': hab,
        #                              'CONSTANT': pow(10, const)*s*const_mult}})

        # set_species_param(config, sp, 'Habitats', [hab], overwrite=True)  # TODO: [hab]?

        new_habitat4 = copy.deepcopy(habitat)
        new_habitat4.parameters.Habitat_Type = "LINEAR_SPLINE"
        new_habitat4.parameters.Capacity_Distribution_Number_Of_Years = 1
        new_habitat4.parameters.Capacity_Distribution_Over_Time = capacity_dist_per_year
        new_habitat4.parameters.Max_Larval_Capacity = 800000000
        # new_habitat4.parameters.CONSTANT =  pow(10, const)*s*const_mult   # TODO: how to set CONSTANT?
        # set_larval_habitat(config, new_habitat4)      # not sure it works

        # or update directly and no need to call set_larval_habitat
        set_species_param(config, sp, "Habitats", [new_habitat4.parameters], overwrite=False)


def set_larval_habitat(config, habitats):
    """
    Set vector species and habitat parameters of config argument and return

    Example::

        habitats = {"arabiensis": {"TEMPORARY_RAINFALL": 1.7e9, "CONSTANT": 1e7}}
    """
    from emodpy_malaria.vector_config import set_species_param
    for species, habitat in habitats.items():
        set_species_param(config, species, "Larval_Habitat_Types", habitat, overwrite=False)        # TODO: make sure


def load_spline_and_scale_factors(lhdf, archetype_hfca):
    lhdf = lhdf.set_index('archetype')
    my_spline = [lhdf.at[archetype_hfca, 'MonthVal%d' % x] for x in range(1, 13)]
    maxvalue = lhdf.at[archetype_hfca, 'MaxHab']
    const = lhdf.at[archetype_hfca, 'Constant']
    # pop_scale = lhdf.at[archetype_hfca, 'pop_scale']

    return my_spline, maxvalue, const  # , pop_scale


def load_master_csv(project_path):
    master_csv = os.path.join(project_path, 'admin_pop_archetype.csv')
    df = pd.read_csv(master_csv, encoding='latin')
    df['admin_name'] = df['admin_name'].str.normalize('NFKD').str.encode('ascii', errors='ignore').str.decode('utf-8')
    df = df.set_index('admin_name')
    return df


# def load_pop_scale_factor(lhdf, archetype_hfca) :
#
#     df = load_master_csv()
#     my_spline, maxvalue, const, pop_scale = load_spline_and_scale_factors(lhdf, archetype_hfca)
#     scale_factor = 1 / 10000. * (1 / pop_scale)
#     pop = df.at[archetype_hfca, 'population']
#
#     return pop, scale_factor


def change_individual_property_at_infect_event(campaign, new_ip_key, new_ip_value, start=0, agemin=0, agemax=0,
                                               coverage=1):
    pc = PropertyValueChanger(campaign, Target_Property_Key=new_ip_key, Target_Property_Value=new_ip_value)
    event = ScheduledCampaignEvent(campaign, Start_Day=start,
                                   Target_Age_Min=agemin,
                                   Target_Age_Max=agemax,
                                   Intervention_List=[pc],
                                   Demographic_Coverage=coverage)
    campaign.add(event)


def update_smc_access_ips(campaign, hfca, smc_df):
    df = smc_df[smc_df['admin_name'] == hfca]

    # change SMCAccess property of newborns (important for those born after the first SMC round in a year)
    if df.shape[0] > 0:
        # change_individual_property(campaign, 'SMCAccess', 'Low', target_group={'agemin': 0, 'agemax': 5}, coverage=1,
        #                            blackout_flag=False)
        # change_individual_property(campaign, 'SMCAccess', 'High', target_group={'agemin': 0, 'agemax': 5},
        #                            coverage=df['high_access_U5'].values[0], blackout_flag=False)

        change_individual_property_at_infect_event(campaign, 'SMCAccess', 'Low', agemin=0, agemax=5, coverage=1)
        change_individual_property_at_infect_event(campaign, 'SMCAccess', 'High', agemin=0, agemax=5,
                                                   coverage=df['high_access_U5'].values[0])

        # before the first SMC round in each year, change the SMCAccess IP for the U5 and O5 age groups
        # simdays of the first rounds (change IPs one week before)
        first_round_days = df.loc[df['round'] == 1, 'simday'].values
        change_IP_days = [first_round_days[yy] - 7 for yy in range(len(first_round_days))]

        for rr in change_IP_days:
            # change_individual_property(campaign, 'SMCAccess', 'Low', target_group={'agemin': 0, 'agemax': 5},
            #                            start_day=rr,
            #                            coverage=1, blackout_flag=False)
            # change_individual_property(campaign, 'SMCAccess', 'High', target_group={'agemin': 0, 'agemax': 5},
            #                            start_day=rr,
            #                            coverage=df['high_access_U5'].values[0], blackout_flag=False)
            # change_individual_property(campaign, 'SMCAccess', 'Low', target_group={'agemin': 5, 'agemax': 120},
            #                            start_day=rr,
            #                            coverage=1, blackout_flag=False)
            # change_individual_property(campaign, 'SMCAccess', 'High', target_group={'agemin': 5, 'agemax': 120},
            #                            start_day=rr,
            #                            coverage=df['high_access_5_10'].values[0], blackout_flag=False)

            change_individual_property_at_infect_event(campaign, 'SMCAccess', 'Low', start=rr, agemin=0, agemax=5,
                                                       coverage=1)
            change_individual_property_at_infect_event(campaign, 'SMCAccess', 'High', start=rr, agemin=0, agemax=5,
                                                       coverage=df['high_access_U5'].values[0])
            change_individual_property_at_infect_event(campaign, 'SMCAccess', 'Low', start=rr, agemin=5, agemax=120,
                                                       coverage=1)
            change_individual_property_at_infect_event(campaign, 'SMCAccess', 'High', start=rr, agemin=5, agemax=120,
                                                       coverage=df['high_access_5_10'].values[0])

    return {'admin_name': hfca}


if __name__ == "__main__":
    pass
