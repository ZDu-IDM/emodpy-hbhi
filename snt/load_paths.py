import os


def load_box_paths(user_path=None, country_name='Example'):
    if not user_path:
        user_path = os.path.expanduser('~')

    if country_name == 'Example':
        home_path = os.path.join(user_path, 'Documents', 'malaria-snt-core/example_for_migration')
        data_path = home_path
        project_path = os.path.join(home_path, 'example_files')
    elif country_name == 'SierraLeone':
        home_path = os.path.join(user_path, 'Dropbox (IDM)', 'Malaria Team Folder')
        data_path = os.path.join(home_path, 'data')
        project_path = os.path.join(home_path, 'projects', 'SierraLeone_hbhi')
    elif country_name == 'Burundi':
        home_path = os.path.join(user_path, 'Dropbox (IDM)', 'Malaria Team Folder')
        data_path = os.path.join(home_path, 'data')
        project_path = os.path.join(home_path, 'projects', 'burundi_hbhi', 'snt_2023')
    elif country_name == 'Nigeria':
        home_path = os.path.join(user_path, 'Dropbox (IDM)', 'NU_collaboration')
        data_path = os.path.join(home_path, 'hbhi_nigeria', 'snt_2022')
        project_path = os.path.join(home_path, 'hbhi_nigeria', 'snt_2022')

    return data_path, project_path


