## ! DO NOT MANUALLY INVOKE THIS setup.py

from setuptools import find_packages
from setuptools import setup
from catkin_pkg.python_setup import generate_distutils_setup

setup_args = generate_distutils_setup(
    packages=find_packages('src'),
    package_dir={'': 'src'},
    install_requires=open('requirements.txt').readlines(),
    entry_points={
        'console_scripts': [
            'euslime = euslime.cli:main',
            ],
        },
)

setup(**setup_args)
