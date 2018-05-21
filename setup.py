import euslime

from setuptools import find_packages
from setuptools import setup

setup(
    name=euslime.__name__,
    description=euslime.__doc__,
    long_description=open('README.md').read(),
    version=euslime.__version__,
    author=euslime.__author__,
    url='https://github.com/furushchev/euslime',
    license='BSD',
    packages=find_packages(),
    install_requires=open('requirements.txt').readlines(),
    entry_points={
        'console_scripts': [
            'euslime = euslime.cli:main',
        ],
    },
)
