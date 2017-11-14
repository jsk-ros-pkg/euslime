import euswank

from setuptools import find_packages
from setuptools import setup

setup(
    name=euswank.__name__,
    description=euswank.__doc__,
    long_description=open('README.md').read(),
    version=euswank.__version__,
    author=euswank.__author__,
    url='https://github.com/furushchev/euswank',
    license='BSD',
    packages=find_packages(),
    install_requires=open('requirements.txt').readlines(),
    entry_points={
        'console_scripts': [
            'euswank = euswank.cli:main',
        ],
    },
)
