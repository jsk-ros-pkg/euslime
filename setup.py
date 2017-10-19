from euswank import __version__

from setuptools import find_packages
from setuptools import setup

setup(
    name='euswank',
    description='Swank proxy for Euslisp',
    long_description=open('README.md').read(),
    version=__version__,
    author='Yuki Furuta',
    author_email='furushchev@jsk.imi.i.u-tokyo.ac.jp',
    url='https://github.com/furushchev/euswank',
    license='BSD',
    packages=find_packages(),
    install_requires=open('requirements.txt').readlines(),
)
