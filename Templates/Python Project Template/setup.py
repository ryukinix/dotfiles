#!/usr/bin/env python3
#
#    Copyright © Nexus Edge
#
#       @team: Data Science & Artificial Intelligence
#    @project: Natural Language Processing: Training
#     @author: Manoel Vilela
#      @email: manoel.vilela@nexusedge.com.br
#

from setuptools import setup, find_packages
from codecs import open  # To use a consistent encoding
from os import path
import project

here = path.abspath(path.dirname(__file__))
readme = path.join(here, 'README.md')

with open(readme, encoding='utf-8') as f:
    long_description = f.read()


with open('requirements.txt') as f:
    install_requires = list(map(str.strip, f.readlines()))


setup(
    name=project.__name__,
    version=project.__version__,
    description="{description}",
    long_description=long_description,
    classifiers=[
        "Environment :: Console",
        "Development Status :: 3 - Alpha",
        "Topic :: Utilities",
        "Operating System :: Unix",
        "Programming Language :: Python :: 3.4",
        "Programming Language :: Python :: 3.5",
    ],
    # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
    keywords='{topic} {topic-2} {topic-3} {topic-4}',
    author=project.__author__,
    author_email=project.__email__,
    url=project.__url__,
    download_url="{u}/archive/v{v}.tar.gz".format(u=project.__url__,
                                                  v=project.__version__),
    zip_safe=False,
    license='ONEL',
    packages=find_packages(exclude=['ez_setup', 'examples',
                                    'tests', 'docs', '__pycache__']),
    platforms='unix',
    install_requires=install_requires,
    entry_points={  # no entry-points yet
        # 'console_scripts': [
        #     'project = project.cli:main'
        # ]
    },
)
