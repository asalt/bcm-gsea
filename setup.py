from setuptools import setup, find_packages

setup(
    name="tackle2",
    version="0.1",
    packages=find_packages(),
    include_package_data=True,
    install_requires=[
        "Click",
        "ollama",
    ],
    entry_points={
        "console_scripts": [
            "tackle2=python.cli:main",
        ],
    },
)
