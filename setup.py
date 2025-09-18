from setuptools import setup, find_packages

setup(
    name="bcm-gsea",
    version="0.1",
    packages=find_packages(),
    include_package_data=True,
    install_requires=[
        "Click",
        "ollama",
    ],
    entry_points={
        "console_scripts": [
            "bcm-gsea=python.cli:main",
        ],
    },
)
