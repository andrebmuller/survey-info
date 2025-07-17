# survey-info

A specialized AutoCAD LISP routine for generating survey drilling markers (palitos de sondagem) with data imported from CSV files.

## Description

This tool creates standardized survey drilling markers in AutoCAD drawings by reading survey data from a CSV file. It generates visual representations of drilling information including depth markers, water level indicators, and project details.

## Features

- **CSV Data Import**: Reads survey data from `sondagens.csv` file
- **Automated Drawing Generation**: Creates standardized drilling markers with flags and depth indicators
- **Water Level Indicators**: Displays groundwater level (N.A.) markers
- **Layer Management**: Automatically creates and manages drawing layers
- **Optimized Performance**: Efficient point calculations and drawing operations

## Requirements

- AutoCAD (any version supporting AutoLISP)
- CSV file named `sondagens.csv` in the drawing directory

## Installation

1. Copy `survey-info.lsp` to your AutoCAD support folder
2. Load the LISP file in AutoCAD using `APPLOAD` command
3. Place your `sondagens.csv` file in the same directory as your drawing

## Usage

1. Type `SONDM` in the AutoCAD command line
2. Enter the survey name when prompted
3. Click the insertion point in your drawing
4. The tool will automatically generate the survey marker

## CSV File Format

The CSV file should contain the following columns (semicolon-separated):
- Survey ID
- Surface elevation
- Maximum depth
- Projection
- Water level depth
- Additional fields as needed

Example:
```
ID Sondagem;Cota de boca;Profundidade máxima;Projeção;NA
SP-9093;745,87;54,45;1;8
SP-9094;716,25;60;2;7
```

## Version History

- **v1.4.0**: Current optimized version with improved performance
- Previous versions available in `/old` directory

## Author

André Buchmann Müller

## License

See LICENSE.md file for licensing information.
