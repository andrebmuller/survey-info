# survey-info

An AutoLISP utility for automatically generating survey stake drawings in AutoCAD from CSV drilling data files.

## Overview

This tool converts geotechnical drilling data from CSV files into survey stake drawings in AutoCAD. It processes SPT (Standard Penetration Test) values and creates detailed technical drawings with information banners, water level indicators, and properly scaled geometric elements.

## Features

- **Automatic CSV Processing**: Reads single survey data from CSV files with validation
- **Interactive File Selection**: Choose CSV files through standard Windows dialog
- **Complete Survey Stakes**: Creates information banner, water level indicators, SPT markings, and scaled frame
- **Brazilian Format Support**: Handles decimal comma format and local standards

## Requirements

- AutoCAD 2018+ (recommended)
- AutoLISP support enabled
- CSV files with 8 columns (see CSV File Structure)

## Installation

1. Copy `survey-info.lsp` to your AutoCAD support directory
2. Load using APPLOAD or add to startup
3. Restart AutoCAD

## Usage

1. Run the command: `SONDMETRO`
2. Select CSV file when prompted
3. Click insertion point for the survey stake
4. Complete survey stake is generated automatically

## CSV File Structure

| Column | Field | Description | Example |
|--------|--------|-------------|---------|
| **A** | Survey ID | Survey identification code | `SP-9012` |
| **B** | Mouth Elevation | Initial elevation | `733,33` |
| **C** | Maximum Depth | Total depth in meters | `25,25` |
| **D** | Projection | Projection value | `1,25` |
| **E** | Water Level (N.A.) | Water level depth | `2,00` |
| **F** | Initial Depth | Interval start depth | `0,00` |
| **G** | Final Depth | Interval end depth | `1,00` |
| **H** | SPT Value | SPT value or "-" | `7,00` |

### Example CSV
```csv
ID Sondagem;Cota de boca;Profundidade máxima;Projeção;NA;Profundidade inicial;Profundidade final;SPT
SP-9012;733,33;25,25;1,25;2,00;0,00;1,00;-
;;;;;1,00;2,00;7,00
;;;;;2,00;3,00;9,00
```

## Layer Organization

| Layer Name | Color | Content |
|------------|-------|---------|
| `msp-ge_perfil` | **Green (3)** | Geometric elements, frame, lines |
| `msp-ge_textos` | **Yellow (2)** | Text annotations and labels |
| `msp-ge_NA` | **Red (1)** | Water level indicators |

## Error Handling

| Error | Solution |
|-------|----------|
| "Estrutura inválida" | Check semicolon separators |
| "Profundidade inválida" | Correct depth value in column C |
| "Arquivo não encontrado" | Verify CSV file location |
| Nothing appears | Execute `ZOOM EXTENTS` |

## Notes

- CSV files must use semicolon (;) as separator
- Decimal values use comma (,) format
- Scale factor: 2.5 units per meter depth
- Environment settings are automatically preserved

## Version History

- **v1.6.0** - Current version, single survey per CSV file
- **v1.0.0** - Initial version with basic functionality

## Author

[André Buchmann Müller](https://andrebmuller.notion.site/abm-eng)

## License

MIT License - See LICENSE.md for details
