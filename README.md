# HPP Heat Map

An interactive web application that visualizes ZIP code density across the continental United States at the city/municipal level.

## Features

- Upload and process CSV files containing ZIP code data
- Interactive map visualization with density heat mapping
- Real-time analytics and insights
- Export capabilities for data and visualizations
- Responsive design for desktop and tablet

## Prerequisites

- R version 4.1.0 or higher
- RStudio (recommended)

## Installation

1. Clone this repository:
```bash
git clone https://github.com/yourusername/zip_density_app.git
cd zip_density_app
```

2. Install required R packages:
```R
install.packages(c(
  "shiny",
  "shinydashboard",
  "bslib",
  "DT",
  "leaflet",
  "sf",
  "tigris",
  "maps",
  "dplyr",
  "readr",
  "tidyr",
  "stringr",
  "plotly",
  "ggplot2",
  "RColorBrewer",
  "viridis",
  "zipcodeR",
  "tidycensus"
))
```

3. Run the application:
```R
shiny::runApp()
```

## Usage

1. Launch the application
2. Click "Upload CSV" to select your ZIP code data file
3. The application will automatically detect ZIP code columns
4. View the interactive map and analytics panel
5. Use the export options to download processed data or map images

## CSV File Format

Your CSV file should contain at least one column with ZIP codes. The application will automatically detect columns containing ZIP codes. ZIP codes should be in 5-digit format (e.g., "12345").

Example CSV format:
```csv
id,zip_code,other_data
1,12345,data1
2,67890,data2
```

## Development

### Project Structure

```
zip_density_app/
├── app.R                    # Main Shiny application
├── global.R                 # Global variables and utilities
├── DESCRIPTION             # Package dependencies
└── README.md              # This file
```

### Adding New Features

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a new Pull Request

## Support

For support, please open an issue in the GitHub repository. 

## Screenshot
![image](https://github.com/user-attachments/assets/7fcc2526-0c52-4977-8fc9-faecf798c9c9)
