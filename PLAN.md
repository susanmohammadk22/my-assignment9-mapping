## Assignment 9: Mapping DFW Electric Vehicle Infrastructure & Adoption

### 1. Units and Boundaries

The unit of analysis is the county across the Dallas–Fort Worth metropolitan area. County boundaries will be obtained from the U.S. Census TIGER/Line geographic data using the `tigris` package. The geographic boundaries will be joined to the project datasets using standardized county names.

### 2. Variables

- **EV Adoption Rate:** A percentage representing EV registrations relative to total vehicle registrations. Mapped as a choropleth.
- **Median Household Income:** Measured in U.S. dollars. Mapped as a choropleth for comparison with EV adoption.
- **Charging Stations:** Total number of public EV charging stations per county. Mapped using proportional symbols.
- **Charging Ports:** Total number of public charging ports per county. A count variable suitable for proportional symbols.

### 3. Coordinate Reference Systems

- **Storage:** EPSG:4326 (WGS 84), preserving geographic coordinates in latitude and longitude.
- **Static Maps:** EPSG:5070 (NAD83 / Conus Albers), an equal-area projection appropriate for comparing county areas across the contiguous United States.
- **Interactive Map:** EPSG:4326, which is compatible with Leaflet's geographic input; Leaflet displays the map using Web Mercator.

### 4. Interaction

The interactive map will allow users to select the mapped variable (EV adoption rate, charging stations, or charging ports) and filter counties. A classification selector will allow users to compare quantile and continuous color scales. The map, summary value boxes, and companion chart will update together based on the selected inputs.