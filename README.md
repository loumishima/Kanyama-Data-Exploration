# Predicted dates for Waste Collection in Kanyama

**Structure**:

1. [Problem](#prob)
2. [General Description](#gen)
3. [Usage](#use)
4. [User Interface](#UI)
5. [Next Steps](#next)
6. [Final Considerations](#final)

***

<a name="prob"></a>
## Problem / Situation 

Sanitation systems are a complex and enigmatic, even with more structured architectures like the ones on Europe or North America and organising the data from these are really complicated. Now imagine tracking data from regions with precaurious sewers, with little or inexistent sanitation mapping. That's why a project is needed in sanitation documenting on third world Countries, to link the people and companies with the government to make sanitation better and consequently health.

<a name="gen"></a>
## General Description

This project is an estimator for when septic tanks and pit latrines will be full by normal usage on Kanyama, one of the Lusaka city compounds, located on the country of Zambia.

The application makes the predictions based on the average daily usage, toilet structure and containment layout. With that it generates the possible dates of when the toilets are going to be full and with that information, providing the contact with the waste management team to operate on these areas, speeding up the process, creating a pattern of colletion and avoiding the spread of sanitation diseases.

That system can be used by the government to measure their structure quality and also by sanitation companies to plan their strategy on waste collection, building teams to work more effectively depending on the toilet area's structures. Also this project can be used by the people to verify their neighbourhood situation and ask the government for improvements on theirs toilet infrastructure.

<a name="use"></a>
## Usage

This application is avaiable at [Gatherhub platform](https://gatherhub.shinyapps.io/shiny/) and totally ready to operate, all the datasets are already included on the application, requiring no more effort from the user.

<a name="UI"></a>
## User Interface

The "Number of Occurences" tab shows, indicates how many toilets are going to be full on a daily basis, from the day the toilets were last emptied, until the day that they are supposed to be full, according to the calculation. The time-span are by default annual, with a day-by-day(Raw) and monthly (Smooth) aggregation. 

Monthly and Daily Predictions:

![Default panel](data/print3.png)

Enabling the month selection make the time-span flexible according to the user preference, but only the day-by-day analysis is available on this mode. 

General tab with Month selection activated:

![General Option](data/print4.png)

Accessing the "Transport" sub-tab shows the places that have the respective properties, note that the info was provided by the residents, which makes this attribute more subjective than expected:

1. Vacuum Tanker: Access Available by the Vacuum Tanker
2. Light Truck: Access unavailable by the Vacuum Tanker but available by the Light Truck
3. Push Cart: Access unavailable both by the Light Truck and the Vacuum Tanker

Transport tab with Month selection activated:

![General Option](data/print5.png)

Transport tab:

![Transport Option](data/print2.png)

The "Amount of Waste" tab works on a similar way, but instead counting toilets this section takes into consideration the containment size on each plot and indicates how many cubic meters has to be collected on each day into the future, making easier for the waste management team to organise its collection approach. The amount of days to the toilet filling can be calculated by:

![Formula](http://www.sciweavers.org/upload/Tex2Img_1563527795/render.png)

Where:

* Fill level: Actual amount of waste in the septic tank/ pit latrine
  * Where 0 is empty and 1 is totally full
* A: Waste compartiment area
* Np: Number of people using the toilet
* V: Average human fecal waste per day (0.000128 g)

This simple equation will return the number of days from the data collection until the time the toilet is totally full. This is a nice approximator for the sanitation companies to elaborate their waste collecting strategies and that final date can be adjusted when applied on real life.

The last attribute, the "Map" tab is responsible to transform the "Number of Occurence"/"Amount of Waste" tabs into a geospatial visualisation, indicating what are the points to be collected and where are the full toilets, The map is zoom sensitive making possible the plots update according to the amount of points on the map.

![Map Option](data/print1.png)

<a name="next"></a>
## Next Steps

This project was an introduction to the WSUP datasets, but to make the project more robust it's still necessary to:

* Add the monthly option on the Maps Tab
* Upgrade the formula to consider water table, weather forecast and flood occurences.
* Upgrade the UI for a more smooth application

<a name="final"></a>
## Final Considerations

We from Gather would like to thank [WSUP](https://www.wsup.com) for making the data available for the analysis and for the collaboration on this project.

The simplified version of the formula was created based on the BROUCKAERT, CJ; FOXON, KM  and  WOOD, K.  article and it is available [here](http://www.scielo.org.za/scielo.php?script=sci_arttext&pid=S1816-79502013000400013)

