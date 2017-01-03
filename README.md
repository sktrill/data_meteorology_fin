# Data_Meteorology_Finance

<p><b>Background</b></p>
Every four years or so there seems to be a trend of news stories describing the impending gloom of El Nino / La Nina and the devastating impact this weather phenomenon will have on agriculture production, local weather and commodity pricing. Yet we survive. With El Nino weather predicted in 2016 I brace myself for another round of regurgitated new stories. El Nino and La Nina are the ying and yang in the El Nino Southern Oscillation cycle (commonly referred to as ENSO). These complex weather patterns, result from sea surface temperature (SST) variations in the equatorial Pacific. In North America El Nino months are often wetter and warmer, especially over the Gulf Coast, while on the other side of the Pacific, the Australian coast experiences drier and cooler weather. The converse is true in both regions during La Nina months. 

<p><b>Purpose</b></p>
This analysis looks to examine the relationship between ENSO-related (El Nino or La Nina) weather events in USA vs. Australia and its impact on commodity markets.

<p><b>Datasets</b></p>
The following datasets were used:
<ul>
<li>ENSO Oceanic Nino Index from USA's NOAA</li>
<li>ENSO Southern Oscillation Index from Australia's BOM</li>
<li>Mean monthly rain in Melbourne, Florida, USA, from the NOAA</li>
<li>Mean monthly maximum temperature in Melbourne, Florida, USA, from the NOAA</li>
<li>Mean monthly minimum temperature in Melbourne, Florida, USA, from the NOAA</li>
<li>Mean monthly rain in Melbourne, Victoria, Australia, from the BOM</li>
<li>Mean monthly maximum temperature in Melbourne, Victoria, Australia, from the BOM</li>
<li>Mean monthly minimum temperature in Melbourne, Victoria, Australia, from the BOM</li>
<li>Historical commodity prices for Copper, Nickel, Palm Oil, Zinc from the IMF via Quandl</li>
<li>Historical commodity prices for Corn, Rice, Soybean, Soybean Oil, Wheat from the CME via Quandl</li>
<li>Historical commodity prices for AgroIndex from the World Bank via Quandl</li>
<li>Historical commodity prices for Gold from the CME via Quandl</li>
</ul>

<p><b>Data Analysis / Understanding the relationship on weather and pricing</b></p>
<ul>
<li> Historical ENSO events for Strong El Nino, Strong La Nina and Very Strong El Nino events are clearly visible in the SOI and ONI dataset from BOM and NOAA respectively. There are also a number of moderate ENSO events</li>
<li> The inverse relationship between during ENSO events is shown in the relationship between SOI and OCI both of which are measures of mean oceanic surface temperature volatility. This hints at the fact that El Nino events cause warmer/wetter climate in USA while causing drier climate in Australia</li>
<li> In measuring rain distribution during warmer periods in Melbourne, Florida, we see a pattern where the median amount of rain during El Nino events is higher than during non-events. Similarly during La Nina events the median amount of rain is reduced. This relationship is exaggerated the warmer the climate gets</li> 
<li> In measuring rain distribution during warmer periods in Melbourne, Florida, we see a similar expected pattern where the median amount of rain during El Nino events is lower than during non-events. Similarly during La Nina events the median amount of rain is increased.</li> 
<li> There certainly appears to be, in the case of Corn, Wheat, Soybean, Soybean Oil, a negative impact on pricing as ENSO events affect agricultural supply (this relationship however will be statisically explored in more detail). For example reduced rainfall in Australia leads to reduced wheat production (depressing futures pricing for that commodity). Similarly the US is a major corn producer, just as soy beans are a major South American export. Both these crops produce diminished yields during periods of excess rainfall (during El Nino years), resulting in negative shock to the commodity spot price</li>
</ul>

<p><b>Discussion / On lies, damn lies and statistics</b></p>
There are a number of possible issues to be aware of: 
<ul>
<li> Small Sample Size - this is likely the biggest issue. Perhaps moderate ENSO events and severity of events could also be added to look at the relationship from a regression perspective (as opposed to classification)</li>
<li> Missing Factors - this is especially true in the case of commodity pricing. Perhaps a better proxy would have been to look at localized agricultural production . It would also be interesting to look at the impact on futures pricing </li>
</ul>

<p><b>Conclusion</b></p>
ENSO events do result in a statistically significant difference in climate. Specifically we see the warmer/wetter impact in Melbourne, USA versus the drier spells experienced in Melbourne, Australia during El Nino events. The converse is true for La Nina. As for commodity prices we can see evidence of price shocks during ENSO events, however this be studied statistically in future updates to this experiment. 

<p>Further Reading</p>
I'm far from informed about this issue (though interested). If you'd like to learn more here are a few things I came across:
<ul>
<li> What is ENSO: http://oceanservice.noaa.gov/facts/ninonina.html </li>
<li> NOAA ENSO impact on Southeast US: http://www.srh.noaa.gov/tlh/?n=enso </li>
<li> Macroeconomic Effects of El Nino, IMF, Faculty of Economics, Cambridge, 2015</li>
<li> Role of ENSO in Agricultural Commodity Markets, Y Lefkovitz, Stern, 2013</li>
<li> How Might El Nino Affect Crop Prices, AgWeb: http://www.agweb.com/article/how-might-el-nino-affect-crop-prices-naa-alison-rice/</li>
<li> Yearly El Nino news, WSJ in 2015: http://www.wsj.com/articles/commodities-prices-are-heating-up-on-el-nino-1444679542</li>
<li> Yearly El Nino news, Agrimoney in 2014: http://www.agrimoney.com/feature/the-commodity-price-which-gains-most-during-el-ninos-is...--277.html</li>
</ul>


<p>Data Sources</p>
<ul>
<li> NOAA ENSO Oceanic Nino Index since 1950: http://www.cpc.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml </li>
<li> BOM Southern Oscillation Index since 1950: http://www.bom.gov.au/climate/enso/#tabs=SOI</li>
<li> NOAA monthly mean rain, max/min temperature for Melbourne, USA: https://www.ncdc.noaa.gov/cdo-web/</li>
<li> BOM monthly mean rain, max/min temperature for Melbourne, Australia: http://www.bom.gov.au/climate/data/</li>
<li> Commodity historical pricing: https://www.quandl.com/collections/markets/commodities </li>
</ul>

@thekotecha
