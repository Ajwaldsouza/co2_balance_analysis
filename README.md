# CO2 Balance Analysis

\<a rel="license" href="[http://creativecommons.org/licenses/by/4.0/"](http://creativecommons.org/licenses/by/4.0/")\<img alt="Creative Commons License" style="border-width:0" src="[https://i.creativecommons.org/l/by/4.0/88x31.png"](https://i.creativecommons.org/l/by/4.0/88x31.png") /\>\</a\>\<br /\>This work is licensed under a \<a rel="license" href="[http://creativecommons.org/licenses/by/4.0/"](http://creativecommons.org/licenses/by/4.0/")Creative Commons Attribution 4.0 International License\</a\>.

Code and data for the project published as : *Manuscript title* in the journal *Journal name.*

The database `Data/` contains all the data and code used. In `Data/`, `Raw data/` contains the raw un-wrangled data obtained directly from the respirometric system used for decomposing biomass. `Temperature/` contains the substrate temperature of the bioreactor vessels in the respirometric system during decomposition. `CO2 data/` contains the CO2 mass data generated by processing the raw data using the script `data_import_wrangling.R`. The script `CO2 analysis.R` uses the data in `CO2 data/` to generate required outputs and conduct the analyses discussed in the paper. All the plots generated are exported to `Plots/`.
