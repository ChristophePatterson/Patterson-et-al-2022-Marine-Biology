# Patterson-et-al-2022-Marine-Biology
Code used for the publication of Patterson et al 2022 "The range expansion of Clibanarius erythropus to the UK suggests that other range-shifting intertidal species may not follow"" in Marine Biology by Christophe Patterson, Dr Chris Laing, and Dr Regan Early.

Due to the size of the data files produced by the programme Ichthyop v3.3, example data files are not desosited here and researchers looking to recreate or produce further anyalis should also see the [icthyopop manual](https://www.ichthyop.org/) for guidence around using Ichthyop v3.3.

All oceanographic data used in this study is are archived at [http://marc.ifremer.fr/en/](http://marc.ifremer.fr/en/). All parameters used within the programme Ichthypop v3.3 are the default unless otherwise stated within the text.

## Available Code
* To download Mars3D data use - [Downloading-Mars3D-data.R](https://github.com/ChristophePatterson/Patterson-et-al-2022-Marine-Biology/blob/main/Scripts/Downloading-Mars3D-data.R)
* For "simple" mass production of ichthyop configuration files over specific dates use - [cfg-files-for-vertical-migration.R](https://github.com/ChristophePatterson/Patterson-et-al-2022-Marine-Biology/blob/main/Scripts/cfg-files-for-vertical-migration.R)
* Once all configurations of files have been run in ichthyop and produce .nc files of larval dispersal the following code extracts the relevent information from each .nc file and stores that information as in a single csv file for down stream analysis. - [Extracting-key-recruiment-data.R](https://github.com/ChristophePatterson/Patterson-et-al-2022-Marine-Biology/blob/main/Scripts/Extracting-key-recruiment-data.R)
* All statistics and graphs produced in the paper can be created using - [Statistics-and-plots.R](https://github.com/ChristophePatterson/Patterson-et-al-2022-Marine-Biology/blob/main/Scripts/Statistics-and-plots.R)



