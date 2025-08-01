This repository contains the **data** and **R code** to visualize maps of interpolated community-means EIVs changes (light, temperature, soil moisture, soil nitrogen, and soil reaction) across Europe on a Shiny app.

#### App link: [https://gmidolo.shinyapps.io/interpolated_EIV_change_app](https://gmidolo.shinyapps.io/interpolated_EIV_change_app/)

![alt text](https://github.com/gmidolo/interpolated_EIV_change_app/blob/main/screenshot.app.png)

#### Description:

We interpolated spatiotemporal changes in community-mean ecological indicator values (sourced from the EIVE database; <a href='https://doi.org/10.3897/VCS.98324' target='_blank'>Dengler et al. 2023</a>) between 1960 and 2020 using Random Forests. Predictions are obtained over 606,818 European vegetation plots available on the <a href='https://euroveg.org/eva-database/' target='_blank'>European Vegetation Archive</a> (<a href='https://doi.org/10.1111/avsc.12519' target='_blank'>Chytrý et al. 2020</a>) and <a href='https://euroveg.org/resurvey/' target='_blank'>ReSurveyEurope</a> (<a href='https://doi.org/10.1111/jvs.13235' target='_blank'>Knollová et al. 2024</a>).

Predictions of &#916;CM<sub>EIV</sub> from individual plots are aggregated onto a 10 km &times; 10 km grid. On the map, point size represents the number of plots within each grid cell. Areas with denser sampling in space and time are more likely to show more accurate trends.

<strong>Article repository</strong>: 
                  <img src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png" width="15" style="vertical-align: middle; margin-right: 3px;"/> <a href="https://github.com/gmidolo/interpolated_EIV_change" target="_blank">github.com/gmidolo/interpolated_EIV_change</a>'


#### Author:
<strong>Gabriele Midolo</strong><a href="https://orcid.org/0000-0003-1316-2546" target="_blank" style="margin-left: 5px;"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" width="15" style="vertical-align: middle;"/></a> (<a href="mailto:midolo@fzp.czu.cz">midolo@fzp.czu.cz</a>) <br> Department of Spatial Sciences, Faculty of Environmental Sciences, Czech University of Life Sciences Prague, Kamýcká 129, 165 00, Praha - Suchdol, Czech Republic


#### Project contributors:<br>
Petr Keil <a href="https://orcid.org/0000-0003-3017-1858" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Adam Thomas Clark <a href="https://orcid.org/0000-0002-8843-3278" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Milan Chytrý <a href="https://orcid.org/0000-0002-8122-3075" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Franz Essl <a href="https://orcid.org/0000-0001-8253-2112" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Stefan Dullinger <a href="https://orcid.org/0000-0003-3919-0887" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Ute Jandt <a href="https://orcid.org/0000-0002-3177-3669" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Helge Bruelheide <a href="https://orcid.org/0000-0003-3135-0356" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Jürgen Dengler <a href="https://orcid.org/0000-0003-3221-660X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Irena Axmanová <a href="https://orcid.org/0000-0001-9440-7976" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Svetlana Aćić <a href="https://orcid.org/0000-0001-6553-3797" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Olivier Argagnon <a href="https://orcid.org/0000-0003-2069-7231" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Idoia Biurrun <a href="https://orcid.org/0000-0002-1454-0433" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Gianmaria Bonari <a href="https://orcid.org/0000-0002-5574-6067" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Alessandro Chiarucci <a href="https://orcid.org/0000-0003-1160-235X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Renata Ćušterevska <a href="https://orcid.org/0000-0002-3849-6983" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Pieter De Frenne <a href="https://orcid.org/0000-0002-8613-0943" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Michele De Sanctis <a href="https://orcid.org/0000-0002-7280-6199" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Jan Divíšek <a href="https://orcid.org/0000-0002-5127-5130" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Tetiana Dziuba <a href="https://orcid.org/0000-0001-8621-0890" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Rasmus Ejrnæs <a href="https://orcid.org/0000-0003-2538-8606" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Emmanuel Garbolino <a href="https://orcid.org/0000-0002-4954-6069" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Anke Jentsch <a href="https://orcid.org/0000-0002-2345-8300" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Borja Jiménez-Alfaro <a href="https://orcid.org/0000-0001-6601-9597" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Jonathan Lenoir <a href="https://orcid.org/0000-0003-0638-9582" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Jesper Erenskjold Moeslund <a href="https://orcid.org/0000-0001-8591-7149" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Francesca Napoleone <a href="https://orcid.org/0000-0002-3807-7180" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Sabine Rumpf <a href="https://orcid.org/0000-0001-5909-9568" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Jens-Christian Svenning <a href="https://orcid.org/0000-0002-3415-0862" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Grzegorz Swacha <a href="https://orcid.org/0000-0002-6380-2954" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Irina Tatarenko <a href="https://orcid.org/0000-0001-6835-2465" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Martin Večeřa <a href="https://orcid.org/0000-0001-8507-791X" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>,
Denys Vynokurov <a href="https://orcid.org/0000-0001-7003-6680" target="_blank"><img src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" class="is-rounded" width="15" style="vertical-align: middle;"/></a>

## License

**Data** are available under the terms of the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International license (CC BY-NC-ND 4.0) (<https://creativecommons.org/licenses/by-nc-nd/4.0/>).

**Code** are available under the terms of the GNU General Public License v3.0 (GPL-3.0) (<https://www.gnu.org/licenses/gpl-3.0.html>).

## Citation

*This repository is not linked to any publication yet.*