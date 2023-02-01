/*-----------------------------------------------RQ2_data--------------------------------------------------------------------*/
clear all
cd "C:\myfile\University of Utah\2019 Fall\fishery\July 16 2022"
set more off
use RQ2_data.dta
drop X
decode group, generate(species)
encode spe_country, generate(spe_node)
**set group (exporter_species) and time (year)
xtset spe_node year

**super = modularity + nodeDegree + local_clustering + avg_duration
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration composite_governance i.year, gmm(l.(super modularity nodeDegree local_clustering avg_duration composite_governance), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model1
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration composite_governance year, gmm(l.(super modularity nodeDegree local_clustering avg_duration composite_governance), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model2
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration i.year, gmm(l.(super modularity nodeDegree local_clustering avg_duration), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model3
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration year, gmm(l.(super modularity nodeDegree local_clustering avg_duration), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model4
esttab Model* using RQ2_WithDuration_SE.rtf, se keep(*super modularity nodeDegree local_clustering avg_duration composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace
esttab Model* using RQ2_WithDuration_P.rtf, p keep(*super modularity nodeDegree local_clustering avg_duration composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace


** super = modularity + nodeDegree + local_clustering
xtabond2 super l(1/2).super modularity nodeDegree local_clustering composite_governance i.year, gmm(l.(super modularity nodeDegree local_clustering composite_governance), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model1
xtabond2 super l(1/2).super modularity nodeDegree local_clustering composite_governance year, gmm(l.(super modularity nodeDegree local_clustering composite_governance), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model2
xtabond2 super l(1/2).super modularity nodeDegree local_clustering i.year, gmm(l.(super modularity nodeDegree local_clustering), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model3
xtabond2 super l(1/2).super modularity nodeDegree local_clustering year, gmm(l.(super modularity nodeDegree local_clustering), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model4
esttab Model* using RQ2_WithoutDuration_SE.rtf, se keep(*super modularity nodeDegree local_clustering composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace
esttab Model* using RQ2_WithoutDuration_P.rtf, p keep(*super modularity nodeDegree local_clustering composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace

/*-----------------------------------------------RQ2_data_03aqua--------------------------------------------------------------------*/
clear all
cd "C:\myfile\University of Utah\2019 Fall\fishery\July 16 2022"
set more off
use RQ2_data_03aqua.dta
drop X
decode group, generate(species)
encode spe_country, generate(spe_node)
**set group (exporter_species) and time (year)
xtset spe_node year

**super = modularity + nodeDegree + local_clustering + avg_duration
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration composite_governance i.year, gmm(l.(super modularity nodeDegree local_clustering avg_duration composite_governance), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model1
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration composite_governance year, gmm(l.(super modularity nodeDegree local_clustering avg_duration composite_governance), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model2
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration i.year, gmm(l.(super modularity nodeDegree local_clustering avg_duration), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model3
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration year, gmm(l.(super modularity nodeDegree local_clustering avg_duration), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model4
esttab Model* using RQ2_03aqua_WithDuration_SE.rtf, se keep(*super modularity nodeDegree local_clustering avg_duration composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace
esttab Model* using RQ2_03aqua_WithDuration_P.rtf, p keep(*super modularity nodeDegree local_clustering avg_duration composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace


** super = modularity + nodeDegree + local_clustering
xtabond2 super l(1/2).super modularity nodeDegree local_clustering composite_governance i.year, gmm(l.(super modularity nodeDegree local_clustering composite_governance), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model1
xtabond2 super l(1/2).super modularity nodeDegree local_clustering composite_governance year, gmm(l.(super modularity nodeDegree local_clustering composite_governance), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model2
xtabond2 super l(1/2).super modularity nodeDegree local_clustering i.year, gmm(l.(super modularity nodeDegree local_clustering), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model3
xtabond2 super l(1/2).super modularity nodeDegree local_clustering year, gmm(l.(super modularity nodeDegree local_clustering), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model4
esttab Model* using RQ2_03aqua_WithoutDuration_SE.rtf, se keep(*super modularity nodeDegree local_clustering composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace
esttab Model* using RQ2_03aqua_WithoutDuration_P.rtf, p keep(*super modularity nodeDegree local_clustering composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace


/*-----------------------------------------------RQ2_data_05aqua--------------------------------------------------------------------*/
clear all
cd "C:\myfile\University of Utah\2019 Fall\fishery\July 16 2022"
set more off
use RQ2_data_05aqua.dta
drop X
decode group, generate(species)
encode spe_country, generate(spe_node)
**set group (exporter_species) and time (year)
xtset spe_node year

**super = modularity + nodeDegree + local_clustering + avg_duration
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration composite_governance i.year, gmm(l.(super modularity nodeDegree local_clustering avg_duration composite_governance), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model1
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration composite_governance year, gmm(l.(super modularity nodeDegree local_clustering avg_duration composite_governance), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model2
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration i.year, gmm(l.(super modularity nodeDegree local_clustering avg_duration), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model3
xtabond2 super l(1/2).super modularity nodeDegree local_clustering avg_duration year, gmm(l.(super modularity nodeDegree local_clustering avg_duration), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model4
esttab Model* using RQ2_05aqua_WithDuration_SE.rtf, se keep(*super modularity nodeDegree local_clustering avg_duration composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace
esttab Model* using RQ2_05aqua_WithDuration_P.rtf, p keep(*super modularity nodeDegree local_clustering avg_duration composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace


** super = modularity + nodeDegree + local_clustering
xtabond2 super l(1/2).super modularity nodeDegree local_clustering composite_governance i.year, gmm(l.(super modularity nodeDegree local_clustering composite_governance), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model1
xtabond2 super l(1/2).super modularity nodeDegree local_clustering composite_governance year, gmm(l.(super modularity nodeDegree local_clustering composite_governance), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model2
xtabond2 super l(1/2).super modularity nodeDegree local_clustering i.year, gmm(l.(super modularity nodeDegree local_clustering), laglimits(1 4)) iv(i.year) small robust twostep noleveleq
eststo Model3
xtabond2 super l(1/2).super modularity nodeDegree local_clustering year, gmm(l.(super modularity nodeDegree local_clustering), laglimits(1 4)) iv(year) small robust twostep noleveleq
eststo Model4
esttab Model* using RQ2_05aqua_WithoutDuration_SE.rtf, se keep(*super modularity nodeDegree local_clustering composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace
esttab Model* using RQ2_05aqua_WithoutDuration_P.rtf, p keep(*super modularity nodeDegree local_clustering composite_governance year) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) replace
