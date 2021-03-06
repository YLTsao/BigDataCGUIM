NBA 2014-2015球季 各隊分析
================

install.packages("SportsAnalytics")
===================================

library(SportsAnalytics) NBA1415&lt;-fetch\_NBAPlayerStatistics("14-15")

1 最辛苦的球員
--------------

出戰分鐘數最多

MaxTime&lt;-aggregate(TotalMinutesPlayed~Team,NBA1415,max) tapply(NBA1415\(TotalMinutesPlayed,NBA1415\)Team,max) NBA1415MaxTime&lt;-merge(NBA1415,MaxTime) output&lt;-NBA1415MaxTime\[order(NBA1415MaxTime$TotalMinutesPlayed,decreasing = T),c("Team","Name","TotalMinutesPlayed")\] library(knitr) kable(output, digits=2)

|     | Team | Name             |  TotalMinutesPlayed|
|:----|:-----|:-----------------|-------------------:|
| 11  | HOU  | James Harden     |                2979|
| 18  | MIN  | Andrew Wiggins   |                2971|
| 25  | POR  | Damian Lillard   |                2928|
| 13  | LAC  | Chris Paul       |                2860|
| 30  | WAS  | John Wall        |                2841|
| 24  | PHO  | Eric Bledsoe     |                2799|
| 3   | BRO  | Joe Johnson      |                2787|
| 6   | CLE  | Kyrie Irving     |                2735|
| 7   | DAL  | Monta Ellis      |                2698|
| 19  | NOR  | Tyreke Evans     |                2695|
| 15  | MEM  | Marc Gasol       |                2690|
| 5   | CHI  | Pau Gasol        |                2682|
| 26  | SAC  | Ben Mclemore     |                2674|
| 8   | DEN  | Ty Lawson        |                2668|
| 16  | MIA  | Goran Dragic     |                2641|
| 29  | UTA  | Gordon Hayward   |                2618|
| 10  | GSW  | Stephen Curry    |                2613|
| 9   | DET  | Ke Caldwell-pope |                2591|
| 22  | ORL  | Victor Oladipo   |                2572|
| 17  | MIL  | G Antetokounmpo  |                2542|
| 2   | BOS  | Avery Bradley    |                2427|
| 28  | TOR  | Kyle Lowry       |                2422|
| 1   | ATL  | Kyle Korver      |                2418|
| 12  | IND  | Solomon Hill     |                2380|
| 4   | CHA  | Gerald Henderson |                2323|
| 23  | PHI  | Nerlens Noel     |                2311|
| 27  | SAN  | Danny Green      |                2311|
| 21  | OKL  | Russel Westbrook |                2302|
| 14  | LAL  | Wesley Johnson   |                2244|
| 20  | NYK  | Shane Larkin     |                1864|

2 各隊得分王
------------

計算依據為全季總得分最多的球員

MaxPoint&lt;-aggregate(TotalPoints~Team,NBA1415,max) \#tapply(NBA1415\(TotalPoints,NBA1415\)Team,max) NBA1415MaxPoint&lt;-merge(NBA1415,MaxPoint,by=c("Team","TotalPoints")) output&lt;-NBA1415MaxPoint\[order(NBA1415MaxPoint$TotalPoints,decreasing = T),c("Team","Name","TotalPoints")\] library(knitr) kable(output, digits=2)

|     | Team | Name             |  TotalPoints|
|:----|:-----|:-----------------|------------:|
| 11  | HOU  | James Harden     |         2217|
| 10  | GSW  | Stephen Curry    |         1900|
| 21  | OKL  | Russel Westbrook |         1886|
| 6   | CLE  | Lebron James     |         1740|
| 25  | POR  | Damian Lillard   |         1720|
| 19  | NOR  | Anthony Davis    |         1656|
| 13  | LAC  | Chris Paul       |         1564|
| 7   | DAL  | Monta Ellis      |         1513|
| 29  | UTA  | Gordon Hayward   |         1463|
| 5   | CHI  | Pau Gasol        |         1446|
| 26  | SAC  | Rudy Gay         |         1432|
| 22  | ORL  | Nikola Vucevic   |         1428|
| 15  | MEM  | Marc Gasol       |         1413|
| 18  | MIN  | Andrew Wiggins   |         1387|
| 30  | WAS  | John Wall        |         1385|
| 24  | PHO  | Eric Bledsoe     |         1377|
| 16  | MIA  | Dwyane Wade      |         1331|
| 28  | TOR  | Kyle Lowry       |         1244|
| 3   | BRO  | Brook Lopez      |         1236|
| 1   | ATL  | Paul Millsap     |         1218|
| 8   | DEN  | Ty Lawson        |         1143|
| 9   | DET  | Andre Drummond   |         1130|
| 2   | BOS  | Isaiah Thomas    |         1101|
| 4   | CHA  | Al Jefferson     |         1080|
| 27  | SAN  | Tim Duncan       |         1070|
| 17  | MIL  | Khris Middleton  |         1055|
| 20  | NYK  | Carmelo Anthony  |          966|
| 12  | IND  | C.j. Miles       |          942|
| 23  | PHI  | Robert Covington |          927|
| 14  | LAL  | Jordan Hill      |          841|

3 各隊最有效率的球員
--------------------

總得分/出戰分鐘數最高

NBA1415\(Effective<-  round(NBA1415\)TotalPoints/NBA1415\(TotalMinutesPlayed,digits=3) NBA1415EffectiveMax<-merge(NBA1415,aggregate(Effective~Team,NBA1415,max)) output<-NBA1415EffectiveMax[order(EffectiveMax\)Effective,decreasing=T), c("Team","Name","Effective")\] library(knitr) kable(output, digits=2)

|     | Team | Name             |  Effective|
|:----|:-----|:-----------------|----------:|
| 21  | OKL  | Russel Westbrook |       0.82|
| 11  | HOU  | James Harden     |       0.74|
| 10  | GSW  | Stephen Curry    |       0.73|
| 26  | SAC  | Demarcus Cousins |       0.71|
| 6   | CLE  | Lebron James     |       0.70|
| 20  | NYK  | Carmelo Anthony  |       0.68|
| 16  | MIA  | Dwyane Wade      |       0.67|
| 19  | NOR  | Anthony Davis    |       0.67|
| 15  | MEM  | Tyrus Thomas     |       0.67|
| 25  | POR  | Lamarcu Aldridge |       0.66|
| 14  | LAL  | Kobe Bryant      |       0.65|
| 2   | BOS  | Isaiah Thomas    |       0.64|
| 13  | LAC  | Blake Griffin    |       0.62|
| 28  | TOR  | Louis Williams   |       0.62|
| 24  | PHO  | Gerald Green     |       0.61|
| 18  | MIN  | Kevin Martin     |       0.60|
| 7   | DAL  | Charl Villanueva |       0.59|
| 5   | CHI  | Derrick Rose     |       0.59|
| 3   | BRO  | Brook Lopez      |       0.59|
| 12  | IND  | Paul George      |       0.58|
| 23  | PHI  | Tony Wroten      |       0.57|
| 22  | ORL  | Nikola Vucevic   |       0.56|
| 29  | UTA  | Gordon Hayward   |       0.56|
| 4   | CHA  | Jannero Pargo    |       0.55|
| 9   | DET  | Brandon Jennings |       0.54|
| 1   | ATL  | Jeff Teague      |       0.52|
| 27  | SAN  | Kawhi Leonard    |       0.52|
| 8   | DEN  | Danilo Gallinari |       0.52|
| 17  | MIL  | Ersan Ilyasova   |       0.51|
| 30  | WAS  | John Wall        |       0.49|

4 各隊三分球出手最準的球員
--------------------------

TreesMade/ThreesAttempted最高

NBA1415\(ThreesP<-  round(NBA1415\)ThreesMade/NBA1415\(ThreesAttempted,digits=3) NBA1415ThreesPMax<-merge(NBA1415,aggregate(ThreesP~Team,NBA1415,max)) output<-NBA1415ThreesPMax[order(ThreesPMax\)ThreesP,decreasing=T), c("Team","Name","Position","ThreesP","ThreesMade")\] library(knitr) kable(output, digits=2)

|     | Team | Name             | Position |  ThreesP|  ThreesMade|
|:----|:-----|:-----------------|:---------|--------:|-----------:|
| 4   | CHA  | Cody Zeller      | C        |     1.00|           1|
| 18  | MIL  | John Henson      | C        |     1.00|           1|
| 30  | TOR  | Bruno Caboclo    | SF       |     0.67|           2|
| 14  | LAL  | Dwight Buycks    | PG       |     0.64|           7|
| 27  | POR  | Victor Claver    | PF       |     0.55|           6|
| 20  | NOR  | Luke Babbitt     | SF       |     0.51|          59|
| 8   | DEN  | Jamaal Franklin  | SG       |     0.50|           1|
| 11  | HOU  | Dwight Howard    | C        |     0.50|           1|
| 13  | LAC  | Lester Hudson    | SG       |     0.50|           3|
| 25  | PHO  | Earl Barron      | PF       |     0.50|           2|
| 26  | PHO  | Jerel Mcneal     | SG       |     0.50|           1|
| 28  | SAC  | David Stockton   | PG       |     0.50|           1|
| 1   | ATL  | Kyle Korver      | SG       |     0.49|         221|
| 9   | DET  | Tayshaun Prince  | SF       |     0.46|          31|
| 5   | CHI  | Pau Gasol        | PF       |     0.46|          12|
| 2   | BOS  | Luigi Datome     | SF       |     0.45|          18|
| 10  | GSW  | Stephen Curry    | PG       |     0.44|         286|
| 22  | OKL  | Anthony Morrow   | SG       |     0.43|         141|
| 17  | MIA  | Shannon Brown    | PG       |     0.43|           3|
| 29  | SAN  | Tony Parker      | PG       |     0.43|          38|
| 7   | DAL  | Richar Jefferson | SF       |     0.43|          66|
| 6   | CLE  | Kyrie Irving     | PG       |     0.42|         155|
| 21  | NYK  | Jose Calderon    | PG       |     0.42|          59|
| 12  | IND  | Paul George      | SF       |     0.41|           9|
| 32  | WAS  | Bradley Beal     | SG       |     0.41|         106|
| 24  | PHI  | Hollis Thompson  | SG       |     0.40|         115|
| 15  | MEM  | Jordan Adams     | SG       |     0.40|          10|
| 16  | MEM  | Courtney Lee     | SG       |     0.40|          90|
| 31  | UTA  | Jeremy Evans     | SF       |     0.40|           2|
| 23  | ORL  | Channing Frye    | PF       |     0.39|         136|
| 19  | MIN  | Shabazz Muhammad | SF       |     0.39|          20|
| 3   | BRO  | Deron Williams   | PG       |     0.37|          87|
