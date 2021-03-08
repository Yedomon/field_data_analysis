# field_data_analysis


choice 1: Leveraging phenotypic variation in a worldwide panel from the Korean genebank to enhance yield, protein, lignan and oil oriented sesame breeding.

choice 2: Leveraging phenotypic variation in a worldwide panel from the Korean genebank to identify new resources for yield, protein, lignan and oil oriented sesame breeding.


choice 3: Leveraging phenotypic diversity in a worldwide panel from the Korean genebank to identify new resources for yield, protein, lignan and oil oriented sesame breeding.


- #### [Near-infrared reflectance spectroscopy reveals wide variation in major components of sesame seeds from Africa and Asia](https://www.sciencedirect.com/science/article/pii/S2214514117301149#!)


> To cross-validate the results of NIRS scanning, the 70 samples with sufficient seeds were subjected to chemical determination. Approximately 10 g of cleaned seed samples were manually ground to fine powder with liquid nitrogen. Seed oil was estimated by the conventional Soxhlet method using petroleum ether as extraction solvent [13]. Total protein content was determined by the Kjeldahl method according to AOAC [14]. Determination of oleic and linoleic acids was performed following Stefanoudaki et al. [15]. Each sample was tested in triplicate.



- #### [Broadening the genetic base of sesame (Sesamum indicum L.) through germplasm enhancement](https://www.cambridge.org/core/journals/plant-genetic-resources/article/broadening-the-genetic-base-of-sesame-sesamum-indicum-l-through-germplasm-enhancement/8F5A35415966AA813B3484EA291CCC40)


- #### [Diversity in Indian sesame collection and stratification of germplasm accessions in different diversity groups](https://link.springer.com/article/10.1023/A:1008652420477)


- #### [https://link.springer.com/article/10.1023/A:1008652420477](https://www.jstage.jst.go.jp/article/jsbbs/56/1/56_1_85/_pdf/-char/en)









- #### [Genetic parameter estimation and selection in advanced breeding population of white Guinea yam](https://www.tandfonline.com/doi/pdf/10.1080/15427528.2021.1881012)






- #### [Heterosis and combining abilities in a diverse seven-parent pearl millet population diallel tested in West Africa](https://assets.researchsquare.com/files/rs-232609/v1_stamped.pdf)




Raw data of each location were checked for outliers and tested for normality. The statistical analysis was performed following a one-step approach. Adjusted entry means were calculated based on the performance of the parents, controls, and hybrids using the following model across six environments:



   ![eq1](https://github.com/Yedomon/field_data_analysis/blob/main/equation1.PNG?raw=true)






where yijkl is the observed phenotype; µ represents the general mean; gi refers to the effect of the ith genotype; ej refers to the effect of the jth environment; (g×e)ij is the interaction effect between genotype i and environment j; rjk is the effect of the kth replication in the jth environment; bjkl is the effect of lth block nested in the kth replication of jth environment; and εijkl is the residual effect. For the estimation of entry means, the genotypic effects were considered fixed and all other effects were random. For the variance component estimation, all effects were regarded as random (Piepho et al. 2003). Broad-sense heritability (H2 ) for all 13 agronomic traits was calculated
across environments, using the following formula,

![eq2](https://github.com/Yedomon/field_data_analysis/blob/main/equation2.PNG?raw=true)



[Tutorial 1 (ASReml-R) - Estimating the heritability of birth weight](https://www.wildanimalmodels.org/tiki-download_wiki_attachment.php?attId=3)



[asremlPlus](https://github.com/briencj/asremlPlus)


[Vignette Labybird](https://github.com/briencj/asremlPlus/blob/master/vignettes/Ladybird.asreml.pdf)


A tutorial on the statistical analysis of factorial experiments with qualitative and quantitative treatment factor levels [Tuto](https://cran.r-project.org/web/packages/agriTutorial/vignettes/agriTutorialVignette.pdf) | [Paper](https://onlinelibrary.wiley.com/doi/full/10.1111/jac.12267)



[equivalent asmrel](https://stats.stackexchange.com/questions/18709/lme4-or-other-open-source-r-package-code-equivalent-to-asreml-r)

[sommer](https://www.rdocumentation.org/packages/sommer/versions/4.1.2/vignettes/v5.sommer.vs.lme4.Rmd)  | [tuto](https://rdrr.io/cran/sommer/) | sommer vs lme4](https://rdrr.io/cran/sommer/f/inst/doc/v5.sommer.vs.lme4.pdf)  | [paper](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0156744) | [usage](http://finzi.psych.upenn.edu/R/library/sommer/html/mmer2.html) heterescedasticity option| [poster](https://ausbiometric2019.org/posters/Sam_Rogers_IBS_poster.pdf)


> Other functions such as summary, fitted, randef (notice here is randef not ranef), anova, variogram, residuals, coef and plot applicable to typical linear models can also be applied to models fitted using the mmer2-type of functions.
